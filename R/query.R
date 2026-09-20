#' @include src.R
NULL

#' Narrow a source
#'
#' `query()` adds to a source's plan and reads nothing. It composes, so a
#' pipeline of `query()` calls is the same as one call with all the arguments,
#' and the result is still a source you can print, query again, or hand to a
#' terminal verb.
#'
#' Some arguments mean the same thing for both kinds of source and some do
#' not, which is deliberate. `extent` is the same operation on both: restrict
#' to this rectangle. `bands` and `fields` are the same operation on different
#' nouns. But resolution has no vector meaning and attribute filtering has no
#' raster meaning, so `dim` and `resample` are raster-only and `where` and
#' `limit` are vector-only. Passing one to the wrong kind is an error rather
#' than a silent no-op, because a spatial query that quietly ignored half of
#' what you asked for is worse than one that stops.
#'
#' `crs` is the one argument that means something different for each kind, and
#' the difference is the point rather than an inconsistency. For both, an
#' `extent` given beside it is read in that CRS and transformed onto the
#' source to pick what to read, which is four numbers moving and no data
#' touched. For a vector it additionally sets the CRS the features come back
#' in, because transforming coordinates is cheap, local and has no resampling
#' decision in it. For a raster it does not, because reprojecting a grid
#' resamples: that is [warp()], a separate verb so the method stays a choice
#' you make rather than one you inherit.
#'
#' A raster `extent` snaps outward to whole source pixels, so a query with no
#' `dim` reads the source's own values rather than a resampling of them, and
#' the extent the result carries is the one its pixels really have.
#'
#' @param x A source from [src()].
#' @param ... The named arguments below.
#'
#' @section Arguments:
#'
#' All of these are passed by name.
#'
#' \describe{
#'   \item{`extent`}{`c(xmin, xmax, ymin, ymax)`, the rectangle to restrict
#'     to.}
#'   \item{`crs`}{Anything GDAL and PROJ understand. An `extent` beside it
#'     is read in this CRS. For a vector source it is also the CRS the
#'     features come back in; for a raster it is not, and [warp()] is.}
#'   \item{`bands`}{Raster only. Which bands, one-based, or by band
#'     description.}
#'   \item{`dim`}{Raster only. `c(nx, ny)`, the size to read the window at.
#'     This is what makes a read cheap: asking for a small `dim` over a large
#'     extent lets GDAL serve it from an overview.}
#'   \item{`resample`}{Raster only. One of `"nearest"` (the default),
#'     `"bilinear"`, `"cubic"`, `"cubicspline"`, `"lanczos"`, `"average"`,
#'     `"mode"`, `"gauss"` or `"rms"`.}
#'   \item{`where`}{Vector only. An SQL `WHERE` clause. GDAL does not
#'     validate it when it is set, so a bad clause shows up as a warning at
#'     read time rather than an error here.}
#'   \item{`fields`}{Vector only. Which attribute columns to read, by name.}
#'   \item{`limit`}{Vector only. Stop after this many features.}
#' }
#'
#' @return `x` with the plan narrowed.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' query(x, extent = c(-100, 0, 0, 80), dim = c(8, 8), resample = "average")
#'
#' v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
#' query(v, where = "population > 1e6", fields = "name")
query <- S7::new_generic("query", "x", function(x, ...) {
  S7::S7_dispatch()
})

S7::method(query, raster_source) <- function(x, ..., extent = NULL,
                                             crs = NULL, bands = NULL,
                                             dim = NULL, resample = NULL) {
  reject_dots(..., kind = "raster")
  plan <- S7::prop(x, "plan")

  if (!is.null(extent)) {
    extent <- check_extent(extent)
    if (!is.null(crs)) {
      extent <- transform_query_extent(extent, crs, S7::prop(x, "dataset")@crs)
    }
    narrowed <- intersect_extents(plan$extent, extent)
    if (is.null(narrowed)) {
      stop("that extent does not overlap the source, whose extent is ",
           format_extent(plan$source_extent), call. = FALSE)
    }
    cropped <- vaster::vcrop(
      unname(narrowed),
      dimension = plan$source_dimension,
      extent = unname(plan$source_extent),
      snap = "out"
    )
    plan$extent <- stats::setNames(cropped$extent,
                                   c("xmin", "xmax", "ymin", "ymax"))
    plan$dimension <- cropped$dimension
  } else if (!is.null(crs)) {
    stop("`crs` says what `extent` is given in, so it needs an `extent`.\n",
         "  To reproject the raster itself, use warp().", call. = FALSE)
  }

  if (!is.null(bands)) {
    plan$bands <- resolve_bands(x, bands)
  }

  if (!is.null(dim)) {
    dim <- as.integer(dim)
    if (length(dim) != 2L || anyNA(dim) || any(dim < 1L)) {
      stop("`dim` must be two positive whole numbers: nx, ny", call. = FALSE)
    }
    plan$out_dimension <- dim
  }

  if (!is.null(resample)) {
    plan$resample <- check_resample(resample)
  }

  S7::prop(x, "plan") <- plan
  x
}

S7::method(query, vector_source) <- function(x, ..., extent = NULL,
                                             crs = NULL, where = NULL,
                                             fields = NULL, limit = NULL) {
  reject_dots(..., kind = "vector")
  plan <- S7::prop(x, "plan")

  if (!is.null(extent)) {
    extent <- check_extent(extent)
    if (!is.null(crs)) {
      extent <- transform_query_extent(extent, crs, S7::prop(x, "info")$crs)
    }
    plan$extent <- if (is.null(plan$extent)) {
      extent
    } else {
      intersect_extents(plan$extent, extent) %||%
        stop("that extent does not overlap the extent already queried, ",
             format_extent(plan$extent), call. = FALSE)
    }
  }

  if (!is.null(crs)) {
    if (!is.character(crs) || length(crs) != 1L || is.na(crs)) {
      stop("`crs` must be a single, non-missing string", call. = FALSE)
    }
    if (!has_crs(S7::prop(x, "info")$crs)) {
      stop("the layer declares no CRS, so there is nothing to reproject from ",
           "(see report(x))", call. = FALSE)
    }
    plan$crs <- crs
  }

  if (!is.null(where)) {
    if (!is.character(where) || length(where) != 1L || is.na(where)) {
      stop("`where` must be a single, non-missing string", call. = FALSE)
    }
    # Stacking two clauses with AND is the only composition that is always
    # right; ORing them would widen a query that was meant to narrow.
    plan$where <- if (is.null(plan$where)) {
      where
    } else {
      paste0("(", plan$where, ") AND (", where, ")")
    }
  }

  if (!is.null(fields)) {
    if (!is.character(fields) || anyNA(fields)) {
      stop("`fields` must be a character vector of column names", call. = FALSE)
    }
    plan$fields <- if (is.null(plan$fields)) {
      fields
    } else {
      intersect(plan$fields, fields)
    }
  }

  if (!is.null(limit)) {
    limit <- as.integer(limit)
    if (length(limit) != 1L || is.na(limit) || limit < 0L) {
      stop("`limit` must be a single whole number, zero or more", call. = FALSE)
    }
    plan$limit <- min(c(plan$limit, limit))
  }

  S7::prop(x, "plan") <- plan
  x
}

# An argument that belongs to the other kind gets a message saying which kind
# it belongs to and what the analogue is, rather than "unused argument".
reject_dots <- function(..., kind) {
  dots <- list(...)
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }
  named <- names(dots) %||% rep("", length(dots))
  other <- if (kind == "raster") {
    c(where = "attribute filtering has no raster meaning",
      fields = "the raster analogue is `bands`",
      limit = "a raster read is random access, not a stream")
  } else {
    c(dim = paste0("resolution has no vector meaning; the analogue is ",
                   "generalisation, which is a different operation with ",
                   "different failure modes"),
      resample = "there is nothing to resample in a feature",
      bands = "the vector analogue is `fields`")
  }
  hit <- intersect(named, names(other))
  if (length(hit) > 0L) {
    stop("`", hit[1L], "` is not a ", kind, " argument: ", other[[hit[1L]]],
         call. = FALSE)
  }
  stop("unused argument", if (length(dots) > 1L) "s" else "", ": ",
       paste(named[nzchar(named)], collapse = ", "), call. = FALSE)
}

transform_query_extent <- function(extent, from, to) {
  if (!has_crs(to)) {
    stop("the source declares no CRS, so an extent given in another one ",
         "cannot be transformed onto it (see report(x))", call. = FALSE)
  }
  bbox_to_extent(GDAL7::transform_extent(extent_to_bbox(extent), from, to))
}

resolve_bands <- function(x, bands) {
  ds <- S7::prop(x, "dataset")
  n <- ds@raster_count
  if (is.character(bands)) {
    have <- vapply(
      seq_len(n),
      function(i) GDAL7::get_raster_band(ds, i)@description,
      character(1)
    )
    idx <- match(bands, have)
    if (anyNA(idx)) {
      stop("no band is described as '", bands[is.na(idx)][1L], "'.\n",
           "  The descriptions are: ",
           paste(sprintf("'%s'", have), collapse = ", "), call. = FALSE)
    }
    return(idx)
  }
  bands <- as.integer(bands)
  if (anyNA(bands) || any(bands < 1L) || any(bands > n)) {
    stop("`bands` must be between 1 and ", n, ", or band descriptions",
         call. = FALSE)
  }
  bands
}

resample_methods <- c("nearest", "bilinear", "cubic", "cubicspline",
                      "lanczos", "average", "mode", "gauss", "rms")

check_resample <- function(resample) {
  if (!is.character(resample) || length(resample) != 1L || is.na(resample) ||
      !resample %in% resample_methods) {
    stop("`resample` is one of ",
         paste(sprintf('"%s"', resample_methods), collapse = ", "),
         call. = FALSE)
  }
  resample
}
