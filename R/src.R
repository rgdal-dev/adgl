#' A lazy spatial source
#'
#' `src()` opens a GDAL data source and returns a plan over it, never data.
#' The source is probed once at open, so the object knows its own shape and
#' what its metadata is missing, and nothing is read until [collect()],
#' [write_to()] or `plot()` asks for a result.
#'
#' `options` is the non-virtualisation way to fix a deficient source, which is
#' why it is here and why [report()] points at it: a CSV's
#' `"X_POSSIBLE_NAMES=lon"`, a raster's `"OVERVIEW_LEVEL=2"`. It is handed
#' straight to GDAL. `drivers` restricts which drivers may try the file, so a
#' second driver cannot claim one the first should have had.
#'
#' @param dsn A GDAL connection string: a path, or a `/vsicurl/...`,
#'   `/vsis3/...`, `GPKG:...` or any other form GDAL understands.
#' @param options Character vector of `"KEY=VALUE"` open options, or `NULL`
#'   for none.
#' @param drivers Character vector of driver short names that may open the
#'   source, or `NULL` to place no restriction.
#' @param layer For a vector source, the layer to plan over: a name, or a
#'   one-based position. Ignored for a raster.
#'
#' @return A `raster_source` or a `vector_source`.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' x
#'
#' v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
#' v
src <- function(dsn, options = NULL, drivers = NULL, layer = 1L) {
  if (!is.character(dsn) || length(dsn) != 1L || is.na(dsn)) {
    stop("`dsn` must be a single, non-missing string", call. = FALSE)
  }

  ds <- GDAL7::gdal_open(dsn, options = options, drivers = drivers)

  if (ds@raster_count > 0L) {
    new_raster_source(dsn, ds)
  } else if (ds@layer_count > 0L) {
    new_vector_source(dsn, ds, layer)
  } else {
    GDAL7::gdal_close(ds)
    stop("'", dsn, "' opened, but holds no raster bands and no vector layers.\n",
         "  A multidimensional source is one way this happens; read it with ",
         "GDAL7::read_mdarray().", call. = FALSE)
  }
}

# The shared shape. The plan is a plain list and holds no GDAL object, so it
# prints, compares and serialises; that is what lets query() be pure and lets
# write_to() compile the same plan a second way.
spatial_source <- S7::new_class(
  "spatial_source",
  abstract = TRUE,
  properties = list(
    dsn = S7::class_character,
    dataset = S7::class_any,
    info = S7::class_list,
    plan = S7::class_list,
    findings = S7::class_any
  )
)

#' The source classes
#'
#' The two classes [src()] returns. They are exported so that another package
#' can write methods for them; build one with [src()] rather than by calling
#' these directly.
#'
#' @param dsn The GDAL connection string the source was opened from.
#' @param dataset The open `GDAL7::GDALDataset`.
#' @param info What the one probe at open returned: `GDAL7::gdal_info()` for a
#'   raster, the layer's own description for a vector.
#' @param plan The pending query, as a plain list. Nothing in it is a GDAL
#'   object, which is what lets it print, compare and serialise.
#' @param findings The deficiency report, computed once at open. Read it with
#'   [report()].
#'
#' @return A source object.
#' @export
#' @examples
#' S7::S7_inherits(src(system.file("extdata/test.tif", package = "GDAL7")),
#'                 raster_source)
raster_source <- S7::new_class("raster_source", parent = spatial_source)

#' @rdname raster_source
#' @export
vector_source <- S7::new_class("vector_source", parent = spatial_source)

new_raster_source <- function(dsn, ds) {
  info <- GDAL7::gdal_info(ds)
  dimension <- c(ds@raster_xsize, ds@raster_ysize)
  gt <- info$geotransform

  # A source with no geotransform at all still has a shape, and the report is
  # where that is said. The plan carries the index-space extent so the object
  # is still printable and still queryable by band.
  extent <- if (is.null(gt)) {
    stats::setNames(c(0, dimension[1L], 0, dimension[2L]),
                    c("xmin", "xmax", "ymin", "ymax"))
  } else {
    gt_extent(gt, dimension)
  }

  x <- raster_source(
    dsn = dsn,
    dataset = ds,
    info = info,
    plan = list(
      kind = "raster",
      source_extent = extent,
      source_dimension = dimension,
      extent = extent,
      dimension = dimension,
      out_dimension = NULL,
      bands = seq_len(ds@raster_count),
      resample = "nearest",
      warp = NULL
    ),
    findings = NULL
  )
  S7::prop(x, "findings") <- raster_findings(x)
  x
}

new_vector_source <- function(dsn, ds, layer) {
  lyr <- GDAL7::get_layer(ds, layer)
  extent <- tryCatch(bbox_to_extent(GDAL7::get_extent(lyr)),
                     error = function(e) NULL)

  x <- vector_source(
    dsn = dsn,
    dataset = ds,
    info = list(
      layer = layer,
      name = lyr@name,
      geometry_type = lyr@geometry_type,
      crs = lyr@crs,
      source_extent = extent,
      feature_count = GDAL7::feature_count(lyr, force = FALSE),
      fast_spatial_filter = isTRUE(GDAL7::test_capability(lyr, "FastSpatialFilter"))
    ),
    plan = list(
      kind = "vector",
      layer = layer,
      extent = NULL,
      where = NULL,
      fields = NULL,
      limit = NULL,
      crs = NULL
    ),
    findings = NULL
  )
  S7::prop(x, "findings") <- vector_findings(x)
  x
}

#' Close a source
#'
#' Releases the GDAL dataset the source holds open. A source that has been
#' closed cannot be read from again. Sources are closed for you when they are
#' garbage collected, so this matters only when a file has to be released at a
#' known moment: overwriting it, deleting it, or opening it again for update.
#'
#' @param x A source from [src()].
#' @return `x`, invisibly.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' src_close(x)
src_close <- function(x) {
  stopifnot(S7::S7_inherits(x, spatial_source))
  GDAL7::gdal_close(S7::prop(x, "dataset"))
  invisible(x)
}

S7::method(print, raster_source) <- function(x, ...) {
  plan <- S7::prop(x, "plan")
  info <- S7::prop(x, "info")
  d <- plan$dimension
  out <- plan$out_dimension %||% d

  cat("raster source ", info$driver, "  ",
      d[1L], " x ", d[2L], " x ", length(plan$bands), "\n", sep = "")
  cat("  extent ", format_extent(plan$extent), "\n", sep = "")
  cat("  crs    ", crs_label(S7::prop(x, "dataset")@crs), "\n", sep = "")
  if (!identical(out, d)) {
    cat("  reads  ", out[1L], " x ", out[2L], " by ", plan$resample, "\n", sep = "")
  }
  if (!is.null(plan$warp)) {
    cat("  warp   to ", crs_label(plan$warp$crs), " by ", plan$warp$resample,
        "\n", sep = "")
  }
  print_findings(S7::prop(x, "findings"))
  invisible(x)
}

S7::method(print, vector_source) <- function(x, ...) {
  plan <- S7::prop(x, "plan")
  info <- S7::prop(x, "info")

  n <- info$feature_count
  n_label <- if (is.na(n) || n < 0) "? features" else paste(n, "features")

  cat("vector source ", info$name, "  ", n_label, "  ",
      info$geometry_type, "\n", sep = "")
  if (!is.null(info$source_extent)) {
    cat("  extent ", format_extent(info$source_extent), "\n", sep = "")
  }
  cat("  crs    ", crs_label(info$crs), "\n", sep = "")
  if (!is.null(plan$where)) {
    cat("  where  ", plan$where, " (not checked until read)\n", sep = "")
  }
  if (!is.null(plan$extent)) {
    cat("  query  ", format_extent(plan$extent), "\n", sep = "")
  }
  if (!is.null(plan$fields)) {
    cat("  fields ", paste(plan$fields, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(plan$crs)) {
    cat("  reads  in ", crs_label(plan$crs), "\n", sep = "")
  }
  print_findings(S7::prop(x, "findings"))
  invisible(x)
}

format_extent <- function(extent) {
  if (is.null(extent)) return("unknown")
  paste(format(unname(extent), digits = 7, trim = TRUE), collapse = ", ")
}

crs_label <- function(crs) {
  if (!has_crs(crs)) {
    return("none")
  }
  # Something the user typed as an authority code is already the label.
  if (grepl("^[A-Za-z]+:[0-9]+$", crs)) {
    return(crs)
  }
  # Otherwise dig the authority out of the WKT. The last ID is the outermost
  # one: a projected CRS names its own base geographic CRS first, so taking
  # the first match would label EPSG:3031 as EPSG:4326.
  ids <- regmatches(crs, gregexpr('ID\\["EPSG",[0-9]+\\]', crs))[[1L]]
  if (length(ids) > 0L) {
    return(paste0("EPSG:", gsub("[^0-9]", "", ids[length(ids)])))
  }
  name <- regmatches(crs, regexpr('^[A-Z]+\\["[^"]+"', crs))
  if (length(name) == 1L && nzchar(name)) {
    return(sub('^[A-Z]+\\["', "", sub('"$', "", name)))
  }
  "set"
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# GDAL reports a missing CRS as NULL from one call, as "" from another and as
# NA from a third, and nzchar(NA) is TRUE, so asking with nzchar() alone
# silently decides that a georeference-less file has a CRS.
has_crs <- function(crs) {
  !is.null(crs) && length(crs) == 1L && !is.na(crs) && nzchar(crs)
}

crs_or_null <- function(crs) {
  if (has_crs(crs)) crs else NULL
}
