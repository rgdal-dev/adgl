#' @include src.R
NULL

#' Reproject a raster source
#'
#' `warp()` is the raster verb for changing coordinate reference system. It is
#' separate from [query()] because reprojecting a grid is a different act from
#' subsetting one: it resamples, and which resampling to use is a choice you
#' should make rather than inherit from a default. Ask for `"nearest"` on a
#' class raster and `"bilinear"` or `"average"` on a continuous one.
#'
#' This is the one place `adgl` virtualises a source, and it does so without
#' apology: GDAL implements a warp as a warped VRT or a warped read and there
#' is no other way to reproject a grid. The rule the package holds is the
#' narrower one it was always about, that it will never quietly augment a
#' source to supply metadata the source is missing. It will reproject when you
#' ask it to, and it will still never invent a geotransform for a file that
#' has none.
#'
#' The target extent is computed with [GDAL7::transform_extent()], which
#' unions GDAL's boundary walk with an interior mesh, rather than left to the
#' boundary walk alone. That matters where the target projection has an
#' interior singularity: the walk alone can come back hundreds of kilometres
#' too narrow around the antipode of an azimuthal projection, and an extent
#' that is too small clips data silently.
#'
#' When nothing pins the target window, the extent GDAL chooses is measured
#' once the warp has run, by inverting its corners and comparing the distance
#' it claims against the distance it covers on the ground. A whole-globe
#' source into a polar projection comes back with an extent 8e23 m across
#' whose four corners are all the same pole, which is a picture of a
#' singularity rather than of the data. That is a warning with `dim` and an
#' error with `resolution`, where GDAL cannot build the grid at all. Where
#' GDAL clamps for itself, as it does for Web Mercator's latitude limit,
#' there is nothing to say and nothing is said.
#'
#' Nothing is read here. The reprojection is recorded in the plan and happens
#' when a terminal verb asks for the result, so `warp()` composes with
#' [query()] in either order.
#'
#' @param x A raster source from [src()].
#' @param crs The target CRS: anything GDAL understands, such as
#'   `"EPSG:3031"`, a PROJ string, or WKT.
#' @param ... The named arguments below.
#'
#' Three arguments shape the target grid and any one of them is enough.
#' `dim` gives it a size, `resolution` gives it a pixel size, and `extent`
#' gives it a window; leave all three out and GDAL chooses a grid that roughly
#' preserves the source resolution.
#'
#' `dim` takes a zero in either position, which is the useful part: `c(1024,
#' 0)` asks for a grid 1024 pixels wide and lets GDAL work out the height from
#' the target extent's own aspect ratio, which you would otherwise have to
#' compute in a projection you have not seen yet. `c(0, 1024)` is the same the
#' other way round.
#'
#' `extent` is the one argument here that is given in the *target* CRS, since
#' it is the window the output covers. That makes it an alternative to
#' [query()]`(extent = )` rather than a companion to it, and asking for both
#' is an error rather than a silent choice between them.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{`resample`}{One of `"nearest"` (the default), `"bilinear"`,
#'     `"cubic"`, `"cubicspline"`, `"lanczos"`, `"average"`, `"mode"`,
#'     `"rms"`, `"sum"`, `"min"`, `"max"`, `"med"`, `"q1"` or `"q3"`.}
#'   \item{`dim`}{`c(nx, ny)`, the size of the warped grid, with a zero in
#'     either position meaning "derive this one from the other".}
#'   \item{`resolution`}{The target pixel size in the units of the target
#'     CRS: one number for square pixels, or `c(xres, yres)`. Cannot be given
#'     beside `dim`.}
#'   \item{`extent`}{`c(xmin, xmax, ymin, ymax)` in the target CRS, the
#'     window the output covers.}
#' }
#'
#' @return `x` with a reprojection added to its plan.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' w <- warp(query(x, extent = c(-170, 170, -80, 80)), "EPSG:3857",
#'           resample = "bilinear")
#' w
#'
#' # 400 pixels wide, and however tall that turns out to be.
#' warp(x, "EPSG:3857", dim = c(400, 0))
warp <- S7::new_generic("warp", "x", function(x, crs, ...) {
  S7::S7_dispatch()
})

S7::method(warp, raster_source) <- function(x, crs, ..., resample = "nearest",
                                            dim = NULL, resolution = NULL,
                                            extent = NULL) {
  rlang_check_empty(...)
  if (!is.character(crs) || length(crs) != 1L || is.na(crs)) {
    stop("`crs` must be a single, non-missing string", call. = FALSE)
  }
  refuse_on_blocking(x, "warp")

  plan <- S7::prop(x, "plan")

  if (!is.null(dim) && !is.null(resolution)) {
    stop("give `dim` or `resolution`, not both: over a target extent each ",
         "one already fixes the other", call. = FALSE)
  }
  if (!is.null(resolution) && !is.null(plan$out_dimension)) {
    stop("`resolution` cannot apply here, because query(dim = ) has already ",
         "fixed the size of this read.\n  Give one or the other.",
         call. = FALSE)
  }
  if (!is.null(extent) && !whole_source(plan)) {
    stop("`extent` here is the window the output covers, in the CRS being ",
         "warped to, so it replaces the rectangle query(extent = ) set ",
         "rather than narrowing it.\n  Give one or the other.", call. = FALSE)
  }

  plan$warp <- list(
    crs = crs,
    resample = check_warp_resample(resample),
    dim = check_warp_dim(dim %||% plan$out_dimension),
    resolution = check_warp_resolution(resolution),
    extent = if (is.null(extent)) NULL else check_extent(extent, "extent")
  )
  S7::prop(x, "plan") <- plan
  x
}

S7::method(warp, vector_source) <- function(x, crs, ...) {
  stop("warp() is a raster verb. Reprojecting features resamples nothing, ",
       "so it belongs in query(crs = ).", call. = FALSE)
}

# GDAL's warper takes a different set of methods from RasterIO: it has "sum"
# and the order statistics, and it has no "gauss".
warp_resample_methods <- c("nearest", "bilinear", "cubic", "cubicspline",
                           "lanczos", "average", "mode", "rms", "sum",
                           "min", "max", "med", "q1", "q3")

# GDAL's own --size takes a zero in one position and derives it from the other
# and the target extent, which is the whole reason to want it: you know how
# wide the output should be, and working out how tall that makes it means
# computing an extent in a projection you have not looked at yet.
check_warp_dim <- function(dim) {
  if (is.null(dim)) {
    return(NULL)
  }
  dim <- as.integer(dim)
  if (length(dim) != 2L || anyNA(dim) || any(dim < 0L)) {
    stop("`dim` must be two whole numbers, nx and ny, neither negative.\n",
         "  One of them may be 0, which asks GDAL to derive it from the ",
         "other and the target extent.", call. = FALSE)
  }
  if (all(dim == 0L)) {
    stop("`dim` cannot be 0 in both positions; `dim = NULL` is how you ask ",
         "GDAL to choose the whole size.", call. = FALSE)
  }
  dim
}

check_warp_resolution <- function(resolution) {
  if (is.null(resolution)) {
    return(NULL)
  }
  resolution <- as.double(unname(resolution))
  if (length(resolution) == 1L) {
    resolution <- c(resolution, resolution)
  }
  if (length(resolution) != 2L || anyNA(resolution) || any(resolution <= 0)) {
    stop("`resolution` is one positive number for square pixels, or two, ",
         "xres and yres, in the units of the target CRS", call. = FALSE)
  }
  resolution
}

# Whether the plan still covers the whole source, which is what decides
# whether GDAL may choose the target extent for itself.
whole_source <- function(plan) {
  isTRUE(all.equal(unname(plan$extent), unname(plan$source_extent)))
}

check_warp_resample <- function(resample) {
  if (!is.character(resample) || length(resample) != 1L || is.na(resample) ||
      !resample %in% warp_resample_methods) {
    stop("`resample` is one of ",
         paste(sprintf('"%s"', warp_resample_methods), collapse = ", "),
         call. = FALSE)
  }
  resample
}

# The plan's warp compiled into arguments for GDAL's "raster reproject"
# algorithm. `output` and `output-format` are left to the caller, because the
# same arguments serve a read into memory and a write to a file.
warp_args <- function(x) {
  plan <- S7::prop(x, "plan")
  ds <- S7::prop(x, "dataset")
  warp <- plan$warp

  from <- ds@crs
  if (!has_crs(from)) {
    stop("the source declares no CRS, so there is nothing to reproject from ",
         "(see report(x))", call. = FALSE)
  }

  args <- list(
    input = ds,
    "dst-crs" = warp$crs,
    resampling = warp$resample
  )

  # An extent given to warp() is already in the target CRS, so it goes through
  # untransformed and is the window the output covers.
  if (!is.null(warp$extent)) {
    args$bbox <- as.double(unname(extent_to_bbox(warp$extent)))
  } else if (!whole_source(plan)) {
    # Otherwise the target extent is pinned only when the plan narrows the
    # source. Over a whole source GDAL's own choice is the better one, because
    # it knows where the target projection stops being defined: a global
    # longlat extent taken literally into Web Mercator reaches the poles,
    # where the northing has no finite value and the nearest sample to it is
    # meaningless rather than merely large.
    bbox <- GDAL7::transform_extent(
      unname(extent_to_bbox(plan$extent)), from, warp$crs
    )
    if (all(is.finite(bbox))) {
      args$bbox <- as.double(unname(bbox))
    }
  }

  # size and resolution are mutually exclusive in GDAL too; warp() has already
  # refused the pair, so at most one of these is set.
  if (!is.null(warp$dim)) {
    args$size <- as.integer(warp$dim)
  }
  if (!is.null(warp$resolution)) {
    args$resolution <- as.double(warp$resolution)
  }
  args
}

# Whether a target extent means anything.
#
# Michael's flag, and it is the definite one: an extent is only a picture of
# the data if the distance it claims matches the distance it covers on the
# ground. Invert its corners and measure. A whole-globe source into EPSG:3031
# gives an extent 8e23 m across whose four corners all invert to the north
# pole - zero ground distance - so it is a picture of a singularity, not of
# the source. Size alone cannot say that: with dim = the size returned is the
# size asked for, and only the distance gives it away.
#
# The ratio is near 1 for a projection used over its own domain, about 3 for
# world Mercator clipped at its usual latitude, and unbounded as a source
# reaches a singularity. Ten is the line.
MAX_STRETCH <- 10

# Great-circle distance on a sphere of the WGS84 semi-major axis. A red flag
# needs an order of magnitude, not a geodesic to the millimetre.
ground_distance <- function(lon, lat) {
  r <- 6378137
  p <- pi / 180
  d <- sin((lat[2L] - lat[1L]) * p / 2)^2 +
    cos(lat[1L] * p) * cos(lat[2L] * p) * sin((lon[2L] - lon[1L]) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(d)))
}

# How many times larger the extent is than the ground it covers: 1 for an
# honest extent, Inf when its corners are all the same place. NA when the
# question cannot be asked, which is a target CRS that will not invert to
# longitude and latitude at all.
extent_stretch <- function(bbox, crs) {
  corners <- tryCatch(
    PROJ::proj_trans(
      wk::xy(bbox[c(1L, 3L, 1L, 3L)], bbox[c(2L, 4L, 4L, 2L)], crs = crs),
      "EPSG:4326"
    ),
    error = function(e) NULL
  )
  if (is.null(corners)) {
    return(NA_real_)
  }
  lon <- unclass(corners)$x
  lat <- unclass(corners)$y
  if (anyNA(lon) || anyNA(lat) || !all(is.finite(c(lon, lat)))) {
    return(Inf)
  }

  diagonal <- sqrt((bbox[3L] - bbox[1L])^2 + (bbox[4L] - bbox[2L])^2)
  ground <- max(ground_distance(lon[1:2], lat[1:2]),
                ground_distance(lon[3:4], lat[3:4]))
  if (ground == 0) Inf else diagonal / ground
}

# What the extent looks like once GDAL has chosen it.
#
# The check has to come after the run, not before it, because GDAL clamps
# where it knows how to: a global source into Web Mercator comes back at the
# usual latitude limit, and the extent we would have predicted for it is not
# the one GDAL uses. Polar stereographic has no such limit, so there the
# prediction and the result agree, and both are nonsense.
warn_on_stretch <- function(extent, crs) {
  stretch <- extent_stretch(unname(extent_to_bbox(extent)), crs)
  if (is.na(stretch) || stretch <= MAX_STRETCH) {
    return(invisible(NULL))
  }
  warning(stretch_message(extent, crs, stretch), call. = FALSE)
  invisible(NULL)
}

stretch_message <- function(extent, crs, stretch) {
  times <- if (is.finite(stretch)) {
    paste0(format(stretch, digits = 3), " times")
  } else {
    "immeasurably more than"
  }
  paste0(
    "the target extent is ", format(extent[["xmax"]] - extent[["xmin"]],
                                    digits = 3),
    " across, ", times, " the ground distance it covers, because part of ",
    "this source has no useful position in ", crs_label(crs), ".\n",
    "  Pin the output with warp(extent = ), or narrow the source with ",
    "query(extent = ) first."
  )
}

# Run the pipeline, and when GDAL refuses because the output would be too
# large to address, say why rather than passing on "Too large output raster
# size" with nothing attached. The predicted extent is good enough for the
# diagnosis: GDAL only fails this way when it did not clamp, which is exactly
# when the prediction and its own choice agree.
run_warp <- function(x, args) {
  tryCatch(
    GDAL7::gdal_run("raster reproject", args, progress = FALSE),
    error = function(e) {
      stop(diagnose_warp_failure(x, conditionMessage(e)), call. = FALSE)
    }
  )
}

diagnose_warp_failure <- function(x, message) {
  plan <- S7::prop(x, "plan")
  warp <- plan$warp
  if (is.null(warp) || !is.null(warp$extent)) {
    return(message)
  }
  from <- S7::prop(x, "dataset")@crs
  target <- suppressWarnings(tryCatch(
    GDAL7::transform_extent(unname(extent_to_bbox(plan$extent)), from,
                            warp$crs),
    error = function(e) NULL
  ))
  if (is.null(target) || !all(is.finite(target))) {
    return(message)
  }
  stretch <- extent_stretch(unname(target), warp$crs)
  if (is.na(stretch) || stretch <= MAX_STRETCH) {
    return(message)
  }
  paste0(message, "\n  ", stretch_message(bbox_to_extent(target), warp$crs,
                                          stretch))
}

# A warped plan is realised by running the pipeline into a MEM dataset and
# reading from that. Nothing touches disk, and the result is an ordinary
# raster source, so every verb downstream is the unwarped one.
warped_source <- function(x) {
  require_algorithms("warp")
  args <- c(warp_args(x), list(output = "", "output-format" = "MEM"))
  ds <- run_warp(x, args)

  out <- new_raster_source(paste0("<warped ", S7::prop(x, "dsn"), ">"), ds)
  if (is.null(S7::prop(x, "plan")$warp$extent)) {
    warn_on_stretch(S7::prop(out, "plan")$source_extent,
                    S7::prop(x, "plan")$warp$crs)
  }
  plan <- S7::prop(out, "plan")
  plan$bands <- S7::prop(x, "plan")$bands
  plan$resample <- S7::prop(x, "plan")$resample
  S7::prop(out, "plan") <- plan
  out
}

require_algorithms <- function(verb) {
  if (GDAL7::gdal_has_algorithms()) {
    return(invisible(NULL))
  }
  stop(verb, "() needs GDAL's algorithm registry, which this build does not ",
       "have.\n",
       "  It arrived in GDAL 3.11, and a statically linked GDAL before 3.12 ",
       "has an empty registry even when the API is present.\n",
       "  GDAL7::gdal_has_algorithms() reports what this build can do.",
       call. = FALSE)
}
