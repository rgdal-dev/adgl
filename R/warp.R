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
#' Nothing is read here. The reprojection is recorded in the plan and happens
#' when a terminal verb asks for the result, so `warp()` composes with
#' [query()] in either order.
#'
#' @param x A raster source from [src()].
#' @param crs The target CRS: anything GDAL understands, such as
#'   `"EPSG:3031"`, a PROJ string, or WKT.
#' @param ... The named arguments below.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{`resample`}{One of `"nearest"` (the default), `"bilinear"`,
#'     `"cubic"`, `"cubicspline"`, `"lanczos"`, `"average"`, `"mode"`,
#'     `"rms"` or `"sum"`.}
#'   \item{`dim`}{`c(nx, ny)`, the size of the warped grid. When it is
#'     `NULL`, GDAL picks a size that roughly preserves the source
#'     resolution.}
#' }
#'
#' @return `x` with a reprojection added to its plan.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' w <- warp(query(x, extent = c(-170, 170, -80, 80)), "EPSG:3857",
#'           resample = "bilinear")
#' w
warp <- S7::new_generic("warp", "x", function(x, crs, ...) {
  S7::S7_dispatch()
})

S7::method(warp, raster_source) <- function(x, crs, ..., resample = "nearest",
                                            dim = NULL) {
  rlang_check_empty(...)
  if (!is.character(crs) || length(crs) != 1L || is.na(crs)) {
    stop("`crs` must be a single, non-missing string", call. = FALSE)
  }
  refuse_on_blocking(x, "warp")

  if (!is.null(dim)) {
    dim <- as.integer(dim)
    if (length(dim) != 2L || anyNA(dim) || any(dim < 1L)) {
      stop("`dim` must be two positive whole numbers: nx, ny", call. = FALSE)
    }
  }

  plan <- S7::prop(x, "plan")
  plan$warp <- list(
    crs = crs,
    resample = check_warp_resample(resample),
    dim = dim %||% plan$out_dimension
  )
  S7::prop(x, "plan") <- plan
  x
}

S7::method(warp, vector_source) <- function(x, crs, ...) {
  stop("warp() is a raster verb. Reprojecting features resamples nothing, ",
       "so it belongs in query(crs = ).", call. = FALSE)
}

# GDAL's warper takes a slightly different set of methods from RasterIO: it
# has "sum" and it has no "gauss".
warp_resample_methods <- c("nearest", "bilinear", "cubic", "cubicspline",
                           "lanczos", "average", "mode", "rms", "sum")

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

  # The target extent is pinned only when the plan narrows the source. Over a
  # whole source GDAL's own choice is the better one, because it knows where
  # the target projection stops being defined: a global longlat extent taken
  # literally into Web Mercator reaches the poles, where the northing has no
  # finite value and the nearest sample to it is meaningless rather than
  # merely large.
  if (!isTRUE(all.equal(unname(plan$extent), unname(plan$source_extent)))) {
    bbox <- GDAL7::transform_extent(
      unname(extent_to_bbox(plan$extent)), from, warp$crs
    )
    if (all(is.finite(bbox))) {
      args$bbox <- as.double(unname(bbox))
    }
  }

  if (!is.null(warp$dim)) {
    args$size <- as.integer(warp$dim)
  }
  args
}

# A warped plan is realised by running the pipeline into a MEM dataset and
# reading from that. Nothing touches disk, and the result is an ordinary
# raster source, so every verb downstream is the unwarped one.
warped_source <- function(x) {
  require_algorithms("warp")
  args <- c(warp_args(x), list(output = "", "output-format" = "MEM"))
  ds <- GDAL7::gdal_run("raster reproject", args, progress = FALSE)

  out <- new_raster_source(paste0("<warped ", S7::prop(x, "dsn"), ">"), ds)
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
