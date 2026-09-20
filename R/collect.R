#' @include src.R
NULL

#' Read a plan into memory
#'
#' `collect()` is the terminal verb that turns a plan into R data. Everything
#' before it is free; this is where the reading happens, in one GDAL call for
#' a raster and one Arrow stream for a vector.
#'
#' For a raster, `as` picks the in-memory form:
#'
#' * `"grd"`, the default, is [wk::grd_rct()]: an array with
#'   `dim = c(ny, nx, nbands)`, y decreasing down the rows, in a bounding
#'   rectangle that carries the CRS. It costs one transpose per band, because
#'   GDAL's x-fastest order is the transpose of what R's `matrix()` builds,
#'   and it buys `plot()`, `grd_crop()`, `grd_tile()` and the rest of wk's
#'   grid vocabulary.
#' * `"gis"` is the shape `gdalraster::read_ds()` returns: one flat atomic
#'   vector, band after band, x varying fastest within a band, carrying a
#'   `"gis"` attribute of `type`, `bbox`, `dim`, `srs` and `datatype`. It is
#'   the zero-transpose path, and [ximage::ximage()] draws it directly.
#'
#' `type` is the R type the values are read into. `"raw"` is one byte a value
#' and is what an RGB image wants; `"integer"` is four and covers Byte, Int8,
#' Int16, UInt16 and Int32. A band whose type will not fit is an error rather
#' than a silent clamp. The `"grd"` form is built from whatever comes back, so
#' a raw read stays a raw array.
#'
#' `mask` decides what happens to a value the source itself declares absent.
#' A band's nodata value is a fill, not a measurement, so by default it comes
#' back as `NA` rather than as the number that stands for it. This is what
#' terra and stars do, and what rasterio calls `masked`. It pivots on `type`,
#' because only a double has a missing value to put there: `mask` defaults to
#' `TRUE` for `"double"` and to `FALSE` for `"integer"` and `"raw"`, and
#' asking for it with either of those is an error rather than a pretence.
#'
#' Masking is exact rather than approximate, and that matters when the read is
#' resampled. A nearest read returns source values unchanged, so every fill
#' pixel is caught. An averaging or bilinear read blends a fill with its
#' neighbours, and the blend is no longer equal to the nodata value, so it
#' survives as a number. That is the same in every package that does this, and
#' it is why `resample = "nearest"` is the default.
#'
#' For a vector, the result is a tibble whose geometry column is [wk::wkb()]
#' with its CRS set. The column keeps whatever name GDAL gave it, because wk
#' finds a geometry column by asking rather than by name, so `wk_bbox()`,
#' `wk_plot()` and the chunked handlers all work on the result unchanged. When
#' the plan carries a `crs` from [query()], the coordinates are transformed
#' with [PROJ::proj_trans()] on the way out, which is one pass over the
#' geometry and nothing else.
#'
#' @param x A source from [src()], usually after [query()].
#' @param ... The named arguments below.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{`as`}{Raster only. `"grd"` (the default) or `"gis"`.}
#'   \item{`type`}{Raster only. `"double"` (the default), `"integer"` or
#'     `"raw"`.}
#'   \item{`mask`}{Raster only. Return each band's nodata value as `NA`.
#'     Defaults to `TRUE` for `type = "double"` and `FALSE` otherwise.}
#' }
#'
#' @return For a raster, a `wk_grd_rct` or a vector carrying a `"gis"`
#'   attribute. For a vector, a tibble.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' g <- collect(query(x, dim = c(10, 5)))
#' dim(g$data)
#'
#' v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
#' collect(query(v, where = "population > 1e6"))
collect <- S7::new_generic("collect", "x", function(x, ...) {
  S7::S7_dispatch()
})

S7::method(collect, raster_source) <- function(x, ..., as = c("grd", "gis"),
                                               type = "double", mask = NULL) {
  rlang_check_empty(...)
  as <- match.arg(as)
  mask <- resolve_mask(mask, type)
  refuse_on_blocking(x, "collect")

  # A warped plan is realised into a MEM dataset first, and read from there
  # as if it had been the source all along.
  if (!is.null(S7::prop(x, "plan")$warp)) {
    x <- warped_source(x)
    on.exit(GDAL7::gdal_close(S7::prop(x, "dataset")), add = TRUE)
  }

  plan <- S7::prop(x, "plan")
  ds <- S7::prop(x, "dataset")
  out_dim <- plan$out_dimension %||% plan$dimension

  values <- GDAL7::read_raster(
    ds,
    window = raster_window(plan),
    out_size = out_dim,
    resample = plan$resample,
    bands = plan$bands,
    type = type
  )

  if (mask) {
    values <- mask_values(values, band_nodata(ds, plan$bands))
  }

  crs <- ds@crs
  if (identical(as, "gis")) {
    gis_result(values, plan$extent, out_dim, crs, band_types(ds, plan$bands))
  } else {
    grd_result(values, plan$extent, out_dim, crs)
  }
}

S7::method(collect, vector_source) <- function(x, ...) {
  rlang_check_empty(...)
  plan <- S7::prop(x, "plan")
  info <- S7::prop(x, "info")

  d <- GDAL7::read_vector(
    S7::prop(x, "dataset"),
    layer = plan$layer,
    where = plan$where,
    bbox = if (is.null(plan$extent)) NULL else unname(extent_to_bbox(plan$extent))
  )

  geom <- which(vapply(d, is_wkb_column, logical(1)))
  if (length(geom) == 1L) {
    d[[geom]] <- wk::wkb(unclass(d[[geom]]), crs = crs_or_null(info$crs))
    if (!is.null(plan$crs)) {
      d[[geom]] <- PROJ::proj_trans(d[[geom]], plan$crs)
    }
  } else if (!is.null(plan$crs)) {
    stop("there is no single geometry column to reproject", call. = FALSE)
  }

  # Field and limit narrowing happen here rather than in GDAL, because the
  # Arrow stream GDAL7 exposes has no column projection and no row limit. It
  # saves memory rather than I/O, and this is the honest place to say so.
  if (!is.null(plan$fields)) {
    keep <- union(names(d)[geom], plan$fields)
    missing <- setdiff(plan$fields, names(d))
    if (length(missing) > 0L) {
      stop("no field named '", missing[1L], "'.\n  The fields are: ",
           paste(setdiff(names(d), names(d)[geom]), collapse = ", "),
           call. = FALSE)
    }
    d <- d[, intersect(names(d), keep), drop = FALSE]
  }
  if (!is.null(plan$limit) && nrow(d) > plan$limit) {
    d <- d[seq_len(plan$limit), , drop = FALSE]
  }

  tibble::as_tibble(d)
}

is_wkb_column <- function(column) {
  is.list(column) && length(column) > 0L &&
    all(vapply(column, function(e) is.raw(e) || is.null(e), logical(1)))
}

# The plan's extent is already snapped to whole source pixels by query(), so
# these divisions land on whole numbers; round() rather than floor() so that
# floating-point arithmetic cannot lose a pixel off an edge.
raster_window <- function(plan) {
  src_extent <- plan$source_extent
  src_dim <- plan$source_dimension
  xres <- (src_extent[["xmax"]] - src_extent[["xmin"]]) / src_dim[1L]
  yres <- (src_extent[["ymax"]] - src_extent[["ymin"]]) / src_dim[2L]

  c(
    round((plan$extent[["xmin"]] - src_extent[["xmin"]]) / xres),
    round((src_extent[["ymax"]] - plan$extent[["ymax"]]) / yres),
    plan$dimension[1L],
    plan$dimension[2L]
  )
}

# Nodata is a fill value, not a measurement, so it comes back as NA. The
# decision pivots on type because only a double has a missing value to put
# there: a raw has none at all, and NA_integer_ is a real Int32 value, so
# masking an integer read would make a measurement and a fill indis-
# tinguishable in the other direction.
resolve_mask <- function(mask, type) {
  if (is.null(mask)) {
    return(identical(type, "double"))
  }
  if (!is.logical(mask) || length(mask) != 1L || is.na(mask)) {
    stop("mask must be TRUE or FALSE", call. = FALSE)
  }
  if (mask && !identical(type, "double")) {
    stop("mask = TRUE needs type = \"double\", and this read is type = \"",
         type, "\".\n",
         "  A raw has no missing value, and NA_integer_ is a real Int32 ",
         "value,\n",
         "  so neither can say \"absent\" without also losing a number.\n",
         "  Use type = \"double\", or mask = FALSE and compare against the ",
         "nodata value yourself.",
         call. = FALSE)
  }
  mask
}

band_nodata <- function(ds, bands) {
  vapply(bands, function(i) {
    value <- GDAL7::get_raster_band(ds, i)@nodata_value
    if (is.null(value)) NA_real_ else as.double(value)
  }, double(1))
}

mask_values <- function(values, nodata) {
  for (i in seq_along(values)) {
    # GDAL is entitled to declare NaN as the nodata value, and NaN is not
    # equal to itself, so that case is asked about rather than compared. It
    # also has to be tested before is.na(), which is TRUE for a NaN as well.
    nan_fill <- is.nan(nodata[i])
    if (!nan_fill && is.na(nodata[i])) {
      next
    }
    v <- values[[i]]
    hit <- if (nan_fill) is.nan(v) else !is.na(v) & v == nodata[i]
    v[hit] <- NA_real_
    values[[i]] <- v
  }
  values
}

band_types <- function(ds, bands) {
  vapply(bands, function(i) GDAL7::get_raster_band(ds, i)@data_type_name,
         character(1))
}

# GDAL hands back one row at a time with x varying fastest, which is the
# transpose of what matrix() builds, so each band is one t().
grd_result <- function(values, extent, out_dim, crs) {
  nx <- out_dim[1L]
  ny <- out_dim[2L]
  bands <- lapply(values, function(v) t(matrix(v, nrow = nx, ncol = ny)))

  data <- if (length(bands) == 1L) {
    array(bands[[1L]], dim = c(ny, nx))
  } else {
    array(unlist(bands, use.names = FALSE), dim = c(ny, nx, length(bands)))
  }

  wk::grd_rct(
    data,
    bbox = wk::rct(
      extent[["xmin"]], extent[["ymin"]], extent[["xmax"]], extent[["ymax"]],
      crs = crs_or_null(crs)
    )
  )
}

#' The gis attribute
#'
#' `gis_attr()` builds the attribute that `gdalraster::read_ds()` puts on its
#' result, which [ximage::ximage()] reads to draw an image in its own
#' coordinates. `adgl` emits this shape without importing gdalraster, so the
#' contract is reproduced here and asserted by a test rather than inherited.
#'
#' Read from gdalraster at its master branch on 2026-09-20. Note the two
#' orders: `bbox` is `c(xmin, ymin, xmax, ymax)` while `dim` is
#' `c(nx, ny, nbands)`, with x first.
#'
#' @param bbox `c(xmin, ymin, xmax, ymax)`.
#' @param dim `c(nx, ny, nbands)`.
#' @param srs The CRS as WKT, or `""`.
#' @param datatype Character, the GDAL type name of each band.
#'
#' @return A list of five elements: `type`, `bbox`, `dim`, `srs`, `datatype`.
#' @export
#' @examples
#' gis_attr(c(0, 0, 10, 10), c(10L, 10L, 1L), "", "Byte")
gis_attr <- function(bbox, dim, srs, datatype) {
  list(
    type = "raster",
    bbox = as.double(unname(bbox)),
    dim = as.integer(unname(dim)),
    srs = as.character(srs),
    datatype = as.character(datatype)
  )
}

gis_result <- function(values, extent, out_dim, crs, datatype) {
  out <- unlist(values, use.names = FALSE)
  attr(out, "gis") <- gis_attr(
    bbox = extent_to_bbox(extent),
    dim = c(out_dim[1L], out_dim[2L], length(values)),
    srs = crs_or_null(crs) %||% "",
    datatype = datatype
  )
  out
}

# A finding that blocks is one where the result would be a lie rather than a
# degraded truth, so the terminal verbs stop on it.
refuse_on_blocking <- function(x, verb) {
  findings <- S7::prop(x, "findings")
  blocking <- findings[findings$severity == "blocks", , drop = FALSE]
  if (nrow(blocking) == 0L) {
    return(invisible(NULL))
  }
  stop(verb, "() will not run on this source: ", blocking$what[1L], ".\n",
       "  ", blocking$consequence[1L], ".\n",
       "  See report(x); the fix is outside this package by design.",
       call. = FALSE)
}

rlang_check_empty <- function(...) {
  dots <- list(...)
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }
  named <- names(dots) %||% rep("", length(dots))
  stop("unused argument", if (length(dots) > 1L) "s" else "", ": ",
       paste(named[nzchar(named)], collapse = ", "), call. = FALSE)
}
