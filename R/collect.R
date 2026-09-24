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
#' with its CRS set. The id is always `fid` and the geometry always `geom`,
#' whatever the driver called them. An attribute that already has one of
#' those names comes back as `fid_1` or `geom_1`. When the plan carries a `crs` from
#' [query()], the coordinates are transformed with [PROJ::proj_trans()] on
#' the way out, which is one pass over the geometry and nothing else.
#'
#' The whole vector plan is carried out in GDAL: the filters, the fields,
#' which the driver is told not to read at all, the names, and the limit,
#' which stops the read rather than trimming it. So `as = "arrow"` can hand
#' back the Arrow stream itself, unread, as a `nanoarrow_array_stream` for
#' arrow, duckdb, geoarrow or anything else that takes one, with the CRS in
#' the geometry column's GeoArrow metadata. The one part of a plan that is
#' not in GDAL is `query(crs = )`, so a stream of a reprojected plan is an
#' error rather than a stream in the wrong CRS. A layer allows one stream at a
#' time: read it through, or release it with [GDAL7::release_arrow_stream()],
#' before reading the source again.
#'
#' @param x A source from [src()], usually after [query()].
#' @param ... The named arguments below.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{`as`}{For a raster, `"grd"` (the default) or `"gis"`. For a
#'     vector, `"tibble"` (the default) or `"arrow"`.}
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

  values <- read_plan(ds, plan, out_dim, type)

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

S7::method(collect, vector_source) <- function(x, ..., as = c("tibble", "arrow")) {
  rlang_check_empty(...)
  as <- match.arg(as)
  plan <- S7::prop(x, "plan")
  info <- S7::prop(x, "info")

  if (identical(as, "arrow")) {
    if (!is.null(plan$crs)) {
      stop("query(crs = ) reprojects in R, through PROJ, after the read, so ",
           "there is no stream of it to hand back.\n",
           "  collect() the tibble, or drop crs and reproject the stream ",
           "downstream.", call. = FALSE)
    }
    # GeoArrow's encoding carries the CRS in the geometry column's own
    # metadata, which is how a consumer downstream (geoarrow, duckdb, sf)
    # knows what the coordinates are in.
    return(plan_stream(x, options = "GEOMETRY_METADATA_ENCODING=GEOARROW"))
  }

  d <- read_stream(plan_stream(x))

  geom <- which(names(d) == "geom")
  if (length(geom) == 1L) {
    d[[geom]] <- wk::wkb(unclass(d[[geom]]), crs = crs_or_null(info$crs))
    if (!is.null(plan$crs)) {
      d[[geom]] <- PROJ::proj_trans(d[[geom]], plan$crs)
    }
  } else if (!is.null(plan$crs)) {
    stop("there is no single geometry column to reproject", call. = FALSE)
  }

  tibble::as_tibble(d)
}

# The whole vector plan as one Arrow stream, and every part of it done in
# GDAL: the filters, the fields (as fields the driver is told not to read),
# the fid and geom names, and the limit, which stops the read rather than
# trimming it afterwards. collect() converts this stream; collect(as =
# "arrow") hands it back as it is.
#
# Filters and ignored fields are set every time, cleared when the plan has
# none, because GDAL keeps both on the layer object and hands back the same
# object for the same layer: one left over from an earlier read would narrow
# this one without the plan saying so.
plan_stream <- function(x, options = NULL) {
  plan <- S7::prop(x, "plan")
  lyr <- plan_layer(S7::prop(x, "dataset"), plan)
  GDAL7::set_filter(
    lyr,
    where = plan$where %||% character(0),
    bbox = if (is.null(plan$extent)) numeric(0) else unname(extent_to_bbox(plan$extent))
  )

  fields <- lyr@field_names
  if (!is.null(plan$fields)) {
    missing <- setdiff(plan$fields, fields)
    if (length(missing) > 0L) {
      stop("no field named '", missing[1L], "'.\n  The fields are: ",
           paste(fields, collapse = ", "), call. = FALSE)
    }
    fields <- intersect(fields, plan$fields)
  }
  rename <- standard_names(fields, lyr@fid_column, lyr@geometry_column)
  lyr@ignored_fields <- setdiff(lyr@field_names, fields)

  GDAL7::arrow_stream(lyr, options = options, rename = rename, limit = plan$limit)
}

# nanoarrow warns that it does not recognise GDAL's ogc.wkb extension type and
# hands back the storage type, which is the WKB wanted here. The stream goes
# back to its layer as soon as it is read, since a layer allows one at a time.
read_stream <- function(stream) {
  force(stream)
  on.exit(GDAL7::release_arrow_stream(stream), add = TRUE)
  withCallingHandlers(
    nanoarrow::convert_array_stream(stream),
    warning = function(w) {
      if (grepl("ogc.wkb", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# The id and the geometry come back as `fid` and `geom` whatever the driver
# called them, which is fid and geom in a GeoPackage but OGC_FID and
# wkb_geometry in a shapefile, GeoJSON or any SQL result. GDAL says which
# columns they are, so this renames and never guesses. An attribute being
# read that already has one of those names, such as the `fid` QGIS writes
# into a shapefile, moves aside to `fid_1` (or the first free `_n`), so the
# standard names always mean the id and the geometry. The result is the
# rename for GDAL7::arrow_stream(), c(new = "old").
standard_names <- function(fields, fid_column, geometry_column) {
  from <- c(fid = fid_column, geom = geometry_column)
  from <- from[!is.na(from)]
  renaming <- from[names(from) != from]
  taken <- c(fields, names(from))
  for (clash in names(renaming)[names(renaming) %in% fields]) {
    n <- 1L
    while (paste0(clash, "_", n) %in% taken) n <- n + 1L
    aside <- paste0(clash, "_", n)
    taken <- c(taken, aside)
    renaming[[aside]] <- clash
  }
  renaming
}

# One read of whatever the plan says, padded when the plan leaves the source.
#
# A padded window cannot go to GDAL as it stands, because RasterIO takes an
# integer pixel offset and refuses one outside the raster. So the window is
# clipped to the source, read, and placed back into a full-size array of NA.
# query() aligned the whole padded rectangle to the source's own pixel edges,
# so the clipping is by whole source pixels; only the scaling to an out_size
# smaller than the window can land between two, and that is rounded.
read_plan <- function(ds, plan, out_dim, type = "double") {
  window <- raster_window(plan)
  if (!isTRUE(plan$pad)) {
    return(GDAL7::read_raster(ds, window = window, out_size = out_dim,
                              resample = plan$resample, bands = plan$bands,
                              type = type))
  }
  if (!identical(type, "double")) {
    stop("pad = TRUE needs type = \"double\", and this read is type = \"",
         type, "\".\n",
         "  The part of the window outside the source is not a value, and ",
         "neither a raw nor an integer\n  has one to say so with.",
         call. = FALSE)
  }

  inner <- clip_window(window, plan$source_dimension)
  out <- lapply(plan$bands, function(i) {
    matrix(NA_real_, nrow = out_dim[1L], ncol = out_dim[2L])
  })
  if (is.null(inner)) {
    return(lapply(out, as.vector))
  }

  block <- scale_window(window, inner, out_dim)
  values <- GDAL7::read_raster(ds, window = inner, out_size = block$size,
                               resample = plan$resample, bands = plan$bands,
                               type = type)
  cols <- block$offset[1L] + seq_len(block$size[1L])
  rows <- block$offset[2L] + seq_len(block$size[2L])
  for (i in seq_along(values)) {
    out[[i]][cols, rows] <- values[[i]]
  }
  lapply(out, as.vector)
}

# The part of a window that is really in the raster, or NULL when none is.
clip_window <- function(window, source_dimension) {
  x0 <- max(0, window[1L])
  y0 <- max(0, window[2L])
  x1 <- min(source_dimension[1L], window[1L] + window[3L])
  y1 <- min(source_dimension[2L], window[2L] + window[4L])
  if (x1 <= x0 || y1 <= y0) {
    return(NULL)
  }
  c(x0, y0, x1 - x0, y1 - y0)
}

# Where the clipped window sits in the output grid, and how big it is there.
# Both are whole output pixels, so a read that is also being resampled can put
# the seam up to one output pixel out; a read at source resolution is exact.
scale_window <- function(window, inner, out_dim) {
  sx <- out_dim[1L] / window[3L]
  sy <- out_dim[2L] / window[4L]
  offset <- c(round((inner[1L] - window[1L]) * sx),
              round((inner[2L] - window[2L]) * sy))
  size <- c(max(1, round(inner[3L] * sx)), max(1, round(inner[4L] * sy)))
  size <- pmin(size, out_dim - offset)
  list(offset = offset, size = size)
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
