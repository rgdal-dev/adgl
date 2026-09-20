#' @include src.R
NULL

#' Write a plan to a file
#'
#' `write_to()` is the terminal verb that sends a plan to any format in GDAL's
#' suite rather than into R. The driver comes from the file extension unless
#' you name one.
#'
#' A warped plan compiles straight into GDAL's own reprojection pipeline and
#' never enters R at all, which is what you want for a large result. Every
#' other plan is read into memory and written back out, because the read and
#' the write are each one call and the intermediate is the size of the result
#' you asked for rather than the size of the source. If that intermediate is
#' too big, narrow the plan with `dim` first, or tile it yourself.
#'
#' @param x A source from [src()], usually after [query()].
#' @param dsn Where to write it.
#' @param ... The named arguments below.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{`driver`}{The GDAL driver's short name. The default reads it from
#'     the extension of `dsn`.}
#'   \item{`options`}{Character vector of `"KEY=VALUE"` creation options,
#'     such as `c("COMPRESS=ZSTD", "TILED=YES")`.}
#'   \item{`layer`}{Vector only. The layer name to create. Defaults to the
#'     file's base name.}
#' }
#'
#' @return `dsn`, invisibly.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' path <- tempfile(fileext = ".tif")
#' write_to(query(x, dim = c(10, 5)), path)
#' src(path)
#' unlink(path)
write_to <- S7::new_generic("write_to", "x", function(x, dsn, ...) {
  S7::S7_dispatch()
})

S7::method(write_to, raster_source) <- function(x, dsn, ..., driver = NULL,
                                                options = NULL) {
  rlang_check_empty(...)
  refuse_on_blocking(x, "write_to")
  driver <- driver %||% driver_from_extension(dsn, "raster")
  plan <- S7::prop(x, "plan")

  if (!is.null(plan$warp) && GDAL7::gdal_has_algorithms()) {
    args <- c(warp_args(x), list(
      output = dsn,
      "output-format" = driver,
      overwrite = TRUE
    ))
    if (!is.null(options)) {
      args[["creation-option"]] <- options
    }
    run_warp(x, args)
    return(invisible(dsn))
  }

  if (!is.null(plan$warp)) {
    # No algorithm registry: realise the warp into MEM and copy that out.
    x <- warped_source(x)
    on.exit(GDAL7::gdal_close(S7::prop(x, "dataset")), add = TRUE)
    plan <- S7::prop(x, "plan")
  }

  out_dim <- plan$out_dimension %||% plan$dimension
  ds <- S7::prop(x, "dataset")
  values <- GDAL7::read_raster(
    ds,
    window = raster_window(plan),
    out_size = out_dim,
    resample = plan$resample,
    bands = plan$bands
  )

  # Through MEM rather than straight to the driver, so that a copy-only
  # driver such as COG or PNG works by the same path as a creatable one.
  mem <- GDAL7::gdal_create("", out_dim[1L], out_dim[2L],
                            bands = length(values),
                            type = mem_type(ds, plan$bands),
                            driver = "MEM")
  on.exit(GDAL7::gdal_close(mem), add = TRUE)

  mem@geotransform <- extent_geotransform(plan$extent, out_dim)
  if (has_crs(ds@crs)) {
    mem@crs <- ds@crs
  }
  carry_nodata(ds, mem, plan$bands)
  GDAL7::write_raster(mem, values)

  # gdal_create_copy() hands back the new dataset still open, and a GTiff
  # only has its data on disk once it is closed. Without this the file exists,
  # has the right shape, and holds nothing but fill.
  out <- GDAL7::gdal_create_copy(mem, dsn, driver = driver, options = options,
                                 progress = FALSE)
  GDAL7::gdal_close(out)
  invisible(dsn)
}

S7::method(write_to, vector_source) <- function(x, dsn, ..., driver = NULL,
                                                options = NULL, layer = NULL) {
  rlang_check_empty(...)
  driver <- driver %||% driver_from_extension(dsn, "vector")
  info <- S7::prop(x, "info")
  plan <- S7::prop(x, "plan")

  d <- collect(x)
  geom <- which(vapply(d, function(column) inherits(column, "wk_wkb"),
                       logical(1)))
  # The CRS to declare is whatever the collected geometry is actually in,
  # which is the target when query(crs = ) reprojected it.
  crs <- crs_or_null(plan$crs %||% info$crs)
  if (length(geom) == 1L) {
    d[[geom]] <- unclass(d[[geom]])
  }

  GDAL7::write_vector(
    as.data.frame(d), dsn,
    layer = layer,
    driver = driver,
    crs = crs,
    geometry_type = info$geometry_type,
    layer_options = options
  )
  invisible(dsn)
}

# A nodata value is metadata the source does have, so losing it on the way
# through MEM would be this package creating the very deficiency it reports.
carry_nodata <- function(from, to, bands) {
  for (i in seq_along(bands)) {
    value <- GDAL7::get_raster_band(from, bands[i])@nodata_value
    if (!is.null(value) && !is.na(value)) {
      band <- GDAL7::get_raster_band(to, i)
      band@nodata_value <- value
    }
  }
  invisible(NULL)
}

# An extent and a dimension back to a north-up geotransform.
extent_geotransform <- function(extent, dimension) {
  c(extent[["xmin"]],
    (extent[["xmax"]] - extent[["xmin"]]) / dimension[1L],
    0,
    extent[["ymax"]],
    0,
    -(extent[["ymax"]] - extent[["ymin"]]) / dimension[2L])
}

# The widest of the bands being written, so that no band is narrowed by the
# choice of container.
mem_type <- function(ds, bands) {
  types <- band_types(ds, bands)
  order <- c("Byte", "Int8", "UInt16", "Int16", "UInt32", "Int32",
             "UInt64", "Int64", "Float32", "Float64")
  known <- types[types %in% order]
  if (length(known) == 0L) {
    return("Float64")
  }
  order[max(match(known, order))]
}

raster_extensions <- c(
  tif = "GTiff", tiff = "GTiff", vrt = "VRT", png = "PNG", jpg = "JPEG",
  jpeg = "JPEG", nc = "netCDF", img = "HFA", asc = "AAIGrid", grd = "RST"
)

vector_extensions <- c(
  gpkg = "GPKG", shp = "ESRI Shapefile", geojson = "GeoJSON",
  json = "GeoJSON", parquet = "Parquet", fgb = "FlatGeobuf",
  gml = "GML", csv = "CSV", sqlite = "SQLite"
)

driver_from_extension <- function(dsn, kind) {
  table <- if (kind == "raster") raster_extensions else vector_extensions
  ext <- tolower(sub(".*\\.", "", basename(dsn)))
  if (!nzchar(ext) || !ext %in% names(table)) {
    stop("no driver is inferred from '", dsn, "'; name one with `driver`.\n",
         "  GDAL7::gdal_drivers() lists what this build can write.",
         call. = FALSE)
  }
  unname(table[[ext]])
}
