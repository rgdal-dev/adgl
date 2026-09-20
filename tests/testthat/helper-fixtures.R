test_tif <- function() system.file("extdata/test.tif", package = "GDAL7")
test_cog <- function() system.file("extdata/overviews.tif", package = "GDAL7")
test_gpkg <- function() system.file("extdata/test.gpkg", package = "GDAL7")

# A raster with no georeferencing at all: GDAL reports the identity
# geotransform and an empty CRS, which is the quiet deficiency report() is
# for. Created rather than shipped, so the fixture cannot drift from what
# this build of GDAL actually does with such a file.
byte_tif <- function(nx = 4, ny = 4) {
  path <- tempfile(fileext = ".tif")
  ds <- GDAL7::gdal_create(path, nx, ny, bands = 1, type = "Byte")
  ds@geotransform <- c(0, 1, 0, ny, 0, -1)
  ds@crs <- "EPSG:4326"
  GDAL7::write_raster(ds, list(as.double(seq_len(nx * ny) - 1)))
  GDAL7::gdal_close(ds)
  path
}

bare_tif <- function(nx = 4, ny = 4, type = "Byte") {
  path <- tempfile(fileext = ".tif")
  ds <- GDAL7::gdal_create(path, nx, ny, bands = 1, type = type)
  GDAL7::write_raster(ds, list(rep(1, nx * ny)))
  GDAL7::gdal_close(ds)
  path
}
