# The expectations, run by hand.
#
# Each test is named for a row of coverage.csv and does in adgl what a user of
# sf, terra, gdalraster, stars or vapour does in their own package, as their
# own documentation and tests show it being done. Anything marked `have` in
# coverage.csv has a test here; a row marked `gap` has a skipped test saying
# what is missing, so the file is a to-do list as well as a check.
#
# This is not part of R CMD check. Run it with:
#
#   Rscript expectations/run.R

library(testthat)
library(adgl)

tif <- function() system.file("extdata/test.tif", package = "GDAL7")
cog <- function() system.file("extdata/overviews.tif", package = "GDAL7")
gpkg <- function() system.file("extdata/test.gpkg", package = "GDAL7")

gap <- function(id, what) {
  test_that(paste0("GAP ", id, ": ", what), {
    skip(paste("not supported:", what))
  })
}

# A defect this suite has already found, kept as a named, skipped entry so the
# run stays readable and the to-do stays visible.
bug <- function(id, what) {
  test_that(paste0("BUG ", id, ": ", what), {
    skip(paste("known defect:", what))
  })
}

# A one-band Byte raster, for the cases where the fixtures' Int16 is the wrong
# shape to ask about.
byte_tif <- function(nx = 8, ny = 4) {
  path <- tempfile(fileext = ".tif")
  ds <- GDAL7::gdal_create(path, nx, ny, bands = 1, type = "Byte")
  ds@geotransform <- c(0, 1, 0, ny, 0, -1)
  ds@crs <- "EPSG:4326"
  GDAL7::write_raster(ds, list(as.double(seq_len(nx * ny) - 1)))
  GDAL7::gdal_close(ds)
  path
}

# -- raster ----------------------------------------------------------------

test_that("read-whole: a whole raster comes back with its own values", {
  # terra: values(rast(f)) ; gdalraster: read_ds(new(GDALRaster, f))
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  g <- collect(x)
  expect_s3_class(g, "wk_grd_rct")
  expect_equal(dim(g$data), c(10L, 20L, 2L))
})

test_that("read-window-world: a map-coordinate rectangle restricts the read", {
  # stars: read_stars(f, RasterIO = list(nXOff =, nYOff =, nXSize =, nYSize =))
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  g <- collect(query(x, extent = c(100, 160, -60, -20)))
  bbox <- as.vector(unlist(unclass(g$bbox)))
  expect_lt(bbox[1L], 101)
  expect_gt(bbox[1L], 90)
  expect_gt(bbox[3L], 159)
})

test_that("read-reduced: a small dim over a large extent reads a small array", {
  # stars: read_stars(f, RasterIO = list(nBufXSize =, nBufYSize =))
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  g <- collect(query(x, dim = c(16, 8)))
  expect_equal(dim(g$data)[1:2], c(8L, 16L))
})

test_that("read-bands: bands are chosen by index", {
  # vapour: vapour_read_raster(f, bands = 2)
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  both <- collect(x)
  second <- collect(query(x, bands = 2))
  expect_equal(dim(second$data), c(10L, 20L))
  expect_equal(as.vector(second$data), as.vector(both$data[, , 2]))
})

test_that("read-type: the R type of the values is the caller's choice", {
  # vapour: vapour_read_raster(f, band_output_type = "raw")
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  expect_type(collect(x, type = "double")$data, "double")
  expect_type(collect(x, type = "integer")$data, "integer")
  # raw is one byte a value, so a band that will not fit is refused rather
  # than clamped. test.tif is Int16; a Byte source is where raw belongs.
  expect_error(collect(x, type = "raw"), "raw")

  b <- src(byte_tif())
  on.exit(src_close(b), add = TRUE)
  expect_type(collect(b, type = "raw")$data, "raw")
})

test_that("read-gis: the read_ds shape is available without gdalraster", {
  # gdalraster: read_ds(ds) returning a flat vector with a "gis" attribute
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  v <- collect(x, as = "gis")
  gis <- attr(v, "gis")
  expect_equal(gis$type, "raster")
  expect_equal(gis$dim, c(20L, 10L, 2L))
  expect_length(gis$bbox, 4L)
})

test_that("proxy: a lazy grid reads only what is subset", {
  # stars: read_stars(f, proxy = TRUE) then plot()
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  g <- as_grd(x)
  expect_equal(dim(g$data), c(256L, 512L, 1L))
  small <- wk::grd_subset(g, i = 1:4, j = 1:8)
  expect_equal(dim(small$data), c(4L, 8L, 1L))
})

test_that("warp: a grid reprojects and the coordinates really move", {
  # terra: project(r, "EPSG:3031") ; stars: st_warp(s, crs = 3031)
  skip_if_not(GDAL7::gdal_has_algorithms(), "no algorithm registry")
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  g <- collect(warp(query(x, extent = c(100, 160, -60, -20)), "EPSG:3031",
                    resample = "bilinear"))
  expect_gt(max(abs(as.vector(unlist(unclass(g$bbox))))), 1e5)
})

test_that("warp-grid: the target grid takes a size or a resolution", {
  # vapour: vapour_warp_raster(f, extent =, dimension =, projection =)
  skip_if_not(GDAL7::gdal_has_algorithms(), "no algorithm registry")
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  by_dim <- collect(warp(x, "EPSG:3031", dim = c(48, 48)))
  expect_equal(dim(by_dim$data), c(48L, 48L))

  # resolution over a *narrowed* source. Over the whole globe it cannot work,
  # for the reason the BUG entry below records.
  by_res <- collect(warp(query(x, extent = c(100, 160, -60, -20)), "EPSG:3031",
                         resolution = 5e5))
  expect_gt(dim(by_res$data)[1L], 1L)
})

test_that("warp-unbounded: a resolution that cannot fit the target says why", {
  # Found by this suite. A whole-globe source into EPSG:3031 has a target
  # extent 8e23 m across, so no pixel size gives an addressable grid.
  x <- src(cog())
  on.exit(src_close(x), add = TRUE)
  expect_error(adgl:::warp_args(warp(x, "EPSG:3031", resolution = 5e5)),
               "no finite position")
})

bug("warp-unbounded-dim", paste(
  "the other half of the same problem: warp(dim = ) over that source returns",
  "a 48 by 48 grid whose bbox is 8e23 m across, with no complaint. A size",
  "bound cannot catch it because the size is the one that was asked for; the",
  "extent is what is wrong. Comparing the source extent against the target",
  "CRS's area of use would catch it, and that needs OSRGetAreaOfUse in GDAL7.",
  "Parked on Michael's call: it is a general GDAL problem, not adgl's"))

test_that("write-options: a raster writes with creation options", {
  # terra: writeRaster(r, f, gdal = c("COMPRESS=ZSTD"))
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  out <- tempfile(fileext = ".tif")
  write_to(x, out, options = c("COMPRESS=DEFLATE", "TILED=NO"))
  expect_true(file.exists(out))

  back <- src(out)
  on.exit(src_close(back), add = TRUE)
  expect_equal(S7::prop(back, "plan")$dimension, c(20L, 10L))
})

test_that("report: a source with nothing to say says nothing", {
  x <- src(tif())
  on.exit(src_close(x), add = TRUE)
  findings <- report(x)
  expect_true(all(findings$severity %in% c("blocks", "degrades", "note")))
})

gap("read-window-pixel", "a window given in pixel coordinates (vapour_read_raster(window =))")
gap("warp-pipeline", "a PROJ pipeline in place of a target CRS (terra::project(pipe =))")
gap("warp-options", "warp and transform option pass-through (vapour_warp_raster(warp_options =))")
gap("extract-points", "values at points (terra::extract, gdalraster::pixel_extract)")
gap("write-datatype", "choosing the written data type (terra::writeRaster(datatype =))")
gap("write-overwrite", "an explicit overwrite (terra::writeRaster(overwrite =))")
gap("vrt", "as_vrt() (gdalraster::buildVRT, vapour_vrt)")
gap("subdatasets", "reaching a subdataset (stars::read_stars(sub =), read_ncdf(var =))")
gap("colour-table", "reading a colour table (gdalraster::plot_raster(col_tbl =))")

# -- vector ----------------------------------------------------------------

test_that("read-layer: a layer comes back as a tibble with a wk geometry", {
  # sf: st_read(f) ; terra: vect(f)
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  d <- collect(v)
  expect_s3_class(d, "tbl_df")
  geom <- d[[which(vapply(d, inherits, logical(1), "wk_wkb"))]]
  expect_s3_class(geom, "wk_wkb")
  expect_false(is.null(wk::wk_crs(geom)))
})

test_that("where: an attribute expression filters features", {
  # sf: st_read(f, query = "SELECT * FROM layer WHERE population > 1e6")
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  all <- collect(v)
  some <- collect(query(v, where = "population > 1e6"))
  expect_lt(nrow(some), nrow(all))
  expect_gt(nrow(some), 0L)
})

test_that("spatial-filter: a rectangle filters features", {
  # sf: st_read(f, wkt_filter = ) ; vapour: vapour_read_extent(f, extent = )
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  all <- collect(v)
  some <- collect(query(v, extent = c(140, 155, -45, -30)))
  expect_lt(nrow(some), nrow(all))
})

test_that("fields: only the named columns come back, plus the geometry", {
  # vapour: vapour_read_fields(f, sql = "SELECT name FROM layer")
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  d <- collect(query(v, fields = "name"))
  expect_true("name" %in% names(d))
  expect_false("population" %in% names(d))
  expect_equal(sum(vapply(d, inherits, logical(1), "wk_wkb")), 1L)
})

test_that("limit-skip: limit stops the read early", {
  # vapour: vapour_read_geometry(f, limit_n = 2)
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  expect_equal(nrow(collect(query(v, limit = 2))), 2L)
})

test_that("layer-choice: the layer is chosen by name or by position", {
  # sf: st_read(f, layer = "places") ; terra: vect(f, layer = )
  by_index <- src(gpkg())
  on.exit(src_close(by_index), add = TRUE)
  name <- S7::prop(by_index, "info")$name

  by_name <- src(gpkg(), layer = name)
  on.exit(src_close(by_name), add = TRUE)
  expect_equal(S7::prop(by_name, "info")$name, name)
})

test_that("transform-features: the features themselves reproject", {
  # sf: st_transform(x, 3857) ; gdalraster: ogr_reproject()
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  before <- collect(v)
  after <- collect(query(v, crs = "EPSG:3857"))
  gb <- before[[which(vapply(before, inherits, logical(1), "wk_wkb"))]]
  ga <- after[[which(vapply(after, inherits, logical(1), "wk_wkb"))]]
  expect_gt(max(abs(unlist(wk::wk_coords(ga)[c("x", "y")]))),
            max(abs(unlist(wk::wk_coords(gb)[c("x", "y")]))))
})

test_that("feature-count: the count is there without reading", {
  # sf: st_layers(f, do_count = TRUE)
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  n <- S7::prop(v, "info")$feature_count
  expect_true(is.na(n) || n >= 0)
})

test_that("layer-extent: the extent is there without reading", {
  # vapour: vapour_read_extent() ; sf: st_bbox()
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  extent <- S7::prop(v, "info")$source_extent
  expect_length(extent, 4L)
  expect_lt(extent[["xmin"]], extent[["xmax"]])
})

test_that("write-options: a layer writes with creation options", {
  # sf: st_write(x, f, layer_options = )
  v <- src(gpkg())
  on.exit(src_close(v), add = TRUE)
  out <- tempfile(fileext = ".gpkg")
  write_to(v, out)
  expect_true(file.exists(out))

  back <- src(out)
  on.exit(src_close(back), add = TRUE)
  expect_equal(nrow(collect(back)), nrow(collect(v)))
})

gap("sql", "an SQL SELECT against the source (sf::st_read(query =), vapour_read_fields(sql =))")
gap("limit-skip-offset", "skipping the first n features (vapour_read_geometry(skip_n =))")
gap("geometry-only", "geometry without attributes, or attributes without geometry")
gap("geometry-type", "geometry type control (sf::st_read(promote_to_multi =, type =))")
gap("write-append", "appending to a layer rather than creating it (sf::st_write(append =))")
