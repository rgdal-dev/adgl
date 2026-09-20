test_that("a raster round-trips through a file unchanged", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  path <- tempfile(fileext = ".tif")
  on.exit(unlink(path), add = TRUE)

  write_to(query(x, bands = 1), path)
  back <- src(path)
  on.exit(src_close(back), add = TRUE)

  expect_equal(collect(back)$data, collect(query(x, bands = 1))$data)
  expect_equal(unname(S7::prop(back, "plan")$source_extent),
               c(-180, 180, -90, 90))
})

test_that("the written file keeps the metadata the source had", {
  # Writing through MEM must not drop the nodata value, or the package would
  # be creating the very deficiency report() exists to name.
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  path <- tempfile(fileext = ".tif")
  on.exit(unlink(path), add = TRUE)

  write_to(query(x, dim = c(10, 5)), path)
  back <- src(path)
  on.exit(src_close(back), add = TRUE)

  expect_false("no_nodata" %in% report(back)$check)
  expect_false("no_crs" %in% report(back)$check)
})

test_that("a narrowed plan writes only what was asked for", {
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)
  path <- tempfile(fileext = ".tif")
  on.exit(unlink(path), add = TRUE)

  write_to(query(x, extent = c(0, 90, 0, 45), dim = c(16, 16)), path)
  back <- src(path)
  on.exit(src_close(back), add = TRUE)

  expect_equal(S7::prop(back, "plan")$source_dimension, c(16L, 16L))
  expect_equal(unname(S7::prop(back, "plan")$source_extent), c(0, 90, 0, 45))
})

test_that("the driver comes from the extension, or is named", {
  expect_equal(adgl:::driver_from_extension("a/b.tif", "raster"), "GTiff")
  expect_equal(adgl:::driver_from_extension("a/b.VRT", "raster"), "VRT")
  expect_equal(adgl:::driver_from_extension("a/b.gpkg", "vector"), "GPKG")
  expect_equal(adgl:::driver_from_extension("a/b.parquet", "vector"), "Parquet")
  expect_error(adgl:::driver_from_extension("a/b.xyzzy", "raster"),
               "name one with")
})

test_that("a vector round-trips with its geometry and fields", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)

  write_to(query(v, where = "population > 1e6"), path)
  back <- collect(src(path))

  expect_equal(nrow(back), 3L)
  expect_equal(sum(vapply(back, inherits, logical(1), "wk_wkb")), 1L)
})

test_that("a warped plan can be written straight out", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)
  path <- tempfile(fileext = ".tif")
  on.exit(unlink(path), add = TRUE)

  write_to(warp(query(x, extent = c(100, 160, -60, -20)), "EPSG:3031",
                dim = c(32, 32)), path)
  back <- src(path)
  on.exit(src_close(back), add = TRUE)

  expect_equal(S7::prop(back, "plan")$source_dimension, c(32L, 32L))
  expect_equal(adgl:::crs_label(S7::prop(back, "dataset")@crs), "EPSG:3031")
})

test_that("the container type is wide enough for every band written", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_equal(adgl:::mem_type(S7::prop(x, "dataset"), 1:2), "Int16")
})
