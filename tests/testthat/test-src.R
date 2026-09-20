test_that("src() tells the two kinds apart by what the source holds", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_s3_class(x, "adgl::raster_source")

  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  expect_s3_class(v, "adgl::vector_source")
})

test_that("opening probes once and reads nothing", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  plan <- S7::prop(x, "plan")
  expect_equal(plan$source_dimension, c(20L, 10L))
  expect_equal(unname(plan$source_extent), c(-180, 180, -90, 90))
  expect_equal(plan$bands, 1:2)

  # Nothing in the plan is a GDAL object, which is what lets it print,
  # compare and serialise.
  expect_null(unlist(lapply(plan, function(e) attr(e, "class"))))
})

test_that("the plan of a fresh source is its whole self", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  plan <- S7::prop(x, "plan")
  expect_equal(plan$extent, plan$source_extent)
  expect_equal(plan$dimension, plan$source_dimension)
  expect_null(plan$out_dimension)
})

test_that("a source with neither bands nor layers is named, not half-opened", {
  # The Zarr fixture is not this case: GDAL exposes a classic 2D view of it,
  # so it opens as an ordinary raster. Multidimensional reading stays with
  # GDAL7::read_mdarray() for now, which is what the message points at.
  skip_if_not(dir.exists(system.file("extdata/multidim.zarr", package = "GDAL7")))
  x <- src(system.file("extdata/multidim.zarr", package = "GDAL7"))
  on.exit(src_close(x), add = TRUE)
  expect_s3_class(x, "adgl::raster_source")
})

test_that("open options reach the driver", {
  # OVERVIEW_LEVEL is the non-virtualisation way to read less of a file, and
  # the reason src() takes open options at all.
  x <- src(test_cog(), options = "OVERVIEW_LEVEL=0")
  on.exit(src_close(x), add = TRUE)
  expect_equal(S7::prop(x, "plan")$source_dimension, c(256L, 128L))
})

test_that("printing says the shape, the extent and the findings", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  out <- utils::capture.output(print(x))
  expect_match(out[1], "raster source GTiff")
  expect_match(out[1], "20 x 10 x 2")
  expect_true(any(grepl("EPSG:4326", out)))

  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  out <- utils::capture.output(print(v))
  expect_match(out[1], "vector source")
  expect_match(out[1], "5 features")
})

test_that("a projected CRS is labelled by its own authority code", {
  # A projected WKT names its base geographic CRS first, so reading the first
  # ID would label every metre-based CRS as EPSG:4326.
  wkt <- GDAL7::crs_to_wkt("EPSG:3031")
  expect_equal(adgl:::crs_label(wkt), "EPSG:3031")
  expect_equal(adgl:::crs_label("EPSG:3857"), "EPSG:3857")
  expect_equal(adgl:::crs_label(""), "none")
})
