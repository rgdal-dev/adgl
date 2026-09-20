test_that("a source that says everything has no findings", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_equal(nrow(report(x)), 0L)
  expect_named(report(x), c("check", "severity", "what", "consequence",
                            "fix_kind", "fix"))
})

test_that("a file with no georeferencing is caught", {
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  r <- report(x)
  expect_true("no_geotransform" %in% r$check)
  expect_equal(r$severity[r$check == "no_geotransform"], "blocks")

  # GDAL reports a missing CRS as NA here rather than as "", and nzchar(NA)
  # is TRUE, so the check has to ask properly or it decides a file with no
  # CRS has one.
  expect_true("no_crs" %in% r$check)
})

test_that("the identity geotransform is caught too, which is the quiet case", {
  # A driver that reports success and still leaves the raster at the origin
  # with one-unit pixels is the deficiency nothing downstream can see.
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  ds <- GDAL7::gdal_open(path, update = TRUE)
  ds@geotransform <- c(0, 1, 0, 0, 0, 1)
  GDAL7::gdal_close(ds)

  x <- src(path)
  on.exit(src_close(x), add = TRUE)
  r <- report(x)
  expect_true("identity_geotransform" %in% r$check)
  expect_equal(r$severity[r$check == "identity_geotransform"], "blocks")
})

test_that("the report says what would fix it and never applies the fix", {
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  r <- report(x)
  expect_true(all(nzchar(r$fix)))
  expect_true(all(r$fix_kind %in%
                    c("open_option", "vrt", "gdal_cli", "r_call")))

  # Reading the report changes nothing about the source.
  before <- S7::prop(x, "plan")
  report(x)
  expect_equal(S7::prop(x, "plan"), before)
})

test_that("a blocking finding stops the terminal verbs rather than lying", {
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  expect_error(collect(x), "will not run")
  expect_error(write_to(x, tempfile(fileext = ".tif")), "will not run")
  expect_error(as_grd(x), "will not run")
})

test_that("a degrading finding does not stop anything", {
  # No nodata is worth saying and not worth refusing over.
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)
  expect_true("no_nodata" %in% report(x)$check)
  expect_equal(report(x)$severity[report(x)$check == "no_nodata"], "degrades")
})

test_that("a vector layer is checked for what a query will cost", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  r <- report(v)
  expect_true(all(r$check %in% c("no_crs", "slow_spatial_filter",
                                 "unknown_feature_count",
                                 "unknown_geometry_type", "shapefile")))
})

test_that("printing a source counts its findings", {
  path <- bare_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("findings:", out)))
  expect_true(any(grepl("report\\(x\\)", out)))
})
