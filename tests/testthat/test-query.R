test_that("query() narrows the plan and reads nothing", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  q <- query(x, bands = 1, resample = "average", dim = c(4, 2))
  plan <- S7::prop(q, "plan")
  expect_equal(plan$bands, 1L)
  expect_equal(plan$resample, "average")
  expect_equal(plan$out_dimension, c(4L, 2L))

  # The original is untouched, because the plan is data rather than state.
  expect_equal(S7::prop(x, "plan")$bands, 1:2)
})

test_that("a chain of queries is the same as one call", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  chained <- query(query(x, extent = c(-100, 0, 0, 80)), dim = c(3, 3))
  once <- query(x, extent = c(-100, 0, 0, 80), dim = c(3, 3))
  expect_equal(S7::prop(chained, "plan"), S7::prop(once, "plan"))
})

test_that("a raster extent snaps out to whole source pixels", {
  # 20 x 10 over the globe is 18 degrees a pixel, so -100 snaps to -108 and
  # 80 snaps to 90. Snapping out rather than rounding is what makes a query
  # with no `dim` return the source's own values, untouched.
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  q <- query(x, extent = c(-100, 0, 0, 80))
  plan <- S7::prop(q, "plan")
  expect_equal(unname(plan$extent), c(-108, 0, 0, 90))
  expect_equal(plan$dimension, c(6L, 5L))
  expect_equal(adgl:::raster_window(plan), c(4, 0, 6, 5))
})

test_that("an extent outside the source is an error, not an empty read", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_error(query(x, extent = c(200, 300, 0, 10)), "does not overlap")
})

test_that("an extent in another CRS picks a window without warping", {
  # The finer fixture, so that snapping to whole pixels does not swamp the
  # thing being tested.
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  # Roughly Australia in Web Mercator, back onto a longlat source.
  q <- query(x, extent = c(1.2e7, 1.8e7, -5.5e6, -1e6), crs = "EPSG:3857")
  e <- S7::prop(q, "plan")$extent
  expect_gt(e[["xmin"]], 100)
  expect_lt(e[["xmax"]], 180)
  expect_lt(e[["ymax"]], 0)
  expect_gt(e[["ymin"]], -60)
  # No pixels moved: the plan still reads the source's own grid.
  expect_null(S7::prop(q, "plan")$warp)
})

test_that("crs without extent points at warp() instead of guessing", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_error(query(x, crs = "EPSG:3857"), "warp\\(\\)")
})

test_that("bands resolve by position and by description", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_equal(S7::prop(query(x, bands = 2), "plan")$bands, 2L)
  expect_error(query(x, bands = 5), "between 1 and 2")
  expect_error(query(x, bands = "no such band"), "described as")
})

test_that("an argument belonging to the other kind is an error", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  expect_error(query(x, where = "a = 1"), "no raster meaning")
  expect_error(query(x, fields = "name"), "analogue is `bands`")
  expect_error(query(v, dim = c(2, 2)), "no vector meaning")
  expect_error(query(v, resample = "average"), "nothing to resample")
  expect_error(query(x, spelt_wrong = 1), "unused argument")
})

test_that("vector narrowing composes by intersection and AND", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  q <- query(query(v, where = "population > 1e6"), where = "elevation > 20")
  expect_equal(S7::prop(q, "plan")$where,
               "(population > 1e6) AND (elevation > 20)")

  q2 <- query(query(v, extent = c(100, 150, -50, -10)),
              extent = c(120, 160, -45, -20))
  expect_equal(unname(S7::prop(q2, "plan")$extent), c(120, 150, -45, -20))

  expect_equal(S7::prop(query(query(v, limit = 5), limit = 2), "plan")$limit, 2L)
})

test_that("resample is checked against the methods GDAL has", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_error(query(x, resample = "nearset"), "one of")
  expect_error(query(x, dim = c(0, 2)), "positive whole numbers")
})
