test_that("warp() records the reprojection and reads nothing", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  w <- warp(x, "EPSG:3857", resample = "bilinear")
  plan <- S7::prop(w, "plan")
  expect_equal(plan$warp$crs, "EPSG:3857")
  expect_equal(plan$warp$resample, "bilinear")
  # The source plan is untouched: nothing has been reprojected yet.
  expect_null(S7::prop(x, "plan")$warp)
})

test_that("collecting a warped plan gives the target CRS", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  g <- collect(warp(query(x, extent = c(100, 160, -60, -20)), "EPSG:3031",
                    resample = "average", dim = c(32, 32)))
  expect_equal(dim(g$data), c(32L, 32L))
  expect_match(wk::wk_crs(g$bbox), "Antarctic", fixed = FALSE)

  # Metres, not degrees: the values moved, not just the label.
  bbox <- as.vector(unlist(unclass(g$bbox)))
  expect_gt(max(abs(bbox)), 1e5)
})

test_that("the target extent comes from the walk-and-mesh union", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  q <- warp(query(x, extent = c(100, 160, -60, -20)), "EPSG:3031")
  args <- adgl:::warp_args(q)
  expected <- GDAL7::transform_extent(
    unname(extent_to_bbox(S7::prop(q, "plan")$extent)),
    S7::prop(q, "dataset")@crs, "EPSG:3031"
  )
  expect_equal(args$bbox, as.double(unname(expected)))
})

test_that("a whole-source warp leaves the target extent to GDAL", {
  # A global longlat extent taken literally into Web Mercator reaches the
  # poles, where the northing has no finite value; GDAL clamps and we should
  # let it.
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  args <- adgl:::warp_args(warp(x, "EPSG:3857"))
  expect_null(args$bbox)
})

test_that("warp is a raster verb and says so on a vector", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  expect_error(warp(v, "EPSG:3857"), "raster verb")
})

test_that("the warper's own resampling list is the one checked", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  # "sum" is a warper method and not a RasterIO one; "gauss" is the reverse.
  expect_error(warp(x, "EPSG:3857", resample = "sum"), NA)
  expect_error(warp(x, "EPSG:3857", resample = "gauss"), "one of")
  expect_error(warp(x, 3857), "single, non-missing string")
})

test_that("dim takes a zero and GDAL fills it in", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  expect_equal(S7::prop(warp(x, "EPSG:3857", dim = c(400, 0)), "plan")$warp$dim,
               c(400L, 0L))
  expect_equal(S7::prop(warp(x, "EPSG:3857", dim = c(0, 400)), "plan")$warp$dim,
               c(0L, 400L))

  expect_error(warp(x, "EPSG:3857", dim = c(0, 0)), "dim = NULL")
  expect_error(warp(x, "EPSG:3857", dim = c(-1, 10)), "neither negative")
  expect_error(warp(x, "EPSG:3857", dim = 400), "two whole numbers")
})

test_that("a zero in dim really is derived from the target extent", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  # A whole-source warp into Web Mercator draws a clamping warning out of
  # GDAL, which is the right thing for it to say and not what is under test.
  g <- suppressWarnings(collect(warp(x, "EPSG:3857", dim = c(64, 0))))
  expect_equal(dim(g$data)[2L], 64L)
  expect_gt(dim(g$data)[1L], 1L)

  h <- suppressWarnings(collect(warp(x, "EPSG:3857", dim = c(0, 64))))
  expect_equal(dim(h$data)[1L], 64L)
})

test_that("resolution sets the pixel size and refuses a dim beside it", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  w <- warp(x, "EPSG:3857", resolution = 50000)
  expect_equal(S7::prop(w, "plan")$warp$resolution, c(50000, 50000))
  expect_equal(adgl:::warp_args(w)$resolution, c(50000, 50000))
  expect_null(adgl:::warp_args(w)$size)

  expect_equal(S7::prop(warp(x, "EPSG:3857", resolution = c(10, 20)),
                        "plan")$warp$resolution, c(10, 20))

  expect_error(warp(x, "EPSG:3857", dim = c(10, 10), resolution = 100),
               "not both")
  expect_error(warp(x, "EPSG:3857", resolution = 0), "positive number")
  expect_error(warp(query(x, dim = c(5, 5)), "EPSG:3857", resolution = 100),
               "query\\(dim = \\)")
})

test_that("resolution decides the size of the warped grid", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  coarse <- suppressWarnings(collect(warp(x, "EPSG:3857", resolution = 4e6)))
  fine <- suppressWarnings(collect(warp(x, "EPSG:3857", resolution = 2e6)))
  expect_gt(dim(fine$data)[1L], dim(coarse$data)[1L])
})

test_that("warp(extent =) is the target window, in the target CRS", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  bounds <- c(-2e6, 2e6, -3e6, 1e6)
  w <- warp(x, "EPSG:3857", extent = bounds)
  # Straight through, untransformed, in bbox order.
  expect_equal(adgl:::warp_args(w)$bbox, c(-2e6, -3e6, 2e6, 1e6))

  expect_error(warp(x, "EPSG:3857", extent = c(1, 0, 1, 0)), "no area")
  # The query extent has to be one that really narrows: test.tif's pixels are
  # 18 degrees wide, so c(-170, 170, -80, 80) snaps back out to the whole
  # source and leaves nothing for warp(extent = ) to clash with.
  expect_error(
    warp(query(x, extent = c(0, 90, 0, 54)), "EPSG:3857", extent = bounds),
    "one or the other"
  )
})

test_that("a warp extent lands on the output", {
  skip_if_not(GDAL7::gdal_has_algorithms(),
              "this GDAL has no algorithm registry")
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  bounds <- c(-1e7, 1e7, -5e6, 5e6)
  g <- collect(warp(x, "EPSG:3857", extent = bounds, dim = c(40, 20)))
  got <- as.vector(unlist(unclass(g$bbox)))
  expect_equal(got, c(-1e7, -5e6, 1e7, 5e6), tolerance = 1e-6)
})

test_that("the warper's order statistics are accepted", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  for (method in c("min", "max", "med", "q1", "q3")) {
    expect_equal(S7::prop(warp(x, "EPSG:3857", resample = method),
                          "plan")$warp$resample, method)
  }
})
