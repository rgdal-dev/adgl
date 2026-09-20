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
