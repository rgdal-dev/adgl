test_that("the two extent orders round-trip", {
  e <- c(100, 160, -60, -20)
  expect_equal(unname(bbox_to_extent(extent_to_bbox(e))), e)
  expect_equal(names(extent_to_bbox(e)), c("xmin", "ymin", "xmax", "ymax"))
  expect_equal(names(bbox_to_extent(c(1, 2, 3, 4))),
               c("xmin", "xmax", "ymin", "ymax"))
})

test_that("the conversion actually moves the numbers", {
  # The whole point of having the pair: an extent handed to wk or to GDAL
  # unconverted is silently a different rectangle, not an error.
  expect_equal(unname(extent_to_bbox(c(100, 160, -60, -20))),
               c(100, -60, 160, -20))
})

test_that("a degenerate extent is refused rather than clamped", {
  expect_error(adgl:::check_extent(c(1, 1, 0, 2)), "no area")
  expect_error(adgl:::check_extent(c(0, 1, 2, 2)), "no area")
  expect_error(adgl:::check_extent(c(1, 2, 3)), "4 non-missing")
  expect_error(adgl:::check_extent(c(1, NA, 3, 4)), "4 non-missing")
})

test_that("extents intersect, and report no overlap as NULL", {
  a <- c(0, 10, 0, 10)
  expect_equal(unname(adgl:::intersect_extents(a, c(5, 20, 5, 20))),
               c(5, 10, 5, 10))
  expect_null(adgl:::intersect_extents(a, c(20, 30, 0, 10)))
  # Touching at an edge is not an overlap: the intersection has no area.
  expect_null(adgl:::intersect_extents(a, c(10, 20, 0, 10)))
})

test_that("a geotransform becomes an extent, y descending or not", {
  north_up <- c(-180, 18, 0, 90, 0, -18)
  expect_equal(unname(adgl:::gt_extent(north_up, c(20, 10))),
               c(-180, 180, -90, 90))

  # A south-up raster has a positive yres, and the extent is still ordered.
  south_up <- c(-180, 18, 0, -90, 0, 18)
  expect_equal(unname(adgl:::gt_extent(south_up, c(20, 10))),
               c(-180, 180, -90, 90))
})
