test_that("a lazy grid reads the same values as an eager one", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  q <- query(x, bands = 1)

  lazy <- as_grd(q)
  eager <- collect(q)

  expect_equal(dim(lazy$data), c(10L, 20L, 1L))
  expect_equal(as.array(lazy$data)[, , 1], eager$data)
  expect_equal(unclass(lazy$bbox), unclass(eager$bbox))
})

test_that("subsetting the proxy is a windowed read, not a whole one", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  q <- query(x, bands = 1)

  whole <- collect(q)$data
  sub <- wk::grd_subset(as_grd(q), i = 2:4, j = 3:7)$data

  expect_equal(dim(sub), c(3L, 5L, 1L))
  expect_equal(sub[, , 1], whole[2:4, 3:7])
})

test_that("an index off the edge comes back padded rather than short", {
  # wk censors out-of-range indices to NA, so the proxy has to place the
  # in-bounds read into an NA-filled array of the shape that was asked for.
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  proxy <- adgl:::new_proxy(query(x, bands = 1))

  out <- proxy[c(NA, 1L, 2L), c(1L, NA), drop = FALSE]
  expect_equal(dim(out), c(3L, 2L, 1L))
  expect_true(all(is.na(out[1L, , 1L])))
  expect_true(all(is.na(out[, 2L, 1L])))
  expect_false(is.na(out[2L, 1L, 1L]))
})

test_that("a step greater than one is one read at the output size", {
  x <- src(test_cog())
  on.exit(src_close(x), add = TRUE)

  # This is what grd_crop() produces for a device smaller than the raster,
  # and it is the whole reason the proxy exists.
  out <- adgl:::new_proxy(x)[seq(1, 256, by = 8), seq(1, 512, by = 8),
                             drop = FALSE]
  expect_equal(dim(out), c(32L, 64L, 1L))
  expect_false(anyNA(out))
})

test_that("arbitrary indices still work, by reading the bounding window", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  q <- query(x, bands = 1)

  whole <- collect(q)$data
  irregular <- c(1L, 2L, 5L)
  out <- adgl:::new_proxy(q)[irregular, c(1L, 4L, 5L), drop = FALSE]
  expect_equal(out[, , 1], whole[irregular, c(1L, 4L, 5L)])
})

test_that("the proxy reports its own shape without reading", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  proxy <- adgl:::new_proxy(x)
  expect_equal(dim(proxy), c(10L, 20L, 2L))
  expect_match(utils::capture.output(print(proxy)), "unread")
})

test_that("a warped plan has no grid to be lazy over, and says so", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  expect_error(as_grd(warp(x, "EPSG:3857")), "cannot be lazy")
})
