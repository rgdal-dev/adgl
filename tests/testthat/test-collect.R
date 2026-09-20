test_that("the grd and the gis form hold the same values in different orders", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  q <- query(x, bands = 1, dim = c(5, 3))

  g <- collect(q, as = "grd")
  gi <- collect(q, as = "gis")

  # grd is array[ny, nx], y down the rows; gis is flat with x fastest. One is
  # the transpose of the other, and asserting that is the only way to be sure
  # neither has been quietly transposed.
  expect_equal(dim(g$data), c(3L, 5L))
  expect_length(gi, 15L)
  expect_equal(as.vector(t(g$data)), as.vector(gi))
})

test_that("the gis attribute is the contract gdalraster writes", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  gi <- collect(query(x, dim = c(5, 3)), as = "gis")
  a <- attr(gi, "gis")

  expect_named(a, c("type", "bbox", "dim", "srs", "datatype"))
  expect_equal(a$type, "raster")
  # The two orders, which is where this shape is easiest to get wrong.
  expect_equal(a$bbox, c(-180, -90, 180, 90))
  expect_equal(a$dim, c(5L, 3L, 2L))
  expect_equal(a$datatype, c("Int16", "Int16"))
  expect_true(nzchar(a$srs))
})

test_that("gis_attr() states the orders on its own", {
  a <- gis_attr(c(0, 0, 10, 20), c(10L, 20L, 3L), "", c("Byte", "Byte", "Byte"))
  expect_equal(a$bbox, c(0, 0, 10, 20))
  expect_equal(a$dim, c(10L, 20L, 3L))
  expect_type(a$dim, "integer")
})

test_that("the grd carries the extent its pixels really have", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  g <- collect(query(x, extent = c(-100, 0, 0, 80), bands = 1))
  # Snapped out, so the bbox is the pixel edges rather than what was asked.
  expect_equal(as.vector(unlist(unclass(g$bbox))), c(-108, 0, 0, 90))
  expect_equal(dim(g$data), c(5L, 6L))
})

test_that("the narrow read types come through", {
  path <- byte_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  expect_type(collect(x, type = "raw")$data, "raw")
  expect_type(collect(x, type = "integer")$data, "integer")
  expect_type(collect(x)$data, "double")
  expect_error(collect(src(test_tif()), type = "raw"), "Int16")
})

test_that("a vector collect is a tibble with a wk geometry column", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  d <- collect(v)
  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 5L)

  geom <- d[[which(vapply(d, inherits, logical(1), "wk_wkb"))]]
  expect_s3_class(geom, "wk_wkb")
  expect_false(is.na(wk::wk_crs(geom)[1]))

  # wk finds the geometry column by asking rather than by name, so the whole
  # handleable vocabulary works on the result with nothing added here.
  expect_s3_class(wk::wk_bbox(d), "wk_rct")
})

test_that("a where clause and an extent both narrow the vector read", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  expect_equal(nrow(collect(query(v, where = "population > 1e6"))), 3L)
  expect_lt(nrow(collect(query(v, extent = c(140, 160, -45, -30)))), 5L)
  expect_equal(nrow(collect(query(v, limit = 2))), 2L)
})

test_that("fields narrow the columns and keep the geometry", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  d <- collect(query(v, fields = "name"))
  expect_true("name" %in% names(d))
  expect_false("population" %in% names(d))
  expect_equal(sum(vapply(d, inherits, logical(1), "wk_wkb")), 1L)

  expect_error(collect(query(v, fields = "no_such_field")), "no field named")
})

test_that("a vector extent given in another CRS lands on the same features", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  # Australia, given once in longlat and once in Web Mercator.
  in_longlat <- collect(query(v, extent = c(110, 155, -45, -10)))
  in_mercator <- collect(query(
    v, extent = c(12244000, 17255000, -5621000, -1118000), crs = "EPSG:3857"
  ))
  expect_equal(nrow(in_longlat), nrow(in_mercator))

  # On a vector, crs is also the CRS the features come back in, which is the
  # one place the argument means more than it does on a raster.
  expect_match(wk::wk_crs(in_mercator[[ncol(in_mercator)]]), "3857",
               fixed = TRUE)
  expect_false(grepl("3857", wk::wk_crs(in_longlat[[ncol(in_longlat)]]),
                     fixed = TRUE))
})

test_that("query(crs =) reprojects the features on the way out", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  longlat <- collect(v)
  mercator <- collect(query(v, crs = "EPSG:3857"))
  geom <- function(d) d[[which(vapply(d, inherits, logical(1), "wk_wkb"))]]

  expect_equal(nrow(mercator), nrow(longlat))
  expect_s3_class(geom(mercator), "wk_wkb")
  expect_match(wk::wk_crs(geom(mercator)), "3857", fixed = TRUE)

  # Metres, not degrees: the coordinates moved, not just the label.
  before <- as.data.frame(wk::wk_coords(geom(longlat)))
  after <- as.data.frame(wk::wk_coords(geom(mercator)))
  expect_gt(max(abs(after$x)), 1e6)
  expect_lt(max(abs(before$x)), 360)

  # And the transform is the one PROJ would do on its own.
  expect_equal(after$x, as.data.frame(
    wk::wk_coords(PROJ::proj_trans(geom(longlat), "EPSG:3857")))$x)
})

test_that("an extent beside crs is read in that crs, and both apply", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)

  d <- collect(query(
    v, extent = c(12244000, 17255000, -5621000, -1118000), crs = "EPSG:3857"
  ))
  expect_gt(nrow(d), 0L)
  geom <- d[[which(vapply(d, inherits, logical(1), "wk_wkb"))]]
  expect_match(wk::wk_crs(geom), "3857", fixed = TRUE)
})

test_that("a layer with no CRS has nothing to reproject from", {
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)
  GDAL7::write_vector(
    GDAL7::read_vector(test_gpkg()), path, layer = "places", crs = NULL
  )
  v <- src(path)
  on.exit(src_close(v), add = TRUE)
  expect_error(query(v, crs = "EPSG:3857"), "nothing to reproject from")
})
