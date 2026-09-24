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

test_that("a nodata value comes back as missing, not as a measurement", {
  path <- nodata_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  g <- collect(x)
  # The fill is the first two cells of the first row, and the rest is 3:16.
  expect_equal(as.vector(t(g$data))[1:2], c(NA_real_, NA_real_))
  expect_equal(as.vector(t(g$data))[3:16], as.double(3:16))
  expect_false(any(g$data == -32768, na.rm = TRUE))

  # The same on the gis path, which is a flat vector rather than an array.
  expect_equal(sum(is.na(collect(x, as = "gis"))), 2L)

  # And the value itself is still there for anyone who asks for it.
  expect_equal(as.vector(t(collect(x, mask = FALSE)$data))[1:2],
               c(-32768, -32768))
})

test_that("masking pivots on type rather than pretending", {
  path <- nodata_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  # A narrow type has no missing value to put there, so it does not mask by
  # default and will not be talked into it.
  expect_equal(collect(x, type = "integer")$data[1L], -32768L)
  expect_error(collect(x, type = "integer", mask = TRUE), "type = \"double\"")
  expect_error(collect(x, type = "raw", mask = TRUE), "type = \"double\"")
  expect_error(collect(x, mask = NA), "TRUE or FALSE")
})

test_that("a band that declares no nodata value is left alone", {
  path <- byte_tif()
  on.exit(unlink(path), add = TRUE)
  x <- src(path)
  on.exit(src_close(x), add = TRUE)

  expect_false(anyNA(collect(x)$data))
  expect_equal(as.vector(t(collect(x)$data)), as.double(seq_len(16) - 1))
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

test_that("a padded read puts the source where it belongs and NA elsewhere", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  # Two 18-degree pixels past the north-west corner in each direction, so the
  # source occupies the bottom-right quarter of a 4 by 4 grid.
  g <- collect(query(x, extent = c(-216, -144, 54, 126), pad = TRUE))
  expect_equal(dim(g$data), c(4L, 4L, 2L))
  expect_true(all(is.na(g$data[1:2, , ])))
  expect_true(all(is.na(g$data[, 1:2, ])))
  expect_false(anyNA(g$data[3:4, 3:4, ]))

  # The bbox is the whole rectangle asked for, not the part that exists.
  expect_equal(as.vector(unlist(unclass(g$bbox))), c(-216, 54, -144, 126))

  # A window with nothing in it at all is legal once pad says so.
  outside <- collect(query(x, extent = c(400, 500, 200, 300), pad = TRUE))
  expect_true(all(is.na(outside$data)))

  # And the gis path pads the same way.
  expect_equal(sum(!is.na(collect(query(x, extent = c(-216, -144, 54, 126),
                                        pad = TRUE), as = "gis"))), 8L)
})

test_that("padding needs a type that has a missing value", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)
  p <- query(x, extent = c(-216, -144, 54, 126), pad = TRUE)
  expect_error(collect(p, type = "integer"), "pad = TRUE needs")
  expect_error(collect(p, type = "raw"), "pad = TRUE needs")
})

test_that("a padded read survives being resampled to a different size", {
  x <- src(test_tif())
  on.exit(src_close(x), add = TRUE)

  g <- collect(query(x, extent = c(-216, -144, 54, 126), pad = TRUE,
                     dim = c(8, 8)))
  expect_equal(dim(g$data), c(8L, 8L, 2L))
  # The source is a quarter of the rectangle, so a quarter of the output.
  expect_equal(sum(!is.na(g$data[, , 1])), 16L)
  expect_true(all(is.na(g$data[1:4, , 1])))
  expect_false(anyNA(g$data[5:8, 5:8, 1]))
})

test_that("the id and the geometry are fid and geom whatever the driver", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  from_gpkg <- names(collect(v))

  shp <- tempfile(fileext = ".shp")
  write_to(v, shp)
  s <- src(shp)
  on.exit(src_close(s), add = TRUE)
  expect_equal(S7::prop(s, "info")$fid_column, "OGC_FID")
  expect_equal(S7::prop(s, "info")$geometry_column, "wkb_geometry")
  expect_setequal(names(collect(s)), from_gpkg)
  expect_s3_class(collect(s)$geom, "wk_wkb")
  expect_true(any(grepl("fid from OGC_FID, geom from wkb_geometry",
                        utils::capture.output(print(s)))))

  # An SQL result names the id OGC_FID even over a GeoPackage.
  q <- src(test_gpkg(), sql = "SELECT name, geom FROM places")
  on.exit(src_close(q), add = TRUE)
  expect_true(all(c("fid", "geom") %in% names(collect(q))))
})

test_that("an attribute that already has the standard name stops the read", {
  path <- tempfile(fileext = ".geojson")
  GDAL7::write_vector(
    data.frame(geom = c("a", "b"), value = 1:2), path, driver = "GeoJSON")
  x <- src(path)
  on.exit(src_close(x), add = TRUE)
  expect_error(collect(x), "attribute called 'geom'")
})

test_that("as = \"arrow\" hands back the plan as an unread stream", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  plan <- query(v, where = "population > 1e6", fields = "name", limit = 2)

  stream <- collect(plan, as = "arrow")
  expect_s3_class(stream, "nanoarrow_array_stream")
  schema <- nanoarrow::infer_nanoarrow_schema(stream)
  expect_identical(names(schema$children), c("fid", "name", "geom"))
  # The CRS travels with the geometry, in GeoArrow's own metadata.
  expect_match(schema$children$geom$metadata[["ARROW:extension:name"]], "geoarrow.wkb")
  expect_match(schema$children$geom$metadata[["ARROW:extension:metadata"]], "crs")

  d <- suppressWarnings(nanoarrow::convert_array_stream(stream))
  GDAL7::release_arrow_stream(stream)
  expect_equal(nrow(d), 2L)
  expect_equal(d$name, collect(plan)$name)
})

test_that("a stream of a reprojected plan is refused, since that part is in R", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  expect_error(collect(query(v, crs = "EPSG:3857"), as = "arrow"), "PROJ")
})

test_that("fields and limit are done in GDAL, and do not stick to the layer", {
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  expect_equal(names(collect(query(v, fields = "name", limit = 1))),
               c("fid", "name", "geom"))
  lyr <- GDAL7::get_layer(S7::prop(v, "dataset"), 1)
  expect_setequal(lyr@ignored_fields, c("population", "elevation"))
  full <- collect(v)
  expect_equal(nrow(full), 5L)
  expect_true(all(c("population", "elevation") %in% names(full)))
})
