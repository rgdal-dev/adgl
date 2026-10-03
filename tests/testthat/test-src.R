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

test_that("sql = plans over the statement's result instead of a layer", {
  v <- src(test_gpkg(),
           sql = "SELECT name, population / 1e6 AS millions, geom FROM places")
  on.exit(src_close(v), add = TRUE)
  expect_s3_class(v, "adgl::vector_source")

  d <- collect(v)
  expect_equal(nrow(d), 5L)
  expect_true(all(c("name", "millions") %in% names(d)))
  expect_false("population" %in% names(d))
  expect_equal(sum(vapply(d, inherits, logical(1), "wk_wkb")), 1L)
})

test_that("query() narrows an SQL source the way it narrows a layer", {
  v <- src(test_gpkg(), sql = "SELECT * FROM places WHERE population > 1e6")
  on.exit(src_close(v), add = TRUE)
  all <- collect(v)
  expect_equal(nrow(all), 3L)

  expect_equal(nrow(collect(query(v, where = "name = 'Sydney'"))), 1L)
  expect_equal(nrow(collect(query(v, limit = 2))), 2L)
  expect_equal(names(collect(query(v, fields = "name"))), c("fid", "name", "geom"))
  expect_lt(nrow(collect(query(v, extent = c(140, 155, -45, -30)))), 3L)

  # The result set is run afresh at each read, so nothing an earlier read
  # narrowed carries into this one.
  expect_equal(nrow(collect(v)), 3L)
})

test_that("the SQLite dialect is available against any source", {
  v <- src(test_gpkg(), dialect = "SQLITE",
           sql = "SELECT count(*) AS n FROM places")
  on.exit(src_close(v), add = TRUE)
  d <- collect(v)
  expect_equal(nrow(d), 1L)
  expect_equal(as.numeric(d$n), 5)
  out <- utils::capture.output(print(v))
  expect_true(any(grepl("SQLITE", out)))
})

test_that("sql and layer cannot both be given, and a bad statement stops", {
  expect_error(src(test_gpkg(), layer = 1, sql = "SELECT * FROM places"),
               "give one")
  expect_error(src(test_gpkg(), dialect = "SQLITE"), "there is no `sql`")
  expect_error(src(test_gpkg(), sql = c("a", "b")), "single")
  expect_error(suppressWarnings(src(test_gpkg(), sql = "SELECT * FROM nowhere")))
})

test_that("a result with no geometry says so rather than ignoring a rectangle", {
  # The geometry is a column like any other in SQL, so leaving it out of the
  # select list is easy, and GDAL's answer to a spatial filter on the result
  # is a warning and every row.
  v <- src(test_gpkg(), sql = "SELECT name FROM places")
  on.exit(src_close(v), add = TRUE)
  expect_equal(S7::prop(v, "info")$geometry_type, "None")
  expect_equal(nrow(collect(v)), 5L)
  expect_error(query(v, extent = c(140, 155, -45, -30)), "select the geometry")
  expect_error(query(v, crs = "EPSG:3857"), "needs a geometry")
  expect_false("no_crs" %in% report(v)$check)
})

test_that("a read does not inherit a filter left on the layer by the last one", {
  # GDAL keeps a filter on the layer object and returns the same object for
  # the same layer, so the order of these two reads used to matter.
  v <- src(test_gpkg())
  on.exit(src_close(v), add = TRUE)
  some <- collect(query(v, where = "population > 1e6",
                        extent = c(140, 155, -45, -30)))
  expect_equal(nrow(collect(v)), 5L)
  expect_lt(nrow(some), 5L)
})
