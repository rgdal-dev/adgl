# adgl 0.0.0.9000

First working version, built to the design in the project's own design
document rather than grown by accretion.

* `src()` opens a GDAL source and holds a plan over it rather than data. The
  source is probed once at open, so the object knows its own shape and its own
  deficiencies without reading anything. `options` and `drivers` pass straight
  to GDAL, which is the non-virtualisation way to tell a source what its
  metadata does not say.

* `src(sql = )` plans over the result of an SQL `SELECT` instead of a layer,
  so a join, an aggregate or a computed column is a source like any other,
  and `query()`'s `where`, `extent`, `fields` and `limit` narrow the result.
  `dialect = "SQLITE"` gives GDAL's SQLite dialect against any source. The
  statement is run again at each read, since the plan holds no GDAL object.
  Separately, a vector read now clears the filters an earlier read left on
  the layer, which could narrow a later read that asked for no filter.

* `collect(as = "arrow")` hands back a vector plan as an unread Arrow stream
  (a `nanoarrow_array_stream`) for arrow, duckdb, geoarrow or anything else
  that takes one, with the CRS in the geometry column's GeoArrow metadata.
  To make that possible the whole plan is now carried out in GDAL: `fields`
  become fields the driver is told not to read, which saves I/O and not only
  memory, and `limit` stops the read rather than trimming it. `fields` now
  keeps `fid` alongside the geometry. `query(crs = )` is still done in R, so
  a stream of a reprojected plan is an error.

* A vector read returns the feature id as `fid` and the geometry as `geom`
  whatever the driver called them, so a shapefile's `OGC_FID` and
  `wkb_geometry` come back under the same names as a GeoPackage's. The names
  come from GDAL rather than being guessed, the source prints the renaming,
  and an attribute that already has one of the names, such as the `fid` QGIS
  writes into a shapefile, comes back as `fid_1`. `where` still uses GDAL's SQL, in which `fid`
  is the id on every driver. `write_to(geometry_name = , fid_name = )` names
  both in the written layer, where the format stores them as columns.

* `query()` narrows a plan and reads nothing. `extent` and `crs` mean the same
  thing for both kinds of source; `bands`, `dim` and `resample` are raster
  only and `where`, `fields` and `limit` are vector only, and passing one to
  the wrong kind is an error rather than a silent no-op. A raster extent snaps
  outward to whole source pixels, so a query with no `dim` returns the
  source's own values.

* `report()` says what a source's metadata is missing, what that will cost,
  and the exact text that would fix it, and nothing in the package ever
  applies that text. A finding that would make the result a lie rather than a
  degraded truth, such as a rotated geotransform that neither in-memory form
  can hold, stops the terminal verbs.

* `collect()` reads, into a `wk` grid by default or into the flat `gis` shape
  that `ximage` draws directly, and into `double`, `integer` or `raw`. A
  vector collect is a tibble with a `wk` WKB column carrying its CRS, and
  `query(crs = )` reprojects those coordinates on the way out through
  `PROJ::proj_trans()`, which is one pass over the geometry.

* `as_grd()` returns a grid whose data is a proxy, so `plot()` and every other
  `wk` grid verb becomes a windowed GDAL read at the size actually asked for.
  That is what makes plotting a huge source cost the device rather than the
  file.

* `warp()` reprojects a raster, separately from `query()` because resampling
  is a choice the caller should make. Its target extent comes from the union
  of GDAL's boundary walk and an interior mesh, so it does not clip around a
  projection's interior singularities. Three arguments shape the output grid
  and any one of them is enough: `dim`, which takes a zero in either position
  and lets GDAL derive that side from the target extent's own aspect ratio;
  `resolution`, one number for square pixels or two; and `extent`, the window
  the output covers, which is the one argument here given in the target CRS.
  The warper's order statistics, `min`, `max`, `med`, `q1` and `q3`, are
  accepted as resampling methods as well. When nothing pins the target
  window, the extent GDAL chooses is measured once the warp has run, by
  inverting its corners and comparing the distance it claims against the
  distance it covers on the ground. A whole-globe source into a polar
  projection comes back 8e23 m across with all four corners at the same pole;
  that is now a warning with `dim` and an error with `resolution`, instead of
  a silent answer and GDAL's bare "too large output raster size". Where GDAL
  clamps for itself nothing is said.

* `write_to()` sends a plan to any format GDAL can write, taking the driver
  from the extension. A warped plan compiles into GDAL's own pipeline and
  never enters R.

* Three things decide what a rectangle means against a raster, and they are
  independent rather than modes. A position in `query(extent = )` may be left
  unsaid, as `NA` or as an infinity, and then means the bound the plan already
  has, so `c(NA, 150, NA, NA)` is everything west of 150. `snap` says where
  the rectangle lands on the source's own pixel edges, `"out"` as before,
  `"near"` or `"in"`. And `pad = TRUE` lets the rectangle leave the source,
  filling the outside with `NA` rather than intersecting; that is rasterio's
  `boundless` and terra's `extend`, it pads by whole source pixels so the grid
  does not move, it needs `type = "double"` for the same reason masking does,
  and it cannot be combined with `warp()`, whose warper reads from GDAL and
  never sees the pad. The lazy grid pads as well, so a padded plot is one
  windowed read like any other.

  The fourth case belongs to `warp()` rather than to an argument. Landing a
  window between two pixels is a resampling, because GDAL reads at an integer
  pixel offset, and `warp(extent = )` already honours a rectangle exactly. So
  `query()` moves the window to the data and `warp()` moves the data to the
  window.

* `warp(dim = )` accepts `NA` as well as `0` for the side GDAL should work
  out. They mean one thing; `0` is GDAL's own spelling in `--size` and is free
  there because no dimension is ever 0, while an extent needs `NA` because 0
  is a perfectly good coordinate.

* A band's nodata value is a fill rather than a measurement, so `collect()`
  and `as_grd()` return it as `NA` by default, which is what terra and stars
  do and what rasterio calls `masked`. The decision pivots on `type`, because
  only a double has a missing value to put there: masking is on for
  `type = "double"` and off for `"integer"` and `"raw"`, and asking for it
  with either of those is an error rather than a pretence. `mask = FALSE`
  gives the raw values back. Masking is exact, so a resampled read that
  averages a fill with its neighbours no longer matches the nodata value and
  survives as a number; that is the same everywhere and is why
  `resample = "nearest"` is the default.
