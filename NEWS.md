# adgl 0.0.0.9000

First working version, built to the design in the project's own design
document rather than grown by accretion.

* `src()` opens a GDAL source and holds a plan over it rather than data. The
  source is probed once at open, so the object knows its own shape and its own
  deficiencies without reading anything. `options` and `drivers` pass straight
  to GDAL, which is the non-virtualisation way to tell a source what its
  metadata does not say.

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
  accepted as resampling methods as well.

* `write_to()` sends a plan to any format GDAL can write, taking the driver
  from the extension. A warped plan compiles into GDAL's own pipeline and
  never enters R.
