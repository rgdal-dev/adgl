# A lazy spatial source

`src()` opens a GDAL data source and returns a plan over it, never data.
The source is probed once at open, so the object knows its own shape and
what its metadata is missing, and nothing is read until
[`collect()`](https://rgdal-dev.github.io/adgl/reference/collect.md),
[`write_to()`](https://rgdal-dev.github.io/adgl/reference/write_to.md)
or [`plot()`](https://rdrr.io/r/graphics/plot.default.html) asks for a
result.

## Usage

``` r
src(
  dsn,
  options = NULL,
  drivers = NULL,
  layer = 1L,
  sql = NULL,
  dialect = NULL
)
```

## Arguments

- dsn:

  A GDAL connection string: a path, or a `/vsicurl/...`, `/vsis3/...`,
  `GPKG:...` or any other form GDAL understands.

- options:

  Character vector of `"KEY=VALUE"` open options, or `NULL` for none.

- drivers:

  Character vector of driver short names that may open the source, or
  `NULL` to place no restriction.

- layer:

  For a vector source, the layer to plan over: a name, or a one-based
  position. Ignored for a raster.

- sql:

  An SQL `SELECT` whose result is the layer to plan over, in place of
  `layer`. The statement is run once here, to learn the result's shape,
  and again at each read;
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md) then
  narrows its result as it would any layer's.

- dialect:

  The SQL dialect `sql` is written in: `NULL` for the driver's own,
  `"OGRSQL"` for GDAL's built-in one, or `"SQLITE"` for GDAL's SQLite
  dialect, which works against any source and has joins, aggregates and
  the SpatiaLite functions.

## Value

A `raster_source` or a `vector_source`.

## Details

`sql` is for what a layer name cannot say: a join, an aggregate, a
computed column, a subset of columns renamed on the way out. It stands
in for `layer` rather than being a
[`query()`](https://rgdal-dev.github.io/adgl/reference/query.md)
argument, because it decides what the source *is*; `where`, `extent`,
`fields` and `limit` are narrowings of it, and compose with it the way
they compose with each other. A statement that returns no rows (an
`UPDATE`, a `DELETE`) is refused: `src()` reads.

`options` is the non-virtualisation way to fix a deficient source, which
is why it is here and why
[`report()`](https://rgdal-dev.github.io/adgl/reference/report.md)
points at it: a CSV's `"X_POSSIBLE_NAMES=lon"`, a raster's
`"OVERVIEW_LEVEL=2"`. It is handed straight to GDAL. `drivers` restricts
which drivers may try the file, so a second driver cannot claim one the
first should have had.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
x
#> raster source GTiff  20 x 10 x 2
#>   extent -180, 180, -90, 90
#>   crs    EPSG:4326
#>   nothing missing

v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
v
#> vector source places  5 features  Point
#>   extent 115.8605, 151.2093, -42.8826, -12.4634
#>   crs    WGS 84
#>   nothing missing

src(system.file("extdata/test.gpkg", package = "GDAL7"),
    sql = "SELECT name, population / 1e6 AS millions FROM places")
#> vector source SELECT  5 features  None
#>   crs    none
#>   names  fid from OGC_FID
#>   sql    SELECT name, population / 1e6 AS millions FROM places
#>   nothing missing
```
