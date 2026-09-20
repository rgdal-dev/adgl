# Write a plan to a file

`write_to()` is the terminal verb that sends a plan to any format in
GDAL's suite rather than into R. The driver comes from the file
extension unless you name one.

## Usage

``` r
write_to(x, dsn, ...)
```

## Arguments

- x:

  A source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md), usually
  after
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md).

- dsn:

  Where to write it.

- ...:

  The named arguments below.

## Value

`dsn`, invisibly.

## Details

A warped plan compiles straight into GDAL's own reprojection pipeline
and never enters R at all, which is what you want for a large result.
Every other plan is read into memory and written back out, because the
read and the write are each one call and the intermediate is the size of
the result you asked for rather than the size of the source. If that
intermediate is too big, narrow the plan with `dim` first, or tile it
yourself.

## Arguments

- `driver`:

  The GDAL driver's short name. The default reads it from the extension
  of `dsn`.

- `options`:

  Character vector of `"KEY=VALUE"` creation options, such as
  `c("COMPRESS=ZSTD", "TILED=YES")`.

- `layer`:

  Vector only. The layer name to create. Defaults to the file's base
  name.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
path <- tempfile(fileext = ".tif")
write_to(query(x, dim = c(10, 5)), path)
src(path)
#> raster source GTiff  10 x 5 x 2
#>   extent -180, 180, -90, 90
#>   crs    EPSG:4326
#>   nothing missing
unlink(path)
```
