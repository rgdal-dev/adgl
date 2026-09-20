# The source classes

The two classes
[`src()`](https://rgdal-dev.github.io/adgl/reference/src.md) returns.
They are exported so that another package can write methods for them;
build one with
[`src()`](https://rgdal-dev.github.io/adgl/reference/src.md) rather than
by calling these directly.

## Usage

``` r
raster_source(
  dsn = character(0),
  dataset = NULL,
  info = list(),
  plan = list(),
  findings = NULL
)

vector_source(
  dsn = character(0),
  dataset = NULL,
  info = list(),
  plan = list(),
  findings = NULL
)
```

## Arguments

- dsn:

  The GDAL connection string the source was opened from.

- dataset:

  The open
  [`GDAL7::GDALDataset`](https://rdrr.io/pkg/GDAL7/man/GDALDataset.html).

- info:

  What the one probe at open returned:
  [`GDAL7::gdal_info()`](https://rdrr.io/pkg/GDAL7/man/gdal_info.html)
  for a raster, the layer's own description for a vector.

- plan:

  The pending query, as a plain list. Nothing in it is a GDAL object,
  which is what lets it print, compare and serialise.

- findings:

  The deficiency report, computed once at open. Read it with
  [`report()`](https://rgdal-dev.github.io/adgl/reference/report.md).

## Value

A source object.

## Examples

``` r
S7::S7_inherits(src(system.file("extdata/test.tif", package = "GDAL7")),
                raster_source)
#> [1] TRUE
```
