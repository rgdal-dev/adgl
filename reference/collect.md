# Read a plan into memory

`collect()` is the terminal verb that turns a plan into R data.
Everything before it is free; this is where the reading happens, in one
GDAL call for a raster and one Arrow stream for a vector.

## Usage

``` r
collect(x, ...)
```

## Arguments

- x:

  A source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md), usually
  after
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md).

- ...:

  The named arguments below.

## Value

For a raster, a `wk_grd_rct` or a vector carrying a `"gis"` attribute.
For a vector, a tibble.

## Details

For a raster, `as` picks the in-memory form:

- `"grd"`, the default, is
  [`wk::grd_rct()`](https://paleolimbot.github.io/wk/reference/grd.html):
  an array with `dim = c(ny, nx, nbands)`, y decreasing down the rows,
  in a bounding rectangle that carries the CRS. It costs one transpose
  per band, because GDAL's x-fastest order is the transpose of what R's
  [`matrix()`](https://rdrr.io/r/base/matrix.html) builds, and it buys
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  `grd_crop()`, `grd_tile()` and the rest of wk's grid vocabulary.

- `"gis"` is the shape `gdalraster::read_ds()` returns: one flat atomic
  vector, band after band, x varying fastest within a band, carrying a
  `"gis"` attribute of `type`, `bbox`, `dim`, `srs` and `datatype`. It
  is the zero-transpose path, and
  [`ximage::ximage()`](https://rdrr.io/pkg/ximage/man/ximage.html) draws
  it directly.

`type` is the R type the values are read into. `"raw"` is one byte a
value and is what an RGB image wants; `"integer"` is four and covers
Byte, Int8, Int16, UInt16 and Int32. A band whose type will not fit is
an error rather than a silent clamp. The `"grd"` form is built from
whatever comes back, so a raw read stays a raw array.

`mask` decides what happens to a value the source itself declares
absent. A band's nodata value is a fill, not a measurement, so by
default it comes back as `NA` rather than as the number that stands for
it. This is what terra and stars do, and what rasterio calls `masked`.
It pivots on `type`, because only a double has a missing value to put
there: `mask` defaults to `TRUE` for `"double"` and to `FALSE` for
`"integer"` and `"raw"`, and asking for it with either of those is an
error rather than a pretence.

Masking is exact rather than approximate, and that matters when the read
is resampled. A nearest read returns source values unchanged, so every
fill pixel is caught. An averaging or bilinear read blends a fill with
its neighbours, and the blend is no longer equal to the nodata value, so
it survives as a number. That is the same in every package that does
this, and it is why `resample = "nearest"` is the default.

For a vector, the result is a tibble whose geometry column is
[`wk::wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html) with
its CRS set. The column keeps whatever name GDAL gave it, because wk
finds a geometry column by asking rather than by name, so `wk_bbox()`,
`wk_plot()` and the chunked handlers all work on the result unchanged.
When the plan carries a `crs` from
[`query()`](https://rgdal-dev.github.io/adgl/reference/query.md), the
coordinates are transformed with
[`PROJ::proj_trans()`](https://hypertidy.github.io/PROJ/reference/proj_trans.html)
on the way out, which is one pass over the geometry and nothing else.

## Arguments

- `as`:

  Raster only. `"grd"` (the default) or `"gis"`.

- `type`:

  Raster only. `"double"` (the default), `"integer"` or `"raw"`.

- `mask`:

  Raster only. Return each band's nodata value as `NA`. Defaults to
  `TRUE` for `type = "double"` and `FALSE` otherwise.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
g <- collect(query(x, dim = c(10, 5)))
dim(g$data)
#> [1]  5 10  2

v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
collect(query(v, where = "population > 1e6"))
#> # A tibble: 3 × 5
#>     fid name      population elevation geom                       
#>   <dbl> <chr>          <dbl>     <dbl> <wk_wkb>                   
#> 1     2 Melbourne    5031195        31 <POINT (144.9631 -37.8136)>
#> 2     3 Sydney       5312163        19 <POINT (151.2093 -33.8688)>
#> 3     4 Perth        2141834        15 <POINT (115.8605 -31.9523)>
```
