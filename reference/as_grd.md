# A raster source as a lazy grid

`as_grd()` returns a
[`wk::grd_rct()`](https://paleolimbot.github.io/wk/reference/grd.html)
whose data is a proxy rather than an array. Nothing is read until
something subsets it, and every subset becomes one windowed GDAL read at
exactly the size asked for, which GDAL serves from whichever overview
fits.

## Usage

``` r
as_grd(x, mask = TRUE)
```

## Arguments

- x:

  A raster source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md), usually
  after
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md).

- mask:

  Return each band's nodata value as `NA`. `TRUE` by default.

## Value

A `wk_grd_rct` whose `data` reads on demand.

## Details

That is what makes
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) cheap. `wk`'s
own [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
reads the device, works out a step, and crops the grid to it, so a 30000
by 30000 source drawn into an 800 by 600 device reads about 800 by 600
pixels. `grd_crop()`, `grd_extend()`, `grd_tile()` and `grd_subset()`
all work the same way.

Two details are worth knowing. wk censors out-of-range indices to `NA`,
so a request that runs off the edge of the raster comes back padded with
`NA` rather than short. And `[` is a general interface: when the indices
are an arithmetic sequence, which is all `grd_crop()` ever produces, the
read is one `RasterIO` call at the output size; when they are not, the
proxy reads the bounding window at full resolution and subsets in R, and
says so when that window is large.

Every read through the proxy is a double, so `mask` needs no `type`
pivot here: a band's nodata value comes back as `NA` unless you say
otherwise. That is what keeps a fill out of the plot rather than drawing
it as the darkest colour in the ramp. See
[`collect()`](https://rgdal-dev.github.io/adgl/reference/collect.md) for
the whole argument.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
g <- as_grd(x)
dim(g$data)
#> [1] 10 20  2

# Reading a corner is one windowed read, not a read of the whole raster.
dim(wk::grd_subset(g, i = 1:2, j = 1:3)$data)
#> [1] 2 3 2
```
