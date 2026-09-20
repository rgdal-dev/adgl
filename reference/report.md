# What a source's metadata is missing

`report()` returns one row per finding: what the source does not say
about itself, what that will cost the read, and the exact text that
would fix it.

## Usage

``` r
report(x)
```

## Arguments

- x:

  A source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md).

## Value

A tibble with columns `check` (a short id), `severity` (`"blocks"`,
`"degrades"` or `"note"`), `what`, `consequence`, `fix_kind`
(`"open_option"`, `"vrt"`, `"gdal_cli"` or `"r_call"`) and `fix`. Zero
rows when the source says everything it should.

## Details

Nothing in this package ever applies a fix. The `fix` column is text for
you to run or to pass back in, and the only ways to act on one are to
hand an open option to
[`src()`](https://rgdal-dev.github.io/adgl/reference/src.md), to run the
suggested command yourself, or to call a verb named for what it does,
such as [`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md).
A package that quietly wrapped a georeference-less file in a VRT that
supplies a geotransform would be hiding exactly the thing you need to
know.

The findings are computed once when the source is opened, from calls
GDAL has already cached, so `report()` reads nothing and costs nothing.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
report(x)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: check <chr>, severity <chr>, what <chr>, consequence <chr>,
#> #   fix_kind <chr>, fix <chr>
```
