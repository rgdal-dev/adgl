# Close a source

Releases the GDAL dataset the source holds open. A source that has been
closed cannot be read from again. Sources are closed for you when they
are garbage collected, so this matters only when a file has to be
released at a known moment: overwriting it, deleting it, or opening it
again for update.

## Usage

``` r
src_close(x)
```

## Arguments

- x:

  A source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md).

## Value

`x`, invisibly.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
src_close(x)
```
