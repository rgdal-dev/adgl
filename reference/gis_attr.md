# The gis attribute

`gis_attr()` builds the attribute that `gdalraster::read_ds()` puts on
its result, which
[`ximage::ximage()`](https://rdrr.io/pkg/ximage/man/ximage.html) reads
to draw an image in its own coordinates. `adgl` emits this shape without
importing gdalraster, so the contract is reproduced here and asserted by
a test rather than inherited.

## Usage

``` r
gis_attr(bbox, dim, srs, datatype)
```

## Arguments

- bbox:

  `c(xmin, ymin, xmax, ymax)`.

- dim:

  `c(nx, ny, nbands)`.

- srs:

  The CRS as WKT, or `""`.

- datatype:

  Character, the GDAL type name of each band.

## Value

A list of five elements: `type`, `bbox`, `dim`, `srs`, `datatype`.

## Details

Read from gdalraster at its master branch on 2026-09-20. Note the two
orders: `bbox` is `c(xmin, ymin, xmax, ymax)` while `dim` is
`c(nx, ny, nbands)`, with x first.

## Examples

``` r
gis_attr(c(0, 0, 10, 10), c(10L, 10L, 1L), "", "Byte")
#> $type
#> [1] "raster"
#> 
#> $bbox
#> [1]  0  0 10 10
#> 
#> $dim
#> [1] 10 10  1
#> 
#> $srs
#> [1] ""
#> 
#> $datatype
#> [1] "Byte"
#> 
```
