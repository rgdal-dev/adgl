# Extent order

`adgl` takes and returns an extent as `c(xmin, xmax, ymin, ymax)`, which
is what `vaster`, `ximage` and `graphics::par("usr")` use.
[`wk::rct()`](https://paleolimbot.github.io/wk/reference/rct.html) and
GDAL's own bounding-box arguments use `c(xmin, ymin, xmax, ymax)`. These
two functions convert between them, and they are exported because a
caller mixing `adgl` with `wk` needs the same conversion.

## Usage

``` r
extent_to_bbox(extent)

bbox_to_extent(bbox)
```

## Arguments

- extent:

  An extent, `c(xmin, xmax, ymin, ymax)`.

- bbox:

  A bounding box, `c(xmin, ymin, xmax, ymax)`.

## Value

The same four numbers in the other order, named.

## Examples

``` r
extent_to_bbox(c(100, 160, -60, -20))
#> xmin ymin xmax ymax 
#>  100  -60  160  -20 
bbox_to_extent(c(100, -60, 160, -20))
#> xmin xmax ymin ymax 
#>  100  160  -60  -20 
```
