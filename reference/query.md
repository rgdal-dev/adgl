# Narrow a source

`query()` adds to a source's plan and reads nothing. It composes, so a
pipeline of `query()` calls is the same as one call with all the
arguments, and the result is still a source you can print, query again,
or hand to a terminal verb.

## Usage

``` r
query(x, ...)
```

## Arguments

- x:

  A source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md).

- ...:

  The named arguments below.

## Value

`x` with the plan narrowed.

## Details

Some arguments mean the same thing for both kinds of source and some do
not, which is deliberate. `extent` is the same operation on both:
restrict to this rectangle. `bands` and `fields` are the same operation
on different nouns. But resolution has no vector meaning and attribute
filtering has no raster meaning, so `dim` and `resample` are raster-only
and `where` and `limit` are vector-only. Passing one to the wrong kind
is an error rather than a silent no-op, because a spatial query that
quietly ignored half of what you asked for is worse than one that stops.

`crs` is the one argument that means something different for each kind,
and the difference is the point rather than an inconsistency. For both,
an `extent` given beside it is read in that CRS and transformed onto the
source to pick what to read, which is four numbers moving and no data
touched. For a vector it additionally sets the CRS the features come
back in, because transforming coordinates is cheap, local and has no
resampling decision in it. For a raster it does not, because
reprojecting a grid resamples: that is
[`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md), a
separate verb so the method stays a choice you make rather than one you
inherit.

A raster `extent` snaps to whole source pixels, outward by default, so a
query with no `dim` reads the source's own values rather than a
resampling of them, and the extent the result carries is the one its
pixels really have. `snap = "in"` shrinks instead, and `"near"` goes to
the nearest edge. What none of them will do is land the window between
two pixels: GDAL reads at an integer pixel offset, and moving a grid off
its own edges is a resampling, which is
[`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md)'s job. So
`query()` moves the window to the data and
[`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md) moves the
data to the window.

A position in `extent` may be left unsaid, as `NA` or as an infinity,
and it then means the bound the plan already has: `c(NA, 150, NA, NA)`
is everything west of 150 without having to look the other three up.
Because that bound is in the source's own CRS, an unsaid position cannot
be combined with `crs`.

`pad` says whether the rectangle may leave the source. By default it may
not, and an `extent` is intersected with what the plan already covers,
so a read can never run past the edge. With `pad = TRUE` the rectangle
is kept whole and the part outside comes back as `NA`, which is what
rasterio calls a boundless read and terra calls `extend`. The padding is
in whole source pixels, so it does not move the grid; it needs
`type = "double"`, for the same reason masking does, and it cannot be
combined with
[`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md), because
the pad is put on after the read and the warper never sees it.

## Arguments

All of these are passed by name.

- `extent`:

  `c(xmin, xmax, ymin, ymax)`, the rectangle to restrict to. A position
  may be `NA` or infinite, meaning the bound the plan already has.

- `crs`:

  Anything GDAL and PROJ understand. An `extent` beside it is read in
  this CRS. For a vector source it is also the CRS the features come
  back in; for a raster it is not, and
  [`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md) is.

- `bands`:

  Raster only. Which bands, one-based, or by band description.

- `dim`:

  Raster only. `c(nx, ny)`, the size to read the window at. This is what
  makes a read cheap: asking for a small `dim` over a large extent lets
  GDAL serve it from an overview.

- `resample`:

  Raster only. One of `"nearest"` (the default), `"bilinear"`,
  `"cubic"`, `"cubicspline"`, `"lanczos"`, `"average"`, `"mode"`,
  `"gauss"` or `"rms"`.

- `snap`:

  Raster only. Where `extent` lands on the source's pixel edges: `"out"`
  (the default), `"near"` or `"in"`.

- `pad`:

  Raster only. Allow `extent` to leave the source and fill the outside
  with `NA`. `FALSE` by default.

- `where`:

  Vector only. An SQL `WHERE` clause. GDAL does not validate it when it
  is set, so a bad clause shows up as a warning at read time rather than
  an error here. It is GDAL's SQL, so a field is named as the source
  names it, except that `fid` is the feature id on every driver.

- `fields`:

  Vector only. Which attribute columns to read, by name.

- `limit`:

  Vector only. Stop after this many features.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
query(x, extent = c(-100, 0, 0, 80), dim = c(8, 8), resample = "average")
#> raster source GTiff  6 x 5 x 2
#>   extent -108, 0, 0, 90
#>   crs    EPSG:4326
#>   reads  8 x 8 by average
#>   nothing missing

v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
query(v, where = "population > 1e6", fields = "name")
#> vector source places  5 features  Point
#>   extent 115.8605, 151.2093, -42.8826, -12.4634
#>   crs    WGS 84
#>   where  population > 1e6 (not checked until read)
#>   fields name
#>   nothing missing
```
