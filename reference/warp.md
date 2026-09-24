# Reproject a raster source

`warp()` is the raster verb for changing coordinate reference system. It
is separate from
[`query()`](https://rgdal-dev.github.io/adgl/reference/query.md) because
reprojecting a grid is a different act from subsetting one: it
resamples, and which resampling to use is a choice you should make
rather than inherit from a default. Ask for `"nearest"` on a class
raster and `"bilinear"` or `"average"` on a continuous one.

## Usage

``` r
warp(x, crs, ...)
```

## Arguments

- x:

  A raster source from
  [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md).

- crs:

  The target CRS: anything GDAL understands, such as `"EPSG:3031"`, a
  PROJ string, or WKT.

- ...:

  The named arguments below.

  Three arguments shape the target grid and any one of them is enough.
  `dim` gives it a size, `resolution` gives it a pixel size, and
  `extent` gives it a window; leave all three out and GDAL chooses a
  grid that roughly preserves the source resolution.

  `dim` takes a zero, or an `NA`, in either position, which is the
  useful part: `c(1024, 0)` asks for a grid 1024 pixels wide and lets
  GDAL work out the height from the target extent's own aspect ratio,
  which you would otherwise have to compute in a projection you have not
  seen yet. `c(0, 1024)` is the same the other way round. The two
  spellings mean one thing, "work this side out rather than taking it
  from me", which is what `NA` also means in
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md)'s
  `extent`. `0` is GDAL's own spelling in `--size` and is free here
  because no dimension is ever 0; it is not free in an extent, where 0
  is a perfectly good coordinate, which is why the sentinel differs
  between the two and the meaning does not.

  `extent` is the one argument here that is given in the *target* CRS,
  since it is the window the output covers. That makes it an alternative
  to
  [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md)`(extent = )`
  rather than a companion to it, and asking for both is an error rather
  than a silent choice between them.

## Value

`x` with a reprojection added to its plan.

## Details

This is the one place `adgl` virtualises a source, and it does so
without apology: GDAL implements a warp as a warped VRT or a warped read
and there is no other way to reproject a grid. The rule the package
holds is the narrower one it was always about, that it will never
quietly augment a source to supply metadata the source is missing. It
will reproject when you ask it to, and it will still never invent a
geotransform for a file that has none.

The target extent is computed with
[`GDAL7::transform_extent()`](https://rgdal-dev.github.io/GDAL7/reference/transform_extent.html),
which unions GDAL's boundary walk with an interior mesh, rather than
left to the boundary walk alone. That matters where the target
projection has an interior singularity: the walk alone can come back
hundreds of kilometres too narrow around the antipode of an azimuthal
projection, and an extent that is too small clips data silently.

When nothing pins the target window, the extent GDAL chooses is measured
once the warp has run, by inverting its corners and comparing the
distance it claims against the distance it covers on the ground. A
whole-globe source into a polar projection comes back with an extent
8e23 m across whose four corners are all the same pole, which is a
picture of a singularity rather than of the data. That is a warning with
`dim` and an error with `resolution`, where GDAL cannot build the grid
at all. Where GDAL clamps for itself, as it does for Web Mercator's
latitude limit, there is nothing to say and nothing is said.

Nothing is read here. The reprojection is recorded in the plan and
happens when a terminal verb asks for the result, so `warp()` composes
with [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md) in
either order.

## Arguments

- `resample`:

  One of `"nearest"` (the default), `"bilinear"`, `"cubic"`,
  `"cubicspline"`, `"lanczos"`, `"average"`, `"mode"`, `"rms"`, `"sum"`,
  `"min"`, `"max"`, `"med"`, `"q1"` or `"q3"`.

- `dim`:

  `c(nx, ny)`, the size of the warped grid, with a zero in either
  position meaning "derive this one from the other".

- `resolution`:

  The target pixel size in the units of the target CRS: one number for
  square pixels, or `c(xres, yres)`. Cannot be given beside `dim`.

- `extent`:

  `c(xmin, xmax, ymin, ymax)` in the target CRS, the window the output
  covers.

## Examples

``` r
x <- src(system.file("extdata/test.tif", package = "GDAL7"))
w <- warp(query(x, extent = c(-170, 170, -80, 80)), "EPSG:3857",
          resample = "bilinear")
w
#> raster source GTiff  20 x 10 x 2
#>   extent -180, 180, -90, 90
#>   crs    EPSG:4326
#>   warp   to EPSG:3857 by bilinear
#>   nothing missing

# 400 pixels wide, and however tall that turns out to be.
warp(x, "EPSG:3857", dim = c(400, 0))
#> raster source GTiff  20 x 10 x 2
#>   extent -180, 180, -90, 90
#>   crs    EPSG:4326
#>   warp   to EPSG:3857 by nearest
#>   size   400 x 0 (0 is GDAL's to fill in)
#>   nothing missing
```
