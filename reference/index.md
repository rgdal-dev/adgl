# Package index

## Open a source

A source is opened once and probed once, then held as a plan rather than
as data. Nothing is read until a terminal verb asks for it.

- [`src()`](https://rgdal-dev.github.io/adgl/reference/src.md) : A lazy
  spatial source
- [`src_close()`](https://rgdal-dev.github.io/adgl/reference/src_close.md)
  : Close a source
- [`raster_source()`](https://rgdal-dev.github.io/adgl/reference/raster_source.md)
  [`vector_source()`](https://rgdal-dev.github.io/adgl/reference/raster_source.md)
  : The source classes

## Shape a plan

These read nothing and cost nothing. query() narrows what will be read,
warp() says the grid is to be reprojected, and report() says what the
source’s own metadata is missing, what that will cost, and the text that
would fix it.

- [`query()`](https://rgdal-dev.github.io/adgl/reference/query.md) :
  Narrow a source
- [`warp()`](https://rgdal-dev.github.io/adgl/reference/warp.md) :
  Reproject a raster source
- [`report()`](https://rgdal-dev.github.io/adgl/reference/report.md) :
  What a source's metadata is missing

## Read

Where the reading happens. collect() returns the values, write_to()
sends them to a file, plot() draws them, and as_grd() hands back a grid
that reads on demand, one windowed GDAL read per subset.

- [`collect()`](https://rgdal-dev.github.io/adgl/reference/collect.md) :
  Read a plan into memory
- [`as_grd()`](https://rgdal-dev.github.io/adgl/reference/as_grd.md) : A
  raster source as a lazy grid
- [`adgl-plot`](https://rgdal-dev.github.io/adgl/reference/adgl-plot.md)
  : Draw a source
- [`write_to()`](https://rgdal-dev.github.io/adgl/reference/write_to.md)
  : Write a plan to a file

## At the edge

Conversions a caller mixing adgl with wk, vaster or ximage needs, kept
exported because the alternative is everyone rewriting them.

- [`extent_to_bbox()`](https://rgdal-dev.github.io/adgl/reference/extent_to_bbox.md)
  [`bbox_to_extent()`](https://rgdal-dev.github.io/adgl/reference/extent_to_bbox.md)
  : Extent order
- [`gis_attr()`](https://rgdal-dev.github.io/adgl/reference/gis_attr.md)
  : The gis attribute

## The package

- [`adgl`](https://rgdal-dev.github.io/adgl/reference/adgl-package.md)
  [`adgl-package`](https://rgdal-dev.github.io/adgl/reference/adgl-package.md)
  : adgl: Lazy Spatial Input and Output Through GDAL
