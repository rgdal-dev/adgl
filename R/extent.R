# Three coordinate orders meet in this package and they are easy to confuse,
# so all of the conversion happens here and nowhere else.
#
#   adgl, vaster, ximage, graphics::par("usr")   c(xmin, xmax, ymin, ymax)
#   wk::rct, GDAL7's get_extent and bbox args    c(xmin, ymin, xmax, ymax)
#
# adgl uses the first everywhere internally and in every user-facing argument,
# because vaster is where the grid arithmetic happens and an extent that
# changes meaning halfway through a call stack is a bug waiting to be written.
# The second appears only at the boundary, converted by the two functions
# below.

#' Extent order
#'
#' `adgl` takes and returns an extent as `c(xmin, xmax, ymin, ymax)`, which is
#' what `vaster`, `ximage` and `graphics::par("usr")` use. `wk::rct()` and
#' GDAL's own bounding-box arguments use `c(xmin, ymin, xmax, ymax)`. These
#' two functions convert between them, and they are exported because a caller
#' mixing `adgl` with `wk` needs the same conversion.
#'
#' @param extent An extent, `c(xmin, xmax, ymin, ymax)`.
#' @param bbox A bounding box, `c(xmin, ymin, xmax, ymax)`.
#'
#' @return The same four numbers in the other order, named.
#' @export
#' @examples
#' extent_to_bbox(c(100, 160, -60, -20))
#' bbox_to_extent(c(100, -60, 160, -20))
extent_to_bbox <- function(extent) {
  extent <- check_extent(extent, "extent")
  stats::setNames(extent[c(1L, 3L, 2L, 4L)], c("xmin", "ymin", "xmax", "ymax"))
}

#' @rdname extent_to_bbox
#' @export
bbox_to_extent <- function(bbox) {
  bbox <- as.double(unname(bbox))
  if (length(bbox) != 4L || anyNA(bbox)) {
    stop("`bbox` must be 4 non-missing numbers: xmin, ymin, xmax, ymax",
         call. = FALSE)
  }
  stats::setNames(bbox[c(1L, 3L, 2L, 4L)], c("xmin", "xmax", "ymin", "ymax"))
}

# Validate a user-supplied extent. Degenerate in either direction is refused
# rather than clamped, because an empty window is always a mistake upstream
# and a silent empty read is the hardest kind to find.
check_extent <- function(extent, what = "extent") {
  extent <- as.double(unname(extent))
  if (length(extent) != 4L || anyNA(extent)) {
    stop("`", what, "` must be 4 non-missing numbers: xmin, xmax, ymin, ymax",
         call. = FALSE)
  }
  if (extent[1L] >= extent[2L] || extent[3L] >= extent[4L]) {
    stop("`", what, "` has no area: it reads xmin ", extent[1L], ", xmax ",
         extent[2L], ", ymin ", extent[3L], ", ymax ", extent[4L],
         call. = FALSE)
  }
  stats::setNames(extent, c("xmin", "xmax", "ymin", "ymax"))
}

# The intersection of two extents, or NULL when they do not overlap.
intersect_extents <- function(a, b) {
  out <- c(max(a[1L], b[1L]), min(a[2L], b[2L]),
           max(a[3L], b[3L]), min(a[4L], b[4L]))
  if (out[1L] >= out[2L] || out[3L] >= out[4L]) {
    return(NULL)
  }
  stats::setNames(out, c("xmin", "xmax", "ymin", "ymax"))
}

# A geotransform to an extent, in adgl order. GDAL's geotransform is
# c(xmin, xres, xshear, ymax, yshear, yres) with yres negative for the usual
# north-up raster.
gt_extent <- function(geotransform, dimension) {
  gt <- as.double(geotransform)
  nx <- dimension[1L]
  ny <- dimension[2L]
  xmin <- gt[1L]
  xmax <- gt[1L] + nx * gt[2L]
  ymax <- gt[4L]
  ymin <- gt[4L] + ny * gt[6L]
  stats::setNames(
    c(min(xmin, xmax), max(xmin, xmax), min(ymin, ymax), max(ymin, ymax)),
    c("xmin", "xmax", "ymin", "ymax")
  )
}
