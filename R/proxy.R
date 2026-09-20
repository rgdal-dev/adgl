#' @include src.R
NULL

# A grid whose data is not there yet.
#
# wk documents that a grd's `data` member can be "an S3 object with an
# array-like subset method", and plot.wk_grd_rct() already does the whole
# device-driven downsample by itself: it reads par("usr"), computes a step,
# and calls grd_crop(), which bottoms out in
#
#   do.call("[", c(list(grid_data, i, j), more_dims, list(drop = FALSE)))
#
# So an object with dim() and `[` turns every one of wk's grid verbs into a
# windowed GDAL read, and adgl needs no plotting code of its own.

#' A raster source as a lazy grid
#'
#' `as_grd()` returns a [wk::grd_rct()] whose data is a proxy rather than an
#' array. Nothing is read until something subsets it, and every subset becomes
#' one windowed GDAL read at exactly the size asked for, which GDAL serves
#' from whichever overview fits.
#'
#' That is what makes `plot()` cheap. `wk`'s own `plot()` method reads the
#' device, works out a step, and crops the grid to it, so a 30000 by 30000
#' source drawn into an 800 by 600 device reads about 800 by 600 pixels.
#' `grd_crop()`, `grd_extend()`, `grd_tile()` and `grd_subset()` all work the
#' same way.
#'
#' Two details are worth knowing. wk censors out-of-range indices to `NA`, so
#' a request that runs off the edge of the raster comes back padded with `NA`
#' rather than short. And `[` is a general interface: when the indices are an
#' arithmetic sequence, which is all `grd_crop()` ever produces, the read is
#' one `RasterIO` call at the output size; when they are not, the proxy reads
#' the bounding window at full resolution and subsets in R, and says so when
#' that window is large.
#'
#' Every read through the proxy is a double, so `mask` needs no `type` pivot
#' here: a band's nodata value comes back as `NA` unless you say otherwise.
#' That is what keeps a fill out of the plot rather than drawing it as the
#' darkest colour in the ramp. See [collect()] for the whole argument.
#'
#' @param x A raster source from [src()], usually after [query()].
#' @param mask Return each band's nodata value as `NA`. `TRUE` by default.
#'
#' @return A `wk_grd_rct` whose `data` reads on demand.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' g <- as_grd(x)
#' dim(g$data)
#'
#' # Reading a corner is one windowed read, not a read of the whole raster.
#' dim(wk::grd_subset(g, i = 1:2, j = 1:3)$data)
as_grd <- function(x, mask = TRUE) {
  stopifnot(S7::S7_inherits(x, raster_source))
  mask <- resolve_mask(mask, "double")
  refuse_on_blocking(x, "as_grd")
  if (!is.null(S7::prop(x, "plan")$warp)) {
    stop("as_grd() cannot be lazy over a warped plan, because the warp has ",
         "to happen before there is a grid to window.\n",
         "  Use collect() to realise it, or write_to() a file and src() that.",
         call. = FALSE)
  }

  plan <- S7::prop(x, "plan")
  extent <- plan$extent
  crs <- S7::prop(x, "dataset")@crs

  wk::grd_rct(
    new_proxy(x, mask),
    bbox = wk::rct(
      extent[["xmin"]], extent[["ymin"]], extent[["xmax"]], extent[["ymax"]],
      crs = crs_or_null(crs)
    )
  )
}

new_proxy <- function(x, mask = TRUE) {
  plan <- S7::prop(x, "plan")
  # dim() is a method rather than an attribute, because a `dim` attribute on
  # a list has to match its length and this object holds no elements at all.
  structure(
    list(
      source = x,
      mask = mask,
      dim = c(plan$dimension[2L], plan$dimension[1L], length(plan$bands))
    ),
    class = "adgl_proxy"
  )
}

#' @export
dim.adgl_proxy <- function(x) {
  unclass(x)$dim
}

# Registered against base::print by name rather than with a plain @export:
# this package gives S7 a print method for its own classes, which puts an S7
# generic called `print` in the namespace, and a bare S3method() directive
# would then register against that one instead of base's. The proxy would
# print as a raw list from anywhere outside the package.
#' @exportS3Method base::print
print.adgl_proxy <- function(x, ...) {
  d <- dim(x)
  cat("<adgl_proxy ", d[1L], " x ", d[2L], " x ", d[3L], ", unread>\n", sep = "")
  invisible(x)
}

#' @export
as.array.adgl_proxy <- function(x, ...) {
  x[seq_len(dim(x)[1L]), seq_len(dim(x)[2L]), drop = FALSE]
}

#' @export
`[.adgl_proxy` <- function(x, i, j, k, ..., drop = FALSE) {
  d <- dim(x)
  i <- if (missing(i) || is.null(i)) seq_len(d[1L]) else i
  j <- if (missing(j) || is.null(j)) seq_len(d[2L]) else j
  k <- if (missing(k) || is.null(k)) seq_len(d[3L]) else k

  source <- unclass(x)$source
  plan <- S7::prop(source, "plan")
  bands <- plan$bands[k]
  if (anyNA(bands)) {
    stop("band index out of range", call. = FALSE)
  }

  out <- array(NA_real_, dim = c(length(i), length(j), length(k)))
  keep_i <- which(!is.na(i))
  keep_j <- which(!is.na(j))
  if (length(keep_i) == 0L || length(keep_j) == 0L) {
    return(out)
  }

  values <- proxy_read(source, i[keep_i], j[keep_j], bands)
  if (isTRUE(unclass(x)$mask)) {
    values <- mask_values(values, band_nodata(S7::prop(source, "dataset"), bands))
  }
  for (b in seq_along(values)) {
    out[keep_i, keep_j, b] <- t(matrix(values[[b]],
                                       nrow = length(keep_j),
                                       ncol = length(keep_i)))
  }
  out
}

# i indexes rows (y, increasing downward), j indexes columns (x). Both are
# one-based into the plan's window, not into the source.
proxy_read <- function(source, i, j, bands) {
  plan <- S7::prop(source, "plan")
  window <- raster_window(plan)

  if (is_regular(i) && is_regular(j)) {
    # One RasterIO at the output size. An arithmetic sequence with a step
    # greater than one is served as a resampling of the span rather than as
    # literally every nth pixel, which is what makes a plot cheap and is what
    # wk's own crop asks for.
    sub <- c(
      window[1L] + min(j) - 1,
      window[2L] + min(i) - 1,
      diff(range(j)) + 1,
      diff(range(i)) + 1
    )
    return(GDAL7::read_raster(
      S7::prop(source, "dataset"),
      window = sub,
      out_size = c(length(j), length(i)),
      resample = plan$resample,
      bands = bands
    ))
  }

  # Arbitrary indices: read the bounding window whole and subset in R.
  span_x <- diff(range(j)) + 1
  span_y <- diff(range(i)) + 1
  if (span_x * span_y > 1e7) {
    warning("these indices are not a regular sequence, so the whole ",
            span_x, " by ", span_y, " window has to be read and subset in R",
            call. = FALSE)
  }
  sub <- c(window[1L] + min(j) - 1, window[2L] + min(i) - 1, span_x, span_y)
  whole <- GDAL7::read_raster(
    S7::prop(source, "dataset"),
    window = sub,
    out_size = c(span_x, span_y),
    resample = plan$resample,
    bands = bands
  )
  lapply(whole, function(v) {
    m <- matrix(v, nrow = span_x, ncol = span_y)
    as.vector(m[j - min(j) + 1L, i - min(i) + 1L, drop = FALSE])
  })
}

is_regular <- function(index) {
  length(index) == 1L || length(unique(diff(index))) == 1L
}
