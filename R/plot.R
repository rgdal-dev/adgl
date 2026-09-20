#' @include src.R
NULL

#' Draw a source
#'
#' `plot()` is the third terminal verb, and it is lazy in the way its kind of
#' data allows.
#'
#' For a raster that means *resolution* laziness: the plan becomes a lazy grid
#' ([as_grd()]) and `wk`'s own grid plotting reads the device, works out a
#' step, and asks for about as many pixels as the device has, which GDAL
#' serves from whichever overview fits. Drawing a 30000 by 30000 source costs
#' the bytes of an 800 by 600 read, not of the file.
#'
#' For a vector it means *extent* laziness only, and the difference is worth
#' being plain about. There is no geometry overview and building one would be
#' exactly the automatic virtualisation this package refuses, so a wide view
#' of a big layer reads a lot of features. When the layer says how many that
#' is and the number is large, `plot()` says so before it starts.
#'
#' @param x A source from [src()], usually after [query()].
#' @param y Unused, and present only because `plot()`'s own signature has it.
#' @param ... Passed to the underlying plotting method, plus `as` for a
#'   raster: `"grd"` (the default) draws through [wk::grd_rct()], and `"gis"`
#'   reads at about the device's resolution and draws through
#'   [ximage::ximage()], which handles the three- and four-band cases as RGB.
#'
#' @return The drawn object, invisibly.
#' @name adgl-plot
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' plot(query(x, bands = 1))
#'
#' v <- src(system.file("extdata/test.gpkg", package = "GDAL7"))
#' plot(v)
NULL

S7::method(plot, raster_source) <- function(x, y, ..., as = c("grd", "gis")) {
  as <- match.arg(as)

  if (identical(as, "gis")) {
    values <- collect(query(x, dim = device_dimension(x)), as = "gis")
    ximage::ximage(values, ...)
    return(invisible(values))
  }

  if (!is.null(S7::prop(x, "plan")$warp)) {
    # A warped plan has no grid to window until the warp has happened, so
    # there is nothing to be lazy about; realise it and draw that.
    return(invisible(plot(collect(x), ...)))
  }

  g <- as_grd(x)
  plot(g, ...)
  invisible(g)
}

S7::method(plot, vector_source) <- function(x, y, ...) {
  warn_on_wide_read(x)
  d <- collect(x)
  wk::wk_plot(d, ...)
  invisible(d)
}

# A device-sized read for the gis path. wk does this itself for a grd, but
# ximage draws whatever it is handed, so the size has to be chosen here.
device_dimension <- function(x) {
  plan <- S7::prop(x, "plan")
  px <- tryCatch(grDevices::dev.size("px"), error = function(e) c(800, 600))
  pmax(1L, pmin(as.integer(plan$dimension), as.integer(ceiling(px))))
}

warn_on_wide_read <- function(x) {
  info <- S7::prop(x, "info")
  plan <- S7::prop(x, "plan")
  n <- info$feature_count
  if (is.na(n) || n < 0 || n < 1e5) {
    return(invisible(NULL))
  }
  if (is.null(plan$extent) && is.null(plan$where) && is.null(plan$limit)) {
    message("reading ", n, " features: a vector source has no overviews, so ",
            "a whole-layer plot reads the whole layer. query(extent = ) or ",
            "query(limit = ) first to read less.")
  }
  invisible(NULL)
}
