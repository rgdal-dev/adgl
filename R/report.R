#' @include src.R
NULL

#' What a source's metadata is missing
#'
#' `report()` returns one row per finding: what the source does not say about
#' itself, what that will cost the read, and the exact text that would fix it.
#'
#' Nothing in this package ever applies a fix. The `fix` column is text for
#' you to run or to pass back in, and the only ways to act on one are to hand
#' an open option to [src()], to run the suggested command yourself, or to
#' call a verb named for what it does, such as [warp()]. A package that
#' quietly wrapped a georeference-less file in a VRT that supplies a
#' geotransform would be hiding exactly the thing you need to know.
#'
#' The findings are computed once when the source is opened, from calls GDAL
#' has already cached, so `report()` reads nothing and costs nothing.
#'
#' @param x A source from [src()].
#'
#' @return A tibble with columns `check` (a short id), `severity`
#'   (`"blocks"`, `"degrades"` or `"note"`), `what`, `consequence`,
#'   `fix_kind` (`"open_option"`, `"vrt"`, `"gdal_cli"` or `"r_call"`) and
#'   `fix`. Zero rows when the source says everything it should.
#' @export
#' @examples
#' x <- src(system.file("extdata/test.tif", package = "GDAL7"))
#' report(x)
report <- function(x) {
  stopifnot(S7::S7_inherits(x, spatial_source))
  S7::prop(x, "findings")
}

finding <- function(check, severity, what, consequence, fix_kind, fix) {
  tibble::tibble(
    check = check, severity = severity, what = what,
    consequence = consequence, fix_kind = fix_kind, fix = fix
  )
}

no_findings <- function() {
  tibble::tibble(
    check = character(), severity = character(), what = character(),
    consequence = character(), fix_kind = character(), fix = character()
  )
}

print_findings <- function(findings) {
  n <- nrow(findings)
  if (n == 0L) {
    cat("  nothing missing\n")
    return(invisible(NULL))
  }
  cat("  ", n, if (n == 1L) " finding: " else " findings: ",
      paste(findings$check, collapse = ", "), "  (see report(x))\n", sep = "")
  invisible(NULL)
}

raster_findings <- function(x) {
  ds <- S7::prop(x, "dataset")
  info <- S7::prop(x, "info")
  dsn <- S7::prop(x, "dsn")
  gt <- info$geotransform
  out <- list()

  identity_gt <- !is.null(gt) &&
    isTRUE(all.equal(as.double(gt), c(0, 1, 0, 0, 0, 1)))

  if (is.null(gt)) {
    out[[length(out) + 1L]] <- finding(
      "no_geotransform", "blocks",
      "the driver reports no geotransform",
      paste0("there is no mapping from pixels to coordinates, so an extent ",
             "query means nothing and collect() has no bbox to put on the ",
             "result"),
      "gdal_cli",
      paste0("gdal_translate -a_ullr <xmin> <ymax> <xmax> <ymin> -a_srs ",
             "<crs> ", dsn, " georeferenced.tif")
    )
  } else if (identity_gt) {
    # The quiet one: the driver reported success and the raster is still
    # sitting at the origin with one-unit pixels.
    out[[length(out) + 1L]] <- finding(
      "identity_geotransform", "blocks",
      "the geotransform is the identity, c(0, 1, 0, 0, 0, 1)",
      paste0("the raster sits at the origin with 1-unit pixels, which is ",
             "what GDAL reports when a file carries no georeferencing at all"),
      "gdal_cli",
      paste0("gdal_translate -a_ullr <xmin> <ymax> <xmax> <ymin> -a_srs ",
             "<crs> ", dsn, " georeferenced.tif")
    )
  } else if (gt[3L] != 0 || gt[5L] != 0) {
    # Neither in-memory form can hold a rotation, so this one blocks the read
    # rather than degrading it. See collect().
    out[[length(out) + 1L]] <- finding(
      "rotated_geotransform", "blocks",
      paste0("the geotransform is rotated or sheared (gt[3] = ", gt[3L],
             ", gt[5] = ", gt[5L], ")"),
      paste0("neither a wk grd nor the gis form can hold a rotation, so ",
             "collect() refuses rather than returning an axis-aligned lie"),
      "r_call",
      "warp(x, <crs>, resample = \"bilinear\")"
    )
  }

  if (!has_crs(ds@crs)) {
    out[[length(out) + 1L]] <- finding(
      "no_crs", "degrades",
      "the source declares no coordinate reference system",
      "the coordinates are unitless, and nothing can reproject or overlay them",
      "gdal_cli",
      paste0("gdal_edit.py -a_srs <crs> ", dsn)
    )
  }

  if (ds@gcp_count > 0L && (is.null(gt) || identity_gt)) {
    out[[length(out) + 1L]] <- finding(
      "gcps_only", "blocks",
      paste0("georeferenced by ", ds@gcp_count,
             " ground control points and nothing else"),
      paste0("turning GCPs into a grid is a warp, and this package will not ",
             "do one you did not ask for"),
      "gdal_cli",
      paste0("gdalwarp -r bilinear ", dsn, " warped.tif")
    )
  }

  domains <- ds@metadata_domain_list %||% character()
  for (d in intersect(c("RPC", "GEOLOCATION"), domains)) {
    out[[length(out) + 1L]] <- finding(
      tolower(paste0(d, "_only")), "blocks",
      paste0("georeferenced through the ", d, " metadata domain"),
      "the same situation as GCPs by another mechanism, and it needs a warp",
      "gdal_cli",
      paste0("gdalwarp -rpc ", dsn, " warped.tif")
    )
  }

  bands <- info$band_info
  if (!is.null(bands) && nrow(bands) > 0L) {
    if (all(is.na(bands$nodata))) {
      out[[length(out) + 1L]] <- finding(
        "no_nodata", "degrades",
        "no band declares a nodata value",
        "fill values read as data, so statistics and plots include them",
        "open_option",
        "read the values, then set it: gdal_edit.py -a_nodata <value> <file>"
      )
    }

    if (all(bands$overviews == 0L)) {
      remote <- grepl("^/vsi", dsn) && !grepl("^/vsimem", dsn)
      large <- prod(as.double(info$size)) > 4e6
      if (remote || large) {
        out[[length(out) + 1L]] <- finding(
          "no_overviews", if (remote) "degrades" else "note",
          "the source has no overviews",
          paste0("a low-resolution read has to pull full resolution, because ",
                 "there is no reduced level for GDAL to serve it from"),
          "gdal_cli",
          paste0("gdaladdo -r average ", dsn, " 2 4 8 16")
        )
      }
    }

    if (nrow(bands) %in% c(3L, 4L) &&
        all(bands$color %in% c("Undefined", "", NA_character_))) {
      out[[length(out) + 1L]] <- finding(
        "no_color_interpretation", "note",
        paste0(nrow(bands), " bands, none with a colour interpretation set"),
        "a plot will be greyscale from band 1 rather than RGB",
        "r_call",
        "collect(x, as = \"gis\") |> ximage::ximage()"
      )
    }

    # A block one row tall on a tall raster means strip organisation, where a
    # windowed read still costs whole rows.
    strip <- bands$block_y == 1L & info$size[["ysize"]] > 512L
    if (any(strip)) {
      out[[length(out) + 1L]] <- finding(
        "strip_organised", "degrades",
        "the blocks are single rows",
        "a windowed read still pays for whole rows, so a small window is not a small read",
        "gdal_cli",
        paste0("gdal_translate -co TILED=YES -co COMPRESS=ZSTD ", dsn,
               " tiled.tif")
      )
    }
  }

  if (length(out) == 0L) no_findings() else do.call(rbind, out)
}

vector_findings <- function(x) {
  info <- S7::prop(x, "info")
  dsn <- S7::prop(x, "dsn")
  out <- list()

  # A table with no geometry has no coordinates for a CRS to describe.
  if (!identical(info$geometry_type, "None") && !has_crs(info$crs)) {
    out[[length(out) + 1L]] <- finding(
      "no_crs", "degrades",
      "the layer declares no coordinate reference system",
      "the coordinates are unitless, and nothing can reproject or overlay them",
      "gdal_cli",
      paste0("ogr2ogr -a_srs <crs> out.gpkg ", dsn)
    )
  }

  # A result set is not a table: it has no index of its own to build, and no
  # count to know before it is run, so the two findings about those, and the
  # remedies they give, are about a layer this source does not have.
  from_sql <- !is.null(S7::prop(x, "plan")$sql)

  if (!from_sql && !info$fast_spatial_filter) {
    out[[length(out) + 1L]] <- finding(
      "slow_spatial_filter", "degrades",
      "the layer has no spatial index",
      "an extent query reads every feature and discards most of them",
      "gdal_cli",
      paste0("ogrinfo -sql \"SELECT CreateSpatialIndex('<layer>', '<geom>')\" ",
             dsn)
    )
  }

  n <- info$feature_count
  if (!from_sql && (is.na(n) || n < 0)) {
    out[[length(out) + 1L]] <- finding(
      "unknown_feature_count", "note",
      "the driver cannot report a feature count without a full scan",
      "the plan cannot say how big its result will be before reading it",
      "r_call",
      "GDAL7::feature_count(GDAL7::get_layer(ds), force = TRUE)"
    )
  }

  if (identical(info$geometry_type, "Unknown")) {
    out[[length(out) + 1L]] <- finding(
      "unknown_geometry_type", "note",
      "the layer declares its geometry type as Unknown",
      "a later write_to() needs the type given explicitly",
      "r_call",
      "write_to(x, \"out.gpkg\", geometry_type = \"POINT\")"
    )
  }

  if (grepl("\\.shp$", dsn, ignore.case = TRUE)) {
    out[[length(out) + 1L]] <- finding(
      "shapefile", "note",
      "the source is a shapefile",
      paste0("field names truncate at 10 characters, there is no 64-bit ",
             "integer type, and one file holds one layer"),
      "gdal_cli",
      paste0("ogr2ogr out.gpkg ", dsn)
    )
  }

  if (length(out) == 0L) no_findings() else do.call(rbind, out)
}
