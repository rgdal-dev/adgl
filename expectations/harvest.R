# Harvest what the neighbours show off.
#
# The packages around adgl - sf, terra, gdalraster, stars, vapour, tmap, s2 -
# carry thousands of worked examples in their help pages, tests and vignettes.
# Those examples are the closest thing there is to a written record of what a
# user of an R spatial package expects to be able to do. This script reads
# them and counts which operations they actually perform, so adgl can be
# measured against that record rather than against our own idea of it.
#
# It is deliberately not part of the test suite: it needs the other packages'
# sources, which are not a dependency of anything, and it reaches the network
# to get them. Run it by hand.
#
#   Rscript expectations/harvest.R [source-directory] [output-directory]
#
# `source-directory` holds one checkout per package, named after the package.
# The CRAN mirror on GitHub is the easy way to fill it:
#
#   for p in sf terra gdalraster stars vapour tmap s2; do
#     git clone --depth 1 https://github.com/cran/$p <source-directory>/$p
#   done

PACKAGES <- c("sf", "terra", "gdalraster", "stars", "vapour", "tmap", "s2")

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1L) args[[1L]] else "../cran"
out_dir <- if (length(args) >= 2L) args[[2L]] else "expectations"

# ---------------------------------------------------------------------------
# What counts as one of a package's own functions.
#
# NAMESPACE is the honest answer where it lists names, but gdalraster uses
# exportPattern() and terra exports methods rather than functions, so the set
# is unioned with everything assigned at the top level of R/.

package_names <- function(dir) {
  names <- character()

  ns <- file.path(dir, "NAMESPACE")
  if (file.exists(ns)) {
    text <- paste(readLines(ns, warn = FALSE), collapse = "\n")
    for (directive in c("export", "exportMethods", "S3method")) {
      hits <- gregexpr(paste0(directive, "\\(([^)]*)\\)"), text)[[1L]]
      if (hits[1L] > 0L) {
        inner <- regmatches(text, gregexpr(paste0(directive, "\\(([^)]*)\\)"),
                                           text))[[1L]]
        inner <- sub(paste0("^", directive, "\\("), "", sub("\\)$", "", inner))
        parts <- unlist(strsplit(inner, ","), use.names = FALSE)
        names <- c(names, trimws(gsub('["\']', "", parts)))
      }
    }
  }

  r_files <- list.files(file.path(dir, "R"), pattern = "[.][Rr]$",
                        full.names = TRUE)
  for (f in r_files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    for (e in as.list(exprs)) {
      if (is.call(e) && length(e) >= 3L && is.name(e[[1L]]) &&
          as.character(e[[1L]]) %in% c("<-", "=", "assign") &&
          is.name(e[[2L]])) {
        names <- c(names, as.character(e[[2L]]))
      }
    }
  }

  unique(names[nzchar(names) & !grepl("^[.]", names)])
}

# ---------------------------------------------------------------------------
# The three places worked examples live.

example_code <- function(dir) {
  rd <- list.files(file.path(dir, "man"), pattern = "[.]Rd$", full.names = TRUE)
  out <- lapply(rd, function(f) {
    parsed <- tryCatch(tools::parse_Rd(f), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp), add = TRUE)
    # commentDontrun = FALSE keeps the \dontrun blocks, which is where the
    # remote sources and the slow, interesting cases are kept.
    ok <- tryCatch({
      tools::Rd2ex(parsed, out = tmp, commentDontrun = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok || !file.exists(tmp)) return(NULL)
    code <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    if (!nzchar(trimws(code))) return(NULL)
    data.frame(source = "example", file = basename(f), code = code)
  })
  do.call(rbind, out)
}

test_code <- function(dir) {
  files <- list.files(file.path(dir, "tests"), pattern = "[.][Rr]$",
                      full.names = TRUE, recursive = TRUE)
  files <- c(files, list.files(file.path(dir, "inst", "tinytest"),
                               pattern = "[.][Rr]$", full.names = TRUE))
  if (length(files) == 0L) return(NULL)
  do.call(rbind, lapply(files, function(f) {
    data.frame(source = "test", file = basename(f),
               code = paste(readLines(f, warn = FALSE), collapse = "\n"))
  }))
}

vignette_code <- function(dir) {
  files <- list.files(file.path(dir, "vignettes"), pattern = "[.]R(md|nw)$",
                      full.names = TRUE, recursive = TRUE)
  if (length(files) == 0L) return(NULL)
  do.call(rbind, lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    # Pull the R chunks out of the prose. Both fence styles appear.
    opens <- grep("^\\s*```+\\s*\\{r", lines)
    closes <- grep("^\\s*```+\\s*$", lines)
    chunks <- character()
    for (o in opens) {
      close <- closes[closes > o]
      if (length(close) == 0L) next
      chunks <- c(chunks, lines[seq(o + 1L, close[1L] - 1L)])
    }
    if (length(chunks) == 0L) return(NULL)
    data.frame(source = "vignette", file = basename(f),
               code = paste(chunks, collapse = "\n"))
  }))
}

# ---------------------------------------------------------------------------
# Walk the parsed code and record every call to one of the package's own
# functions, with the argument names the caller actually used. The arguments
# are the interesting half: st_read(dsn, layer) and st_read(dsn, query =) are
# different expectations wearing the same name.

collect_calls <- function(expr, keep, into) {
  if (is.call(expr)) {
    head <- expr[[1L]]
    if (is.name(head)) {
      fun <- as.character(head)
      if (fun %in% keep) {
        used <- names(expr)[-1L]
        used <- used[!is.na(used) & nzchar(used)]
        into$add(fun, used)
      }
    }
    for (part in as.list(expr)) {
      if (!missing(part) && (is.call(part) || is.expression(part))) {
        collect_calls(part, keep, into)
      }
    }
  } else if (is.expression(expr)) {
    for (part in as.list(expr)) collect_calls(part, keep, into)
  }
  invisible(NULL)
}

new_sink <- function() {
  funs <- character()
  args <- character()
  list(
    add = function(fun, used) {
      funs <<- c(funs, fun)
      args <<- c(args, if (length(used)) paste(sort(used), collapse = " ") else "")
    },
    result = function() data.frame(fun = funs, args = args)
  )
}

harvest_one <- function(package, dir) {
  message("  ", package)
  keep <- package_names(dir)
  code <- rbind(example_code(dir), test_code(dir), vignette_code(dir))
  if (is.null(code) || nrow(code) == 0L) return(NULL)

  rows <- lapply(seq_len(nrow(code)), function(i) {
    exprs <- tryCatch(parse(text = code$code[i], keep.source = FALSE),
                      error = function(e) NULL)
    if (is.null(exprs)) return(NULL)
    sink <- new_sink()
    collect_calls(exprs, keep, sink)
    got <- sink$result()
    if (nrow(got) == 0L) return(NULL)
    data.frame(package = package, source = code$source[i],
               file = code$file[i], got)
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------

message("harvesting from ", normalizePath(source_dir, mustWork = FALSE))
all <- do.call(rbind, lapply(PACKAGES, function(p) {
  dir <- file.path(source_dir, p)
  if (!dir.exists(dir)) {
    message("  ", p, " (missing, skipped)")
    return(NULL)
  }
  harvest_one(p, dir)
}))

if (is.null(all) || nrow(all) == 0L) {
  stop("nothing harvested: is `source-directory` right?", call. = FALSE)
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

calls <- aggregate(list(calls = seq_len(nrow(all))), by = all[c("package", "fun")],
                   FUN = length)
files <- aggregate(list(files = all$file), by = all[c("package", "fun")],
                   FUN = function(x) length(unique(x)))
sources <- aggregate(list(sources = all$source), by = all[c("package", "fun")],
                     FUN = function(x) paste(sort(unique(x)), collapse = "+"))
inventory <- merge(merge(calls, files), sources)
inventory <- inventory[order(-inventory$calls), ]
write.csv(inventory, file.path(out_dir, "inventory.csv"), row.names = FALSE)

arg_rows <- all[nzchar(all$args), c("package", "fun", "args")]
arg_use <- aggregate(list(calls = seq_len(nrow(arg_rows))),
                     by = arg_rows[c("package", "fun", "args")], FUN = length)
arg_use <- arg_use[order(arg_use$package, arg_use$fun, -arg_use$calls), ]
write.csv(arg_use, file.path(out_dir, "argument-use.csv"), row.names = FALSE)

message("\n", nrow(all), " calls to ", nrow(inventory),
        " distinct functions, from ", length(unique(all$file)), " files")
message("wrote ", file.path(out_dir, "inventory.csv"), " and ",
        file.path(out_dir, "argument-use.csv"))
