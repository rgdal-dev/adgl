#' @keywords internal
#'
#' @importFrom PROJ proj_trans
#' @importFrom vaster vcrop
#' @importFrom ximage ximage
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # S7 methods for generics that live in other packages, print() and plot()
  # among them, are registered here rather than at build time.
  S7::methods_register()
}
