#' Wrapper to fdPar
#'
#' A helper function that builds a fda::fdPar object given a set of time points.
#' It is used by one_landmarkreg_nocurves to pass an fdPar object to smooth.morph
#'
#' @param marks A vector of strictly increasing time points
#' @param lambda Roughness penalty weight
#'
#' @return An object of class fda::fdPar
#' @export
#'
#' @examples
#' targetMarks <- c(0, 1.2, 2, 3.4)
#' fdParObj <- WfdPar_for_reg(targetMarks)
WfdPar_for_reg <- function(marks, lambda = 1e-10) {
  Wbasis <- fda::create.bspline.basis(breaks = marks)
  Wfd <- fda::fd(matrix(0,Wbasis$nbasis,1), Wbasis)
  return(fda::fdPar(Wfd, 2, lambda))
}
