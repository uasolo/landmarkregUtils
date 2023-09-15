#' Definite integral
#'
#' Compute a definite integral for each of the uni-dimensional functions
#'   in fdobj from rng[1] to rng[2].
#'
#' @param fdobj Object of class `fda::fd` containing uni-dimensional curve(s) to integrate
#' @param rng Numeric vector of length 2 specifying the limits of integration.
#' If `NULL`, the entire range of `fdobj` is used.
#'
#' @return A numerical vector of length = number of functions in `fdobj`
#' containing the integral values.
#' @export
#'
#' @examples
defint_fd <- function(fdobj, rng=NULL) {
  if (is.null(rng)) { rng = fdobj$basis$rangeval }
  return (apply(fda::inprod(fdobj,fdobj$basis,rng=rng),1,sum))
}
