#' Mapping original time points to their registered position
#'
#' An auxiliary thin wrapper to `fda::eval.fd()`.
#' Given an already computed inverse time warping function `hinv`,
#' map original time points `argvals` to their registered position `hinv`(`argvals`).
#'
#' @param argvals A vector of time points
#' @param hinv A `fda::fd` object containing only one curve
#'
#' @return A vector of the same length of `argvals`
#' @export
#'
#' @examples
t2treg <- function(argvals, hinv) {
  return(fda::eval.fd(argvals, hinv) %>% as.numeric())
}
