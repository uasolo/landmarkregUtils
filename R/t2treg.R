#' Mapping original time points to their registered position
#'
#' An auxiliary thin wrapper to `fda::eval.fd()`.
#' Given an already computed inverse time warping function `hinv`,
#' map original time points `argvals` to their registered position `hinv`(`argvals`).
#' Values outside the range of `hinv` are mapped to `NA`s.
#'
#' @param argvals A vector of time points
#' @param hinv A `fda::fd` object containing only one curve
#'
#' @return A vector of the same length of `argvals`
#' @export
#'
#' @examples
t2treg <- function(argvals, hinv) {
  inRange <- dplyr::between(argvals, hinv$basis$rangeval[1], hinv$basis$rangeval[2])
  treg <- rep(NA, times = length(argvals))
  treg[inRange] <- fda::eval.fd(argvals[inRange], hinv) %>% as.numeric()
  return(treg)
}
