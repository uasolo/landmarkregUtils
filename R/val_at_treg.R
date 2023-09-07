#' Compute values of a curve corresponding to points on a time-warped axis
#'
#' Given a curve defined by (`argvals`, `vals`) pairs, and given an already computed
#' time warping function `h` (mapping registered time to original time),
#' compute the values of the curve at time points `argvalreg` on the time warped axis.
#' The computation entails an interpolation of the original curve via `fda::Data2fd()`.
#'
#' The range of `argvals` implicitly defines the expected range (co-domain) of `h`.
#' In case the min or max values of `h`(`argvalreg`) exceed the range of `h`
#' (because of rounding errors produced in [landmarkreg_nocurves()]),
#' these extremes are substituted into `argvals`.
#' This is an attempt to prevent common crashes due to approximations in `h`,
#' typically when the value `h`(0) is slightly below zero.
#'
#' @param argvals A vector of original time points
#' @param vals A vector of values corresponding to `argvals`
#' @param argvalreg A vector of time points on the warped time axis,
#' unrelated to `argvals` either in number or values
#' @param h A `fda::fd` object containing only one curve
#'
#' @return A vector of curve values of the same length as `argvalreg`
#' @export
#'
#' @examples
val_at_treg <- function(argvals, vals, argvalreg, h) {
  argvalreg_inv <- fda::eval.fd(argvalreg, h) %>% as.numeric()
  if (min(argvalreg_inv) < min(argvals)) {
    argvals[1] <- min(argvalreg_inv)
  }
  if (max(argvalreg_inv) > max(argvals)) {
    argvals[length(argvals)] <- max(argvalreg_inv)
  }
  yfd <- fda::Data2fd(argvals, vals)
  return(fda::eval.fd(argvalreg_inv, yfd[1]) %>% as.numeric())
}
