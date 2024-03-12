#' Landmark registration directly on time samples
#'
#' Landmark registration applied directly to the input time axis samples.
#' Both input and target landmarks must start from 0.
#'
#'
#' @param timeSamples Vector (not matrix) of time input samples (e.g. regularly spaced)
#' @param inputMarks Numeric vectors of increasing time values, starting with 0.
#' @param targetMarks Numeric vectors of increasing time values, starting with 0.
#' @param WfdPar (optional, see `one_landmarkreg_nocurves`)
#' @param wlambda (optional, see `one_landmarkreg_nocurves`)
#'
#' @return h(`timeSamples`), i.e. a vector of the same length as `timeSamples` with time-registered values
#' corresponding to `timeSamples`
#' @export
#'
#' @examples
#' t <- seq(0,2, by=0.1)
#' x <- t **2 - 0.5 * t
#' inputMarks <- c(0, 1, 1.5, 2)
#' targetMarks <- c(0, 0.5, 1, 1.3)
#' t_reg <- landmarkreg_timeSamples(t, inputMarks, targetMarks)
#' plot(t, x, type="b", pch=19, col="red")
#' lines(t_reg, x, pch=18, col="blue", type="b", lty=2, xlab="t")
#' legend(0, 3, legend = c("original", "registered"), col=c("red", "blue"), lty=1:2, cex=0.8)
landmarkreg_timeSamples <- function(timeSamples, inputMarks, targetMarks, WfdPar=NULL, wlambda=1e-14) {
  h <- one_landmarkreg_nocurves(inputMarks, targetMarks, WfdPar, wlambda)
  hinv_fd <- fda::Data2fd(h, matrix(seq(0, max(targetMarks), length.out = length(h))))
  return(t2treg(timeSamples, hinv_fd))
}
