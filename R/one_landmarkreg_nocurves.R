#' Compute one landmark-based time warping function
#'
#' Computes the time warping function h(t) so that input landmarks are moved to corresponding target landmarks.
#' Both input and target landmarks are first linearly mapped to (0,1),
#' then the code computes h(t) samples using the same procedure used in fda::landmarkreg.
#' Both input and target landmarks must start from 0.
#' The scaling to (0,1) (and reverse scaling at the end) is necessary
#' as smooth.morph does not yield the expected results
#' when operating on other intervals, e.g. when they differ too much from one another.

#'
#' @param inputMarks Numeric vectors of increasing time values, starting with 0.
#' @param targetMarks Numeric vectors of increasing time values, starting with 0.
#' @param WfdPar (optional) fdPar object to compute the warping, it should be defined on the (0,1) range
#' @param wlambda (optional) lambda for the construction of WfdPar
#'
#' @return A vector of samples of h(t), whose t values are (implicitly) given as regularly spaced
#' between 0 and the last value in targetMarks.
#' The first value of h(t) is 0, the last is the same as the last value in inputMarks.
#' The number of samples is large enough to provide for a smooth interpolation
#' operated by calling functions like landmarkreg_nocurve.
#' @export
#'
#' @examples
#' inputMarks <- c(0, 1, 1.5, 2)
#' targetMarks <- c(0, 0.5, 1, 1.3)
#' h <- one_landmarkreg_nocurves(inputMarks, targetMarks)
#' plot(h, type = 'l', xlab = 'registered time', ylab = 'original time')
one_landmarkreg_nocurves <- function(inputMarks, targetMarks, WfdPar=NULL, wlambda=1e-14) {
  if (length(inputMarks) != length(targetMarks)) {
    stop("The same number of input and target lanmarks should be provided")
  }
  if (inputMarks[1] != 0 | targetMarks[1] != 0) {
    stop("First landmark must be 0 for both input and target")
  }
  inputMax <- max(inputMarks)
  inputMarks <- inputMarks / inputMax

  targetMax <- max(targetMarks)
  targetMarks <- targetMarks / targetMax

  if (is.null(WfdPar)) {
    WfdPar <- WfdPar_for_reg(targetMarks, wlambda)
  }
  nWbasis <- WfdPar$fd$basis$nbasis
  nfine   <- min(c(101,10*nWbasis))
  xfine <- seq(0, 1, length=nfine)
  if (all(inputMarks == targetMarks)) {
    # no need to perform registration; smooth.morph would throw an error
    return(xfine * inputMax)
  }
  Wfd  <- fda::smooth.morph(targetMarks, inputMarks, c(0,1), WfdPar)$Wfdobj
  hfun <- fda::monfn(xfine, Wfd)
  b    <- 1/(hfun[nfine]-hfun[1])
  a    <- - b*hfun[1]
  hfun <- a + b*hfun
  hfun[c(1,nfine)] <- c(0,1)
  return(hfun * inputMax)
}
