#' Convert registered interval to its original duration
#'
#' Given a log rate function, computed with [landmarkreg_nocurves()],
#' convert a time interval (`from`, `to`) specified on the registered time axis
#' back to its original duration
#'
#' @param lograte Either a `fda::fd` or a `funData::funData` object containing only one curve
#' which represents the log rage to be applied
#' @param from The lower interval boundary on the registered time axis
#' @param to The upper interval boundary on the registered time axis
#'
#' @return The duration of the registered interval (`from`, `to`) on the original time axis.
#' @export
#'
#' @examples
lograte2duration <- function(lograte, from, to) {
  if (!(fda::is.fd(lograte) | any(c("irregFunData", "funData") %in% class(lograte)))) {
    stop("lograte should be an object of class fda::fd or funData::funData or funData::irregFunData")
  }
  if (!(is.numeric(from) & is.numeric(to))) {
    stop("`from` and `to` should be numeric values")
  }
  if (from > to) {
    stop(paste0("The lower boundary `from` (",
                from,
                ") should not be greater than the upper boundary `to` (",
                to,
                ") ."))
  }
  if (fda::is.fd(lograte)) {
    minReg <- lograte$basis$rangeval[1]
    maxReg <- lograte$basis$rangeval[2]
  } else {
    minReg <- range(lograte@argvals[[1]])[1]
    maxReg <- range(lograte@argvals[[1]])[2]
  }
  if (!dplyr::between(from, minReg, maxReg)) {
    stop(paste0("Argument `from` is outside the boundaries defined by `lograte`: (",
         minReg, ", ", maxReg, ")"))
  }
  if (!dplyr::between(to, minReg, maxReg)) {
    stop(paste0("Argument `to` is outside the boundaries defined by `lograte`: (",
                minReg, ", ", maxReg, ")"))
  }
  if (from == to) {
    return (0)
  }
  if (fda::is.fd(lograte)) {
    lograte <- funData::fd2funData(lograte[1], seq(minReg, maxReg, length.out = 100))
  }
  return(
    lograte[1] %>% `*`(-1) %>% exp() %>% funData::funData2fd() %>% defint_fd(c(from, to))
  )
}
