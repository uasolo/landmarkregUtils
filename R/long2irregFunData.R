#' Convert a set of curves encoded as long form dataframe to a irregFunData
#'
#' The column names for curve id, time and value have to be specified.
#' If not, they are assumed to be the first three columns in this order: id, time, value.
#'
#' @param df A dataframe containing at least three columns
#' @param id Column name of `df` corresponding to curve index (default: 1st column of `df`)
#' @param time Column name of `df` corresponding to the time axis (default: 2nd column of `df`)
#' @param value Column name of `df` corresponding to the values at `time` (default: 3rd column of `df`)
#'
#' @return An object of class `funData::irregFunData`
#' @export
#'
#' @examples
long2irregFunData <- function(df, id=names(df)[1], time=names(df)[2], value=names(df)[3]) {
  return(
    funData::irregFunData(
      argvals = long2list(df, id, time),
      X = long2list(df, id, value)
    )
  )
}
