#' Convert a set of curves encoded as long form dataframe to an irregFunData object
#'
#' The long form dataframe should have at least three columns: curve id, time and value.
#' Their names have to be specified, else they are assumed to be the first three columns in this order: id, time, value.
#' Time and value specify samples for each curve, curve id identifies the curves.
#' The conversion to `funData::irregFunData` places the first curve at `argvals[[1]]`, `X[[1]]`,
#' the second curve at `argvals[[2]]`, `X[[2]]`, etc., using the order of appearance of curve id in the dataframe,
#' i.e. not alphabetic or factor order.
#' Note that the original id's are not stored in the `irregFunData` object.
#'
#'
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
