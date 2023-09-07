#' Convert a set of curves encoded as long form dataframe to a irregFunData
#'
#' @param df A dataframe containing at least three columns
#' @param columnNames A list with three entries: `.obs`, `.index` and `.value`,
#' the values of which are the column names of `df` corresponding to
#' curve ID, time samples and sample values, respectively.
#'
#' @return An object of class `funData::irregFunData`
#' @export
#'
#' @examples
long2irregFunData <- function(df, columnNames) {
  return(
    irregFunData(
      argvals = long2list(df, columnNames$.obs, columnNames$.index),
      X = long2list(df, columnNames$.obs, columnNames$.value)
    )
  )
}
