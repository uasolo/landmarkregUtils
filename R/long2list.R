#' Convert a two-column long form dataframe into a list
#'
#' @param df A dataframe in long format containing at least two columns
#' @param keys String, the column name that will become the list key (default: 1st column of `df`)
#' @param values String, the column name that will become the value (default: 2nd column of `df`)
#'
#' @return A list with as many keys as there are unique values of the `keys` column,
#' each key containing a vector of the corresponding values in the `values` column.
#' @export
#'
#' @examples
long2list <- function(df, keys=names(df)[1], values=names(df)[2]) {
  df %>%
    dplyr::select(dplyr::all_of(c(keys, values))) %>%
    tidyr::pivot_wider(names_from = {{keys}}, values_from = {{values}}, values_fn = list) %>%
    as.list() %>%
    unlist(recursive = FALSE, use.names = FALSE)
}
