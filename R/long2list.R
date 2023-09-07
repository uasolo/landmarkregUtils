#' Convert a two-column long form dataframe into a list
#'
#' @param dat A dataframe in long format containing at least two columns
#' @param keys String, the column name that will become the list key
#' @param values String, the column name that will become the value
#'
#' @return A list with as many keys as there are unique values of the `keys` column,
#' each key containing a vector of the corresponding values in the `values` column.
#' @export
#'
#' @examples
long2list <- function(dat, keys, values) {
  dat %>%
    dplyr::select(dplyr::all_of(c(keys, values))) %>%
    tidyr::pivot_wider(names_from = {{keys}}, values_from = {{values}}, values_fn = list) %>%
    as.list() %>%
    unlist(recursive = FALSE, use.names = FALSE)
}
