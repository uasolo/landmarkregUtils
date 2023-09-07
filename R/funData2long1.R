#' Convert a (irreg)FunData object containing only one curve into a dataframe
#'
#' @param fd A (irreg)FunData object containing only one curve
#' @param columnNames (optional) list indicating the column names corresponding to `fd@argvals` and `fd@X`,
#' indicated by `.index` and `.value`, respectively.
#' If `NULL`, column names are `argvals` and `X`.
#'
#' @return A two-column tibble in long format
#' @export
#'
#' @examples
funData2long1 <- function(fd, columnNames=NULL) {
  argvals <- fd@argvals[[1]]
  if (is.list(fd@X)) {
    X <- fd@X[[1]] %>% as.numeric()
  } else { # matrix
    X <- fd@X %>% as.numeric()
  }
  if (is.null(columnNames)) {
    return(dplyr::tibble(argvals = argvals, X = X))
  } else {
    return(dplyr::tibble(!!{{columnNames$.index}} := argvals,
                  !!{{columnNames$.value}} := X))
  }
}
