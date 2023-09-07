#' Convert a (irreg)FunData object into a dataframe
#'
#' @param fd A (irreg)FunData object
#' @param columnNames (optional) list indicating the column names corresponding to `fd@argvals`, `fd@X`,
#' and the curve ID, indicated by `.index`, `.value` and `.obs`, respectively.
#' If `NULL`, column names are `argvals`, `X` and `ID`.
#'
#' @return A three-column tibble in long format
#' @export
#'
#' @examples
funData2long <- function(fd, columnNames=NULL) {
  if (is.list(fd@X)) {
    ID <- seq_along(fd@X)
  } else { # matrix
    ID <- seq_len(fd@X %>% nrow())
  }
  res <- dplyr::tibble(ID = ID) %>%
    dplyr::group_by(ID) %>%
    dplyr::reframe(funData2long1(fd[dplyr::cur_group_id()], columnNames)) %>%
    dplyr::mutate(ID = factor(ID))
  if (is.null(columnNames)) {
    return(res)
  } else {
    return(
      res %>%
        dplyr::rename(!!{{columnNames$.obs}} := ID)
    )
  }
}
