#' Convert a (irreg)FunData object into a tibble
#'
#' @param fd A (irreg)FunData object
#' @param id Desired column name corresponding to the curve id (default "id")
#' @param time Desired column name corresponding to the time axis (default "time")
#' @param value Desired name corresponding to the values at `time` (default "value")
#'
#' @return A three-column tibble in long format
#' @export
#'
#' @examples
funData2long <- function(fd, id="id", time="time", value="value") {
  if (is.list(fd@X)) {
    id_ <- seq_along(fd@X)
  } else { # matrix
    id_ <- seq_len(fd@X %>% nrow())
  }
  res <- dplyr::tibble(!!{{id}} := factor(id_)) %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::reframe(funData2long1(fd[dplyr::cur_group_id()], {{time}}, {{value}}))
  return(res)
}
