#' Convert a (irreg)FunData object containing only one curve into a tibble
#'
#' @param fd A (irreg)FunData object containing only one curve
#' @param time Desired column name corresponding to the time axis (default "time")
#' @param value Desired name corresponding to the values at `time` (default "value)
#'
#' @return A two-column tibble in long format
#' @export
#'
#' @examples
funData2long1 <- function(fd, time="time", value="value") {
  argvals <- fd@argvals[[1]]
  if (is.list(fd@X)) {
    X <- fd@X[[1]] %>% as.numeric()
  } else { # 1-row matrix
    X <- fd@X %>% as.numeric()
  }
  return(dplyr::tibble(!!{{time}} := argvals[!is.na(X)],
                       !!{{value}} := X[!is.na(X)]))
}
