#' Convert landmark positions to interval durations
#'
#' Given a wide format dataframe listing landmark positions, one row per curve,
#' convert into a long format dataframe of intervals.
#'
#' @param inputMarks A dataframe with landmark information
#' @param id (optional) column namr of `inputMarks` that contains curve index
#' @param targetMarks (optional) The position of landmarks after registration
#'
#' @return A long format dataframe of intervals.
#' @export
#'
#' @examples
#' library(tibble)
#' land <- tribble(
#' ~curveId, ~l1, ~l2, ~l3, ~l4,
#' 1, 0, 0.7, 1, 1.5,
#' 2, 0, 0.5, 1.1, 1.8,
#' 3, 0, 0.6, 1.2, 2
#' )
#' landmarks2durations(land, "curveId")
landmarks2durations <- function(inputMarks, id=NULL, targetMarks=NULL) {
  if (!is.null(id)) {
    if (!(id %in% colnames(inputMarks))) {
      stop(paste0("`id` column name (", id, ") is not a column of `inputMarks`."))
    }
  }
  res <- inputMarks %>%
    {if (is.null(id)) {
      tibble::rowid_to_column(., var = "id") %>%
        dplyr::mutate(id = factor(id))
    } else .} %>%
    {if (is.null(id)) {
      tidyr::pivot_longer(., cols = !id, names_to = "rightBoundary", values_to = "time")
      } else {
    tidyr::pivot_longer(., cols = !dplyr::all_of({{id}}), names_to = "rightBoundary", values_to = "time")
        }} %>%
    {if (is.null(id)) dplyr::group_by(., id) else dplyr::group_by(., dplyr::across(dplyr::all_of({{id}}))) } %>%
    dplyr::mutate(duration = time - dplyr::lag(time)) %>%
    dplyr::filter(!is.na(duration)) %>%
    dplyr::select(!time) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rightBoundary = factor(rightBoundary))

  if (!is.null(targetMarks)) {
    durationReg <- diff(targetMarks)
    res <- res %>%
      dplyr::rename(before = duration) %>%
      {if (is.null(id)) dplyr::group_by(., id) else dplyr::group_by(., dplyr::across(dplyr::all_of({{id}}))) } %>%
      dplyr::mutate(after = durationReg) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(cols = c("before", "after"), names_to = "registration", values_to = "duration") %>%
      dplyr::mutate(registration = factor(registration, levels = c("before", "after")))
  }
  return(res)
}
