#' Convert wide form landmark table into long form boundaries or durations
#'
#' Given a wide format dataframe listing landmark positions, one row per curve,
#' convert into a long format dataframe of either boundary pairs (from, to) or interval durations.
#'
#' @param landmarks Either a wide form dataframe or a vector.
#' If a vector, it is considered a landmark sequence for a single curve.
#' @param id Column name of `landmarks` that contains curve index, if present.
#' @param form Either "boundaries" or "duration", depending on the desired output form.
#'
#' @return A long format dataframe of interval boundaries or durations.
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
#' landmarks2long(land, "curveId")
landmarks2long <- function(landmarks, form="boundaries", id=NULL) {
  if (is.vector(landmarks)) {
    landmarks <- landmarks %>%
      as.list() %>%
      `names<-`(paste0("l", seq_along(.))) %>%
      tibble::as_tibble()
  }
  if (!is.null(id)) {
    if (!(id %in% colnames(landmarks))) {
      stop(paste0("`id` column name (", id, ") is not a column of `landmarks`."))
    }
  }
  res <- landmarks %>%
    {if (is.null(id)) {
      tibble::rowid_to_column(., var = "id") %>%
        dplyr::mutate(id = factor(id))
    } else .} %>%
    {if (is.null(id)) {
      tidyr::pivot_longer(., cols = !id, names_to = "leftBoundary", values_to = "from")
      } else {
    tidyr::pivot_longer(., cols = !dplyr::all_of({{id}}), names_to = "leftBoundary", values_to = "from")
        }} %>%
    {if (is.null(id)) dplyr::group_by(., id) else dplyr::group_by(., dplyr::across(dplyr::all_of({{id}}))) } %>%
    dplyr::mutate(to = dplyr::lead(from)) %>%
    {if (grepl('duration', form)) dplyr::mutate(., duration = to - from) else .} %>%
    dplyr::filter(!is.na(to)) %>%
    {if (grepl('duration', form)) dplyr::select(., !c(from, to)) else . } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(leftBoundary = factor(leftBoundary))

  return(res)
}
