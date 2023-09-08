#' Apply already computed time warping to a set of uni-dimensional curves in dataframe long format
#'
#' Given a set of curves and a corresponding set of time warping functions,
#' typically computed with [landmarkreg_nocurves()], obtain the transformed curves.
#' The curves are encoded in long form dataframe.
#' The result is provided in the same format.
#' A desired time point grid on the registered time axis can be provided,
#' otherwise the time points for each curve are obtained by mapping the original ones according to the respective h(t).
#'
#' This function does not check its arguments nor it provides default values.
#' The user is advised to use [applyReg()] instead, which calls this function internally.
#'
#' @param df A dataframe containing at least three columns
#' @param reg A list with at least key `h`, containing the result of landmark registration operated by [landmarkreg_nocurves()].
#' @param grid A vector of time samples defined on the registered time axis.
#' If specified, the value of the curves in `df` at those time points will be computed,
#' which entails an interpolation of the original curves (via `fda::Data2fd()`).
#' If `NULL`, the registered curve values will be computed at the respective h(t),
#' i.e. at the time points on the registered time axis corresponding to those provided in `df`.
#' In the latter case, `reg` should include the `hinv` entry.
#' @param id Column name of `df` corresponding to curve index
#' @param time Column name of `df` corresponding to the time axis
#' @param value Column name of `df` corresponding to the values at `time`
#'
#' @return A tibble containing the registered curves.
#' The tibble has three columns named as specified by `id`, `time` and `value`.
#' If `grid` is `NULL`, the tibble has the same number of rows as `df`,
#' its `id` and `value` columns are the same as their namesake in `df`,
#' the `time` column contains the warped time values corresponding to their namesake in `df`,
#' i.e. hinv(`time`).
#' If `grid` is specified, each curve is evaluated at `time` points specified by `grid`
#' (hence the tibble has `length(grid) * length(unique(id))` rows and three columns).
#' @export
#'
#' @examples
applyRegDf <- function(df, reg, grid=NULL, id, time, value) {
  # The renaming is meant to avoid the scoping problem created by get()
  # https://stackoverflow.com/questions/58216754/invalid-first-argument-in-get-when-using-data-table-in-lapply

  if (is.null(grid)) {
    return(
      df %>%
        dplyr::rename(id_ = {{id}}, time_ = {{time}}, value_ = {{value}}) %>%
        dplyr::mutate(id_ = id_ %>% factor(levels = unique(.))) %>%
        dplyr::group_by(id_) %>%
        dplyr::mutate(treg = t2treg(time_, reg$hinv[[dplyr::cur_group_id()]])) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(c("id_", "treg", "value_"))) %>%
        dplyr::rename(!!{{id}} := id_, !!{{time}} := treg, !!{{value}} := value_)
    )
  } else {
    return(
      df %>%
        dplyr::rename(id_ = {{id}}, time_ = {{time}}, value_ = {{value}}) %>%
        dplyr::mutate(ID_ = id_ %>% factor(levels = unique(.))) %>%
        dplyr::group_by(ID_, id_) %>%
        dplyr::reframe(treg = {{grid}},
                       valuereg = val_at_treg(time_, value_, {{grid}}, reg$h[dplyr::cur_group_id()])) %>%
        dplyr::select(!ID_) %>%
        dplyr::rename(!!{{id}} := id_, !!{{time}} := treg, !!{{value}} := valuereg)
    )
  }
}
