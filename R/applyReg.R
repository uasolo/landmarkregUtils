#' Apply already computed time warping to a set of curves
#'
#' Given a set of curves and a corresponding set of time warping functions,
#' typically computed with [landmarkreg_nocurves()], obtain the transformed curves.
#' The curves can be encoded in long form dataframe or as (`irreg`)`funData`.
#' The result is provided in the same format as the input.
#' A desired time point grid on the registered time axis can be provided,
#' otherwise the time points for each curve are obtained by mapping the original ones according to the respective h(t).
#'
#' @param dat Either a long form dataframe or a (`irreg`)`funData` object containing a data set of curves.
#' In the dataframe case, it should have at least three columns mapping to `.obs` (the curve ID), `.index` (the time sample) and `.value` (the corresponding value).
#' The dataframe is assumed to be ordered by `.obs` and within `.obs` by `.index` and `.value`.
#' The order of `.obs` should match the order of h(t) in `reg`.
#' In the case of (`irreg`)`funData`, the order of stored curves (in `@X`) should match the order of h(t) in `reg`.
#'
#' @param reg A list with at least key `h`, containing the result of landmark registration operated by [landmarkreg_nocurves()].
#' @param grid A vector of time samples defined on the registered time axis.
#' If specified, the value of the curves in `dat` at those time points will be computed,
#' which entails an interpolation of the original curves (via `fda::Data2fd()`).
#' If `NULL`, the registered curve values will be computed at the respective h(t),
#'  i.e. at the time points on the registered time axis corresponding to those provided in `dat`.
#'  In the latter case, `reg` should include the `hinv` entry.
#' @param columnNames Mandatory when `dat` is a dataframe, disregarded otherwise.
#' A list with three entries: `.obs`, `.index` and `.value`, the values of which are the column names of `dat` corresponding to
#' curve ID, time samples and sample values, respectively.
#'
#' @return The registered curves in the same format as `dat`.
#' @export
#'
#' @examples
applyReg <- function(dat, reg, grid=NULL, columnNames=NULL) {
  maxT <- reg$h$basis$rangeval[2]
  # checks
  if (is.data.frame(dat)) {
    if (!is.list(columnNames)) {
      stop("When dat is a data.frame, column should be a list with keys: .obs, .index, .value.")
    }
    if (!all(c(".obs", ".index", ".value") %in% names(columnNames))) {
      stop("When dat is a data.frame, columnNames should be a list with keys: .obs, .index, .value.")
    }
    if ((ndat_ <- dat %>% dplyr::pull(columnNames$.obs) %>% dplyr::n_distinct()) != (nreg_ <- ncol(reg$h$coefs))) {
      stop(paste0("The number of observations (curves) in dat (", ndat_,
      ") should match the number of warping functions in reg (", nreg_, ")."))
    }
  }
  if (!is.null(grid)) {
    if (!(min(grid) >= 0) | !(max(grid) <= maxT)) {
      stop("One or more grid values are out of the bounds specified in reg")
    }
  }
  if (is.null(grid) & !("hinv" %in% names(reg)) ) {
    stop("reg object does not contain hinv. Re-run landmarkreg_nocurves() setting compute_hinv = TRUE")
  }
  # computation
  if (is.data.frame(dat)) {
    if (is.null(grid)) {
      return(
        dat %>%
          dplyr::mutate(ID_ = .data[[columnNames$.obs]] %>% factor(levels = unique(.))) %>%
          dplyr::group_by(ID_) %>%
          dplyr::mutate(treg = t2treg(.data[[columnNames$.index]], reg$hinv[[dplyr::cur_group_id()]])) %>%
          dplyr::ungroup() %>%
          dplyr::select(!ID_)
      )
    } else {
      return(
        dat %>%
          dplyr::mutate(ID_ = .data[[columnNames$.obs]] %>% factor(levels = unique(.))) %>%
          dplyr::group_by(ID_, .data[[columnNames$.obs]]) %>%
          dplyr::reframe(treg = {{grid}},
                         valuereg = val_at_treg(.data[[columnNames$.index]],
                                                .data[[columnNames$.value]],
                                                {{grid}},
                                                reg$h[dplyr::cur_group_id()])) %>%
          dplyr::select(!ID_)

      )
    }
  }


  return(0)
}
