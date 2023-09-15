#' Apply already computed time warping to a set of curves
#'
#' Given a set of curves and a corresponding set of time warping functions h(t),
#' typically computed with [landmarkreg_nocurves()], obtain the transformed curves.
#' The curves can be encoded either as long form dataframe or as (`irreg`)`funData`.
#' The result is provided in the same format as the input.
#' A desired time point grid on the registered time axis can be provided,
#' otherwise the time points for each curve are obtained by mapping the original ones according to the respective h(t).
#'
#' @param dat Either a long form dataframe or a (`*`)`funData` object containing a data set of curves.
#' In the dataframe case, it should have at least three columns mapping to `id` (the curve ID),
#'  `time` (the time sample) and `value` (the corresponding value).
#' The dataframe is assumed to be ordered by `id` and within `id` by `time`.
#' The order of `id` should match the order of h(t) in `reg`.
#' In the case of (`*`)`funData`, the order of stored curves (in `@X`) should match the order of h(t) in `reg`.
#'
#' @param reg A list with at least key `h`, containing the result of landmark registration operated by [landmarkreg_nocurves()].
#' @param grid A vector of time samples defined on the registered time axis.
#' If specified, the value of the curves in `dat` at those time points are computed,
#' which entails an interpolation of the original curves (via `fda::Data2fd()`).
#' If `NULL`, the registered curve values will be computed at the respective h(t),
#'  i.e. at the time points on the registered time axis corresponding to those provided in `dat`.
#'  In the latter case, `reg` should include the `hinv` entry.
#' @param id Column name of `dat` corresponding to curve index (default: 1st column).
#' Not used if `dat` is a (`*`)`funData`.
#' @param time Column name of `dat` corresponding to the time axis (default: 2nd column).
#' Not used if `dat` is a (`*`)`funData`.
#' @param value Column name(s) of `dat` corresponding to the values at `time`.
#' For multi-dimensional data, a vector of strings (default: columns from 3rd to last).
#' Not used if `dat` is a (`*`)`funData`.
#'
#' @return The registered curves in the same format as `dat`.
#' @export
#'
#' @examples
applyReg <- function(dat, reg, grid=NULL, id=NULL, time=NULL, value=NULL) {
  # Checks
  if (!(any(c("data.frame", "irregFunData", "funData", "multiFunData") %in% class(dat)))) {
    stop(paste0("dat is of class ", class(dat), ", which is neither a dataframe nor a funData object."))
  }
  if (!is.list(reg)) {
    stop(paste0("reg is of class ", class(reg), ". It should be a list where $h is a fd object"))
  }
  if (!("h" %in% names(reg))) {
    stop("h does not contain the $h key.")
  }
  if (!fda::is.fd(reg$h)) {
    stop(paste0("reg$h is of class ", class(reg$h), ". It should be of class fda::fd"))
  }
  maxT <- reg$h$basis$rangeval[2]
  if (is.data.frame(dat)) {
    colnames_ <- colnames(dat)
    if (is.null(id)) id <- colnames_[1]
    if (is.null(time)) time <- colnames_[2]
    if (is.null(value)) value <- colnames_[-c(1,2)]
    if (!all(c(id, time, value) %in% colnames_)) {
      stop("One or more of the arguments id, time and value do not match any dat column names.")
    }
    ndat_ <- dat %>% dplyr::pull({{id}}) %>% dplyr::n_distinct()
  } else if (any(c("irregFunData", "funData", "multiFunData") %in% class(dat))) {
    ndat_ <- funData::nObs(dat)
    id <- "id"
    time <- "time"
    if ("multiFunData" %in% class(dat)) {
      value <- paste0("value", seq_along(dat))
    } else {
      value <- "value"
    }
  }
  if (ndat_ != (nreg_ <- ncol(reg$h$coefs))) {
    stop(paste0("The number of observations (curves) in dat (", ndat_,
                ") should match the number of warping functions in reg (", nreg_, ")."))
  }
  if (!is.null(grid)) {
    if (!(min(grid) >= 0) | !(max(grid) <= maxT)) {
      stop(paste("One or more grid values are out of the bounds specified in reg: 0,", maxT))
    }
  }
  if (is.null(grid) & !("hinv" %in% names(reg)) ) {
    stop("reg object does not contain hinv. Re-run landmarkreg_nocurves() setting compute_hinv = TRUE")
  }
  # Format and dispatch to applyRegDf()
  # datList is of length = number of curve dimensions
  if ("multiFunData" %in% class(dat)) {
    datList <- dat
  } else if (any(c("irregFunData", "funData") %in% class(dat))) {
    datList <- list(dat)
  } else { # dataframe
    datList <- lapply(value, function(v) {
      dat %>%
        dplyr::select(dplyr::all_of(c({{id}}, {{time}}, {{v}})))
    })
  }

  # resList contains the results by dimension
  # (irreg)FunData objects are converted to tibbles then reconverted back
  # after applyRegDf() is called on them
  resList <- lapply(seq_along(datList), function(i) {
    if (any(c("irregFunData", "funData") %in% class(datList[[i]]))) {
      df <- funData2long(datList[[i]], id = id, time = time, value = value[i])
    } else {
      df <- datList[[i]]
    }
    res <- applyRegDf(df, reg, grid, id, time, value = value[i])
    if (any(c("irregFunData", "funData") %in% class(datList[[i]]))) {
      res <- long2irregFunData(res)
      if (!is.null(grid) | "multiFunData" %in% class(dat)) {
        res <- funData::as.funData(res)
      }
    }
    return(res)
  })
  if (length(resList) == 1) {
    return(resList[[1]])
  } else {
    if ("data.frame" %in% class(dat)) {
      return(
        purrr::reduce(resList, dplyr::left_join, by = c({{id}}, {{time}}))
      )
    } else {
      return(funData::multiFunData(resList))
    }
  }

  return(0)
}
