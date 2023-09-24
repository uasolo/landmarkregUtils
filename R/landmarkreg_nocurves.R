#' Compute several landmark-based time warping function
#'
#' Applies `one_landmarkreg_nocurves` to each row of `inputMarks`.
#' Returns all h(t) and log rates in `fda::fd` form.
#' The number of basis for these `fd` objects is very large to allow precise interpolation
#' and avoid having spurious negative values in dh/dt at any point in time.
#' Anyway h is not strictly guaranteed monotonic
#' (contrary to its samples coming from `one_landmarkreg_nocurves`)
#' and its extremes are not precise.
#' If `compute_hinv=TRUE`, returns also all h_inv(t) (TODO: as well as a matrix of errors
#' as difference between `targetMarks` and the actual registered positions).
#'
#' @param inputMarks Matrix or dataframe, each row obeys the same conventions as the namesake
#' argument in `one_landmarkreg_nocurves`
#' @param targetMarks Same as in `one_landmarkreg_nocurves`, all `inputMarks` have the same target.
#' If `NULL`, it is set as the mean by column of `inputMarks`.
#' @param compute_hinv If `TRUE` return hinv, the inverse function of h(t)
#' @param njobs If > 1, use a parallel procedure
#' @param WfdPar (optional, see `one_landmarkreg_nocurves`)
#' @param wlambda (optional, see `one_landmarkreg_nocurves`)
#'
#' @return A list with the following keys:
#'
#' `h`: a `fd` object with as many h(t) as the number of rows of `inputMarks`
#'
#' `lograte`: - log (dh/dt)
#'
#' `landmarks`: location of target landmarks
#'
#' `hinv`: (optional) a list of `fd` objects, one for each row of `inputMarks`
#'
#' @export
#'
#' @examples
#' inputMarks <- matrix(c(0, 0.5, 1, 2, 0, 0.7, 1.2, 1.9, 0, 0.4, 1.1, 2.2), nrow=3, byrow = TRUE)
#' reg <- landmarkreg_nocurves(inputMarks)
landmarkreg_nocurves <- function(inputMarks, targetMarks=NULL, compute_hinv=TRUE, njobs=1,
                                 WfdPar=NULL, wlambda=1e-14) {
  inputMarks <- as.matrix(inputMarks)
  # checks
  l1_not_zeros <- which(inputMarks[,1] != 0)
  if (length(l1_not_zeros) > 0) {
    stop(paste("row(s)",
               paste(l1_not_zeros, collapse = ", "),
               "of inputMarks have nonzero first landmarks"))
  }
  non_increasing <- which(t(apply(inputMarks, 1, diff)) <= 0, arr.ind = TRUE)
  if (nrow(non_increasing) > 0) {
    print(non_increasing)
    stop("The inputMarks positions above are not strictly increasing")
  }
  na <- which(is.na(inputMarks), arr.ind = TRUE)
  if (nrow(na) > 0) {
    print(na)
    stop("The inputMarks positions above contain NAs")
  }

  if (!is.null(targetMarks)) {
    if (targetMarks[1] != 0) {
      stop("The first targetMarks should be zero.")
    }
    non_increasing <- which(diff(targetMarks) <= 0)
    if (length(non_increasing) > 0) {
      stop(paste(
        "The targetMarks position(s)",
        paste(non_increasing, collapse = ", "),
        "are not strictly increasing"
      ))
    }
    na <- which(is.na(targetMarks))
    if (length(na) > 0) {
      stop(paste(
        "The targetMarks position(s)",
        paste(na, collapse = ", "),
        "are NAs"
      ))
    }
  }

  if (is.null(targetMarks)) {
    targetMarks <- apply(inputMarks, 2, mean)
  }

  if (njobs == 1) {
    foreach::registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(njobs)
    doParallel::registerDoParallel(cl)
  }
  if (!is.null(WfdPar)) {
    WfdPar <- WfdPar_for_reg(targetMarks/max(targetMarks), wlambda)
  }
  # using iterators::iter() would be nicer, but there's a bug in interaction with RStudio:
  # https://stackoverflow.com/questions/70230595/r-iterator-example-crashes
  hmat <- foreach::`%dopar%`(
   foreach::foreach(i = seq_len(nrow(inputMarks)),
                  .combine = 'cbind',
                  .export = 'one_landmarkreg_nocurves',
                  .packages = c('fda') ),  {
                    one_landmarkreg_nocurves(inputMarks[i,], targetMarks, WfdPar, wlambda)
                  }
  )
  if (njobs > 1) {
    parallel::stopCluster(cl)
  }
  Lfdobj <- 3 # normally 2, here +1 as first deriv. needed
  nOrder <- 2 + Lfdobj
  nBasis <- round(nrow(hmat)/3) # many but not as many as the time samples
  hbasis <- fda::create.bspline.basis(rangeval = range(targetMarks), nbasis = nBasis, norder = nOrder)
  fdParobj <- fda::fdPar(fda::fd(matrix(0,hbasis$nbasis,1), hbasis))
  hx <- seq(0, max(targetMarks), length.out = nrow(hmat))
  h <- fda::smooth.basis(argvals = hx, y = hmat, fdParobj = fdParobj)$fd
  lograte <- fda::smooth.basis(hx, - log(fda::eval.fd(hx, h ,Lfdobj = 1)) ,fdParobj)$fd
  reg <- list(h = h, lograte = lograte, landmarks = targetMarks)
  if (compute_hinv) {
    hinv <- lapply(seq_len(ncol(hmat)), function(i) {
      fda::Data2fd(hmat[,i], hx)
    })
    reg$hinv <- hinv
  }
  return(reg)
}
