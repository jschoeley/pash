#' Rebase Life-table to Different Age
#'
#' Start the life-table at a later age.
#'
#' @details
#' The life table is truncated to the new origin age and the lx at origin set to
#' 1. ndx, nLx, Tx and ex columns are re-calculated.
#'
#' @param pash A pace-shape object.
#' @param origin The age the life table should be rebased to.
#'
#' @return A pace-shape object.
#'
#' @examples
#' # generate pace-shape object
#' pash <- Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' # rebase life table to age 50
#' lt50 <- RebaseLT(pash, 50)
#' lt50
#'
#' # the rebase operation is reversible
#' lt0 = RebaseLT(lt50, 0)
#' lt0
#'
#' @export
RebaseLT <- function (pash, origin = 0) {

  TestClass(pash)

  # get info from non-destructive copy of pash object life table
  # so that rebase is reversible
  lt = attr(pash, "non_destructive_copy")
  # drop age intervals smaller than origin
  pash[["lt"]] = lt[lt$x >= origin,]
  # rebase lx
  pash[["lt"]]$lx = pash[["lt"]]$lx / pash[["lt"]]$lx[1]
  # rebase ndx
  pash[["lt"]]$ndx = pash[["lt"]]$ndx / sum(pash[["lt"]]$ndx)
  # rebase nLx
  pash[["lt"]]$nLx = pash[["lt"]]$nx*(pash[["lt"]]$lx-pash[["lt"]]$ndx) +
    pash[["lt"]]$nax*pash[["lt"]]$ndx
  # rebase Tx
  pash[["lt"]]$Tx = rev(cumsum(rev(pash[["lt"]]$nLx)))
  # rebase ex
  pash[["lt"]]$ex = pash[["lt"]]$Tx / pash[["lt"]]$lx

  return(pash)
}

#' Standardize Life-table By Pace
#'
#' @param pash A pace-shape object.
#' @param pace Measure of pace used for standardization (default e0).
#' @param q Quantile specification for age where q percent of the life-table
#'   population is still alive (defaults to median).
#'
#' @return A pace standardized life table in form of a data frame. Note
#' that this is not a pace-shape object anymore.
#'
#' @source Wrycza, Tomasz F., Trifon I. Missov, and Annette Baudisch. 2015.
#'   "Quantifying the Shape of Aging." PLOS ONE 10 (3): 1–18.
#'   doi:10.1371/journal.pone.0119163.
#'
#' @examples
#' # generate a pace-shape object
#' pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' # standardize the life table by pace and shape
#' StandardizeLT(pash)
#' StandardizeLT(pash, pace = "qlx", q = 0.9)
#'
#' @export
StandardizeLT <- function(pash, pace = "e0", q = 0.5) {

  lt = pash[["lt"]]
  if (identical(pace, "e0")) {pace = TotalLifeExpectancy(lt$ex)}
  if (identical(pace, "qlx")) {
    pace = SurvivalQuantile(lt$x, lt$nx, lt$lx, q,
                            nax_mode = attr(pash, "nax_mode"))
  }

  # standardize age
  x_s =  lt$x / pace
  nx_s = lt$nx / pace
  # standardize nmx
  nmx_s = lt$nmx*pace
  # standardize ex
  ex_s = lt$ex / pace

  # return standardized LT
  return(
    data.frame(x = lt$x, nx = lt$nx, x_s, nx_s, nmx_s,
               lx_s  = lt$lx, ex_s)
  )
}
