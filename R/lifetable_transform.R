#' Rebase Life Table to Different Age
#'
#' @examples
#' pash = lxToPash(x = prestons_lx$x, lx = prestons_lx$lx)
#' RebaseLT(pash, 50)
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
  pash[["lt"]]$nLx = pash[["lt"]]$nx*(pash[["lt"]]$lx-pash[["lt"]]$ndx) + pash[["lt"]]$nax*pash[["lt"]]$ndx
  # rebase Tx
  pash[["lt"]]$Tx = rev(cumsum(rev(pash[["lt"]]$nLx)))
  # rebase ex
  pash[["lt"]]$ex = pash[["lt"]]$Tx / pash[["lt"]]$lx

  return(pash)
}

#' Standardize Life Table By Pace and Shape
#'
#' @examples
#' pash = lxToPash(x = prestons_lx$x, lx = prestons_lx$lx)
#' StandardizeLT(pash)
#'
#' @export
StandardizeLT <- function(pash) {

  lt = pash[["lt"]]

  # standardize age
  x_s =  lt$x  / lt$ex[1]
  nx_s = lt$nx  / lt$ex[1]
  # standardize nmx
  nmx_s = lt$nmx*lt$ex[1]
  # standardize ex
  ex_s = lt$ex / lt$ex[1]

  # return standardized LT
  return(
    data.frame(x = lt$x, nx = lt$nx, x_s, nx_s, nmx_s,
               lx_s  = lt$lx, ex_s)
  )
}
