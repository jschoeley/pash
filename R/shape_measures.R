#' Life Table Shape Measures
#'
#' Get life table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which shape measure should be returned (see details for choices)?
#'
#' @details
#' The type argument accepts the following strings:
#' \describe{
#'   \item{\code{"LTentr"}}{Life table entropy.}
#'   \item{\code{"LTgini"}}{Life table Gini coefficient.}
#'   \item{\code{"LTcv"}}{Life table coefficient of variation.}
#'   \item{\code{"all"}}{All of the above measures.}
#' }
#'
#' @source Wrycza, Tomasz F., Trifon I. Missov, and Annette Baudisch. 2015.
#' "Quantifying the Shape of Aging." PLOS ONE 10 (3): 1-18. doi:10.1371/journal.pone.0119163.
#'
#' @examples
#' pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetShape(pash)
#'
#' @export
#' @examples
#' load(HMD_SWE_2014)
#' SWE <- Inputlx(x = HMD_SWE_2014$Age, lx = HMD_SWE_2014$lx)
#' GetShape(SWE, type = 'all')
#'
GetShape <- function (pash, type = "all") {
  TestClass(pash)
  with(pash[["lt"]],
       {
        if (identical(type, "LTentr")){
          S = c(LTentr = LifetableEntropy(nax, nx, ndx, ex))
          }
        if (identical(type, "LTgini")){
          S = c(LTgini = LifetableGini(nax, nx, lx, ex))
          }
        if (identical(type, "LTcv")){
          S = c(LTcv = LifetableCV(nax, nx, ndx, lx, ex))
          }
        if (identical(type, "all")) {
          S = c(LTentr = LifetableEntropy(nax, nx, ndx, ex),
                LTgini = LifetableGini(nax, nx, lx, ex),
                LTcv   = LifetableCV(nax, nx, ndx, lx, ex))
        }
  return(S)
       })
}

# Lifetable Entropy -------------------------------------------------------

#' Average Years of Life Lost due to Death in Age x
#'
#' @keywords internal
eDaggerx <- function (nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx * c(ex[-1], 0) + (1 - nAx) * ex)
  edx[length(edx)] <- max(edx[length(edx)], 0)
  return(edx)
}

#' Total Life Years Lost due to Death
#'
#' @keywords internal
EDagger <- function (nax, nx, ndx, ex) {
  edx = eDaggerx(nax, nx, ex)
  ed = sum(ndx*edx)
  return(ed)
}

#' Life Table Entropy
#'
#' @keywords internal
LifetableEntropy <- function (nax, nx, ndx, ex) {
  ed = EDagger(nax, nx, ndx, ex)
  H = 1 - ed / ex[1]
  return(H)
}

#' Life Table Gini-Coefficient
#'
#' @seealso LifetableGini2
#' @keywords internal
LifetableGini <- function (nax, nx, lx, ex) {
  nAx <- nax/nx
  Gx = nAx * c(lx[-1], 0) + (1 - nAx) * lx
  Gx = Gx^2 * nx
  G = 2/ex[1]*sum(Gx) - 1
  return(G)
}

#' Life Table Gini-Coefficient
#'
#' Another discrete formulation of the Gini-Coeffcient
#' @source Shkolnikov and Andreev (2010)
#' \url{http://www.demogr.mpg.de/papers/technicalreports/tr-2010-001.pdf}
#' @seealso LifetableGini
#' @keywords internal
LifetableGini2 <- function (nax, nx, lx, ex) {
  nAx  = nax/nx
  lx_1 = c(lx[-1], 0)
  Gx   = lx_1^2 + nAx*(lx^2 - lx_1^2)
  Gx   = Gx * nx
  G    = 1 - 1/ex[1] * sum(Gx)
  return(1-G)
}

#' Life Table Coefficient of Variation
#'
#' @keywords internal
LifetableCV <- function (nax, nx, ndx, lx, ex) {
  nAx = nax/nx
  var = nAx * c(ex[-1], 0) + (1 - nAx) * ex
  var = nx * var^2
  Var = sum(var * ndx)
  CV  = sqrt(Var)/ex[1]
  return(CV)
}


