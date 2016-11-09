#' Life-table Shape Measures
#'
#' Get life-table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which shape measure should be returned (default \code{"all"})?
#'
#' @details
#' The type argument accepts the following strings:
#' \describe{
#'   \item{\code{"LTentr"}}{Life table entropy.}
#'   \item{\code{"LTgini"}}{Life table Gini coefficient - Pascariu implementation.}
#'   \item{\code{"LTgini2"}}{Life table Gini coefficient - Shkolnikov implementation.}
#'   \item{\code{"LTcv"}}{Life table coefficient of variation.}
#'   \item{\code{"all"}}{All of the above measures.}
#' }
#'
#' @source Wrycza, Tomasz F., Trifon I. Missov, and Annette Baudisch. 2015.
#' "Quantifying the Shape of Aging." PLOS ONE 10 (3): 1-18. doi:10.1371/journal.pone.0119163.
#' @source Vladimir, Shkolnikov M. and Evgheny, Andreev M. 2010.
#' "Spreadsheet for calculation of life-table dispersion measures". Demographic Research.
#' \url{http://www.demogr.mpg.de/papers/technicalreports/tr-2010-001.pdf}
#'
#' @examples
#' pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetShape(pash)
#'
#' @export
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
        if (identical(type, "LTgini2")){
          S = c(LTgini2 = LifetableGini2(nax, nx, lx, ex))
          }
        if (identical(type, "LTcv")){
          S = c(LTcv = LifetableCV(nax, nx, ndx, lx, ex))
          }
        if (identical(type, "all")) {
          S = c(LTentr = LifetableEntropy(nax, nx, ndx, ex),
                LTgini = LifetableGini(nax, nx, lx, ex),
                LTgini2 = LifetableGini2(nax, nx, lx, ex),
                LTcv   = LifetableCV(nax, nx, ndx, lx, ex))
        }
  return(S)
       })
}

# Lifetable Entropy -------------------------------------------------------

#' Average Years of Life Lost Due to Death in Age x
#'
#' @keywords internal
eDaggerx <- function (nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx * c(ex[-1L], 0) + (1 - nAx) * ex)
  return(edx)
}

#' Total Life Years Lost Due to Death
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
  H = 1 - ed / ex[1L]
  return(H)
}

#' Life Table Gini-Coefficient (Pascariu)
#'
#' @seealso LifetableGini2
#' @keywords internal
LifetableGini <- function (nax, nx, lx, ex) {
  nAx <- nax/nx
  Gx = nAx * c(lx[-1L], 0) + (1 - nAx) * lx
  Gx = Gx^2 * nx
  G = 2/ex[1L]*sum(Gx) - 1
  return(G)
}

#' Life Table Gini-Coefficient (Shkolnikov)
#'
#' @seealso LifetableGini
#' @keywords internal
LifetableGini2 <- function (nax, nx, lx, ex) {
  nAx  = nax/nx
  lx_1 = c(lx[-1L], 0)
  Gx   = lx_1^2 + nAx*(lx^2 - lx_1^2)
  Gx   = Gx * nx
  G    = 1 - 1/ex[1L] * sum(Gx)
  return(1-G)
}

#' Life Table Coefficient of Variation
#'
#' @keywords internal
LifetableCV <- function (nax, nx, ndx, lx, ex) {
  nAx = nax/nx
  var = nAx * c(ex[-1L], 0) + (1 - nAx) * ex
  var = nx * var^2
  Var = sum(var * ndx)
  CV  = sqrt(Var)/ex[1L]
  return(CV)
}
