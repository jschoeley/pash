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
GetShape <- function (pash, type = "all") {
  TestClass(pash)
  lt = pash[["lt"]]
  if (identical(type, "LTentr"))
    S = c(LTentr = LifetableEntropy(lt$nax, lt$nx, lt$ndx, lt$lt, lt$ex))
  if (identical(type, "LTgini"))
    S = c(LTgini = LifetableGini(lt$nax, lt$nx, lt$lx, lt$ex))
  if (identical(type, "LTcv"))
    S = c(LTcv = LifetableCV(lt$nax, lt$nx, lt$dx, lt$lx, lt$ex))
  if (identical(type, "all")) {
    S = c(LTentr = LifetableEntropy(lt$nax, lt$nx, lt$ndx, lt$lx, lt$ex),
          LTgini = LifetableGini(lt$nax, lt$nx, lt$lx, lt$ex),
          LTcv   = LifetableCV(lt$nax, lt$nx, lt$dx, lt$lx, lt$ex))
  }
  return(S)
}

# Lifetable Entropy -------------------------------------------------------

#' Average Years of Life Lost due to Death in Age x
#'
#' @keywords internal
eDaggerx <- function (nax, nx, nlx, ex) {
  nAx = nax/nx
  edx = (nAx * c(ex[-1], 0) + (1 - nAx) * ex) / nlx[1]
  edx[length(edx)] <- max(edx[length(edx)], 0)
  return(edx)
}

#' Total Life Years Lost due to Death
#'
#' @keywords internal
EDagger <- function (nax, nx, ndx, nlx, ex) {
  edx = eDaggerx(nax, nx, nlx, ex)
  ed = sum(ndx*edx)
  return(ed)
}

#' Life Table Entropy
#'
#' @keywords internal
LifetableEntropy <- function (nax, nx, ndx, nlx, ex) {
  ed = EDagger(nax, nx, ndx, nlx, ex)
  H = 1 - ed / ex[1]
  return(H)
}

#' Life Table Gini-Coefficient
#'
#' @seealso LifetableGini
#' @keywords internal
LifetableGini <- function (nax, nx, nlx, ex) {
  nAx <- nax/nx
  nlx <- nlx/nlx[1]
  Gx = nAx * c(nlx[-1], 0) + (1 - nAx) * nlx
  Gx = Gx^2 * nx
  Gx[length(Gx)] <- max(Gx[length(Gx)], 0)
  G = 2/ex[1]*sum(Gx) - 1
  return(G)
}

#' Life Table Gini-Coefficient
#'
#' Another discrete formulation of the Gini-Coeffcient
#' @seealso LifetableGini
#' @keywords internal
LifetableGini2 <- function (nax, nx, nlx, ex) {
  nAx = nax/nx
  nlx = nlx/nlx[1]
  nlx_1 = c(nlx[-1], 0)
  Gx = nlx_1^2 + nAx*(nlx^2 - nlx_1^2)
  Gx = Gx * nx
  G = 1 - 1/ex[1] * sum(Gx)
  return(1-G)
}

#' Life Table Coefficient of Variation
#'
#' @keywords internal
LifetableCV <- function (nax, nx, ndx, nlx, ex) {
  nAx = nax/nx
  ndx = ndx/nlx[1]
  var = nAx * c(ex[-1], 0) + (1 - nAx) * ex
  var = nx * var^2
  Var = sum(var * ndx)
  CV  = sqrt(Var)/ex[1]
  return(CV)
}
