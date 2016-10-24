#' Lifetable Shape Measures
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
  if (identical(type, "LTentr")) S = c(LTentr = LifetableEntropy(lt$nax, lt$nx, lt$ndx, lt$ex))
  if (identical(type, "LTgini")) S = c(LTgini = LifetableGini(lt$nx, lt$nax, lt$lx, lt$ex))
  if (identical(type, "LTcv"))   S = c(LTcv = LifetableCV(lt$nx, lt$nax, lt$dx, lt$ex))
  if (identical(type, "all")) {
    S = c(LTentr = LifetableEntropy(lt$nax, lt$nx, lt$ndx, lt$ex),
          LTgini = LifetableGini(lt$nx, lt$nax, lt$lx, lt$ex),
          LTcv   = LifetableCV(lt$nx, lt$nax, lt$dx, lt$ex))
  }
  return(S)
}

# Lifetable Entropy -------------------------------------------------------

# Average Years of Life Lost due to Death in Age x
eDaggerx <- function (nax, nx, ex) {
  nAx = nax/nx
  edx = nAx * c(ex[-1], 0) + (1 - nAx) * ex
  return(edx)
}

# Total Life Years Lost due to Death
EDagger <- function (nax, nx, ndx, ex) {
  edx = eDaggerx(nax, nx, ex)
  ed = sum(ndx*edx)
  return(ed)
}

# Life Table Entropy
LifetableEntropy <- function (nax, nx, ndx, ex) {
  ed = EDagger(nax, nx, ndx, ex)
  H = 1 - ed / ex[1]
  return(H)
}

# Life Table Gini-Coefficient
LifetableGini <- function (nx, nax, lx, ex) {
  return(NA)
}

# Life Table Coefficient of Variation
LifetableCV <- function (nx, nax, dx, ex) {
  return(NA)
}
