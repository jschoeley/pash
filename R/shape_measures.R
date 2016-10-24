#' Lifetable Shape Measures
#'
#' Get lifetable shape measures from pace-shape object.
#'
#' @param pash Object of class \code{pash}.
#'
#' @examples
#' pash = lxToPash(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetShape(pash)
#'
#' @export
GetShape <- function (pash, type = "all") {
  TestClass(pash)
  lt = pash[["lt"]]
  if (identical(type, "LTentr")) S = c(LTentr = LifetableEntropy(lt))
  if (identical(type, "LTgini")) S = c(LTgini = NA)
  if (identical(type, "LTcv"))   S = c(LTcv = NA)
  if (identical(type, "all")) {
    S = c(LTentr = LifetableEntropy(lt),
          LTgini = NA,
          LTcv   = NA)
  }
  return(S)
}

# Lifetable Entropy -------------------------------------------------------

#' Average Years Of Life Lost Due To Death In Age X
eDaggerx <- function (x) {
  nAx = x$nax/x$nx
  edx = nAx * c(x$ex[-1], 0) + (1 - nAx) * x$ex
  return(edx)
}

#' Total Life Years Lost Due To Death
EDagger <- function (x) {
  edx = eDaggerx(x)
  ed = sum(x$ndx*edx)
  return(ed)
}

#' Life Table Entropy
LifetableEntropy <- function (x) {
  ed = EDagger(x)
  H = 1 - ed / x$ex[1]
  return(H)
}

#' Calculate Life Table Gini-Coefficient
LifetableGini <- function (x) {
  nAx = x$nax/x$nx
  Gx = nAx*c(x$lx[-1], 0) + (1 - nAx)*x$lx
  G = 2/x$ex[1] * sum(Gx^2) - 1
  return(G)
}
