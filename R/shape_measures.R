#' Life-table Shape Measures
#'
#' Get life table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which shape measure should be returned (default \code{"all"})?
#' @param harmonized Should the harmonized version of the shape measures be
#'   returned (default \code{TRUE})?
#'
#' @details
#'   If \code{harmonized == TRUE}, then all shape measures are re-scaled so that
#'   (1) they are positive for monotonically increasing forces of mortality over
#'   age (2), they are negative for monotonically decreasing forces
#'   of mortality over age, (3) they are 0 for constant
#'   forces of mortality over age, (4) they have a maximum value
#'   of 1. See Wrycza etal. (2015) for details.
#'
#'   If \code{harmonized == FALSE} the shape measures have their conventional
#'   scaling.
#'
#' @return
#' The following shape measures are reurned:
#' \describe{
#'   \item{\code{"Entropy"}}{Life table entropy}
#'   \item{\code{"Gini"}}{Life table Gini coefficient}
#'   \item{\code{"CV"}}{Life table coefficient of variation.}
#'   \item{\code{"mxRatio"}}{Mortality Ratio - Wrycza et al. (2015)}
#'   \item{\code{"exRatio"}}{Life Expectancy Ratio - Wrycza et al. (2015)}
#'   \item{\code{"ACFM"}}{Average of Change in Force of Mortality
#'     with respect to lx - Wrycza et al. (2015)}
#'   \item{\code{"PSMAD"}}{Probability to Survive up to the Mean Age at Death
#'    - Wrycza et al. (2015)}
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
GetShape <- function(pash, type = "all", harmonized = TRUE) {
  TestClass(pash)
  with(pash[["lt"]],
       {
         shapes = c(Entropy  = LifetableEntropy(nax, nx, ndx, ex, harmonized),
                    Gini     = LifetableGini(x, nax, ndx, ex, harmonized),
                    CV       = LifetableCV(x, ndx, nax, ex, harmonized),
                    mxRatio  = MortalityRatio(x, nx, nmx, ex, harmonized),
                    exRatio  = LER(x, nx, ex, harmonized),
                    ACFM     = ACFM(nmx, ndx, ex, harmonized),
                    PSMAD    = PSMAD(x, nx, lx, ex, harmonized))
         if (identical(type, "all")) { S = shapes } else { S = shapes[type] }
         return(S)
       })
}

# Life-table entropy ------------------------------------------------------

#' Average Life-Expectancy in Age x
#'
#' @source Vaupel et al. (2016)
#' @keywords internal
EDaggerx <- function(nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx*c(ex[-1L], 0) + (1-nAx)*ex)
  edx[length(edx)] = ex[length(ex)]
  return(edx)
}

#' Total Life Years Lost due to Death
#'
#' @keywords internal
EDagger <- function(nax, nx, ndx, ex) {
  edx = EDaggerx(nax, nx, ex)
  ed  = sum(ndx*edx)
  return(ed)
}

#' Life Table Entropy
#'
#' @keywords internal
LifetableEntropy <- function(nax, nx, ndx, ex, harmonized) {
  ed = EDagger(nax, nx, ndx, ex)
  H  = ed/ex[1L]
  if (!isTRUE(harmonized)) {S = H}
  if (isTRUE(harmonized)) {S = 1-H}
  return(S)
}

# Life-table Gini coefficient ---------------------------------------------

#' Life Table Gini-Coefficient
#'
#' Discrete formulation of the Gini-Coeffcient
#' @source Schoeley (2017)
#' @keywords internal
LifetableGini <- function (x, nax, ndx, ex, harmonized) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  if (!isTRUE(harmonized)) {S = G}
  if (isTRUE(harmonized)) {S = 1-2*G}
  return(S)
}

# Life-table coefficient of variation -------------------------------------

#' Life Table Variance
#'
#' Discrete formulation of variance
#' @source Schoeley (2017)
#' @keywords internal
LifetableVar <- function(x, ndx, nax, ex) {
  var = sum(ndx*(x+nax-ex[1L])^2)
  return(var)
}

#' Life Table Coefficient of Variation
#'
#' @keywords internal
LifetableCV <- function(x, ndx, nax, ex, harmonized) {
  var = LifetableVar(x, ndx, nax, ex)
  CV  = sqrt(var)/ex[1L]
  if (!isTRUE(harmonized)) {S = CV}
  if (isTRUE(harmonized)) {S = 1-CV}
  return(S)
}

# ACFM --------------------------------------------------------------------

#' Average of Change in Force of Mortality with respect to lx
#'
#' @source Wrycza et al. (2015)
#' @keywords internal
ACFM <- function(nmx, ndx, ex, harmonized){
  acfm_x = (nmx - nmx[1L]) * ndx
  D = ex[1L] * sum(acfm_x)
  if (!isTRUE(harmonized)) {S = D}
  if (isTRUE(harmonized)) {S = 1-exp(-D)}
  return(S)
}

# Mortality ratio ---------------------------------------------------------

#' Mortality Ratio
#'
#' @importFrom stats approx
#' @keywords internal
MortalityRatio <- function(x, nx, nmx, ex, harmonized){
  m0   = nmx[1L]
  m_e0 = approx(x = x, y = nmx, xout = ex[1L])[["y"]]
  MR   = m0/m_e0
  if (!isTRUE(harmonized)) {S = MR}
  if (isTRUE(harmonized)) {S = 1 - MR}
  return(S)
}

# PSMAD -------------------------------------------------------------------

#' Probability to Survive up to the Mean Age at Death
#'
#' @importFrom stats approx
#' @keywords internal
PSMAD <- function(x, nx, lx, ex, harmonized){
  l_e0  = approx(x = x, y = lx, xout = ex[1L])[["y"]]
  if (!isTRUE(harmonized)) {S = l_e0}
  if (isTRUE(harmonized)) {S = 1 + log(l_e0)}
  return(S)
}

# LER ---------------------------------------------------------------------

#' Life Expectancy Ratio
#'
#' @importFrom stats approx
#' @keywords internal
LER <- function(x, nx, ex, harmonized){
  e_e0 = approx(x = x, y = ex, xout = ex[1L])[["y"]]
  ler = e_e0/ex[1L]
  if (!isTRUE(harmonized)) {S = ler}
  if (isTRUE(harmonized)) {S = 1-ler}
  return(S)
}
