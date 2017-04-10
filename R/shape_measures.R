#' Life-table Shape Measures
#'
#' Get life table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which shape measure should be returned (default \code{"all"})?
#'
#' @return
#' The following shape measure are reurned:
#' \describe{
#'   \item{\code{"Variance"}}{Life table variance}
#'   \item{\code{"Entropy"}}{Life table entropy}
#'   \item{\code{"Gini"}}{Life table Gini coefficient - Schoeley (2017)}
#'   \item{\code{"mxRatio"}}{Mortality Ratio - Wrycza et al. (2015)}
#'   \item{\code{"exRatio"}}{Life Expectancy Ratio - Wrycza et al. (2015)}
#'   \item{\code{"ACFM"}}{Average of Change in Force of Mortality
#'     with respect to lx - Wrycza et al. (2015)}
#'   \item{\code{"PSMAD"}}{Probability to Survive up to the Mean Age at Death
#'    - Wrycza et al. (2015)}
#'   \item{\code{"CV"}}{Life table coefficient of variation.}
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
GetShape <- function(pash, type = "all") {
  TestClass(pash)
  with(pash[["lt"]],
       {
         shapes = c(Entropy  = LifetableEntropy(nax, nx, ndx, ex),
                    Gini     = LifetableGini(x, nax, ndx, ex),
                    CV       = LifetableCV(x, ndx, nax, ex),
                    Variance = LifetableVar(x, ndx, nax, ex),
                    mxRatio  = MortalityRatio(x, nx, nmx, ex),
                    exRatio  = LER(x, nx, ex),
                    ACFM     = ACFM(nmx, ndx, ex),
                    PSMAD    = PSMAD(x, nx, lx, ex))
         if (identical(type, "all")) { S = shapes } else { S = shapes[type] }
         return(S)
       })
}

# Shape Functions ---------------------------------------------------------

#' Life Table Variance
#'
#' Discrete formulation of variance
#' @source Schoeley (2017)
#' @keywords internal
LifetableVar <- function(x, ndx, nax, ex) {
  var = sum(ndx*(x+nax-ex[1L])^2)
  return(var)
}

#' Average Years of Life Lost due to Death in Age x
#'
#' @source Vaupel et al. (2016)
#' @keywords internal
eDaggerx <- function(nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx*c(ex[-1L], 0) + (1-nAx)*ex)
  return(edx)
}

#' Total Life Years Lost due to Death
#'
#' @keywords internal
EDagger <- function(nax, nx, ndx, ex) {
  edx = eDaggerx(nax, nx, ex)
  ed  = sum(ndx*edx)
  return(ed)
}

#' Life Table Gini-Coefficient
#'
#' Discrete formulation of the Gini-Coeffcient
#' @source Schoeley (2017)
#' @keywords internal
LifetableGini <- function (x, nax, ndx, ex) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(G)
}

#' Average of Change in Force of Mortality with respect to lx
#'
#' @source Wrycza et al. (2015)
#' @keywords internal
ACFM <- function(nmx, ndx, ex){
  acfm_x = (nmx - nmx[1L]) * ndx
  acfm   = 1 - exp(-ex[1L] * sum(acfm_x))
  return(acfm)
}

#' Life Table Entropy
#'
#' @keywords internal
LifetableEntropy <- function(nax, nx, ndx, ex) {
  ed = EDagger(nax, nx, ndx, ex)
  H  = 1 - ed/ex[1L]
  return(H)
}

#' Life Table Coefficient of Variation
#'
#' @keywords internal
LifetableCV <- function(x, ndx, nax, ex) {
  var = LifetableVar(x, ndx, nax, ex)
  CV  = sqrt(var)/ex[1L]
  return(CV)
}

#' Find and compute values that depend on ex
#' eg: m(x) when x = e(x)
#' @keywords internal
FindValue <- function(measure, x, nx, ex){
  # I know this looks crazy!
  e0     = ex[1L]
  e0_    = max(x[(x - e0) <= 0]) # find the floor of e0 value
  m_e0_  = measure[x == e0_] # find the floor of nmx (when x = e0_)
  n_e0_  = nx[x == e0_]
  m_e0up = measure[x == (e0_ + n_e0_)] # find the ceiling of nmx (when x = e0_ + nx)
  E0     = e0 - e0_
  M_e0   = (m_e0_*(n_e0_ - E0) + m_e0up * E0) / n_e0_
  return(M_e0)
}

#' Mortality Ratio
#'
#' @keywords internal
MortalityRatio <- function(x, nx, nmx, ex){
  m0   = nmx[1L]
  m_e0 = FindValue(measure = nmx, x, nx, ex)
  MR   = 1 - m0/m_e0
  return(MR)
}

#' Probability to Survive up to the Mean Age at Death
#'
#' @keywords internal
PSMAD <- function(x, nx, lx, ex){
  l_e0  = FindValue(measure = lx, x, nx, ex)
  psmad = 1 - log(l_e0)
  return(psmad)
}

#' Life Expectancy Ratio
#'
#' @keywords internal
LER <- function(x, nx, ex){
  e0   = ex[1L]
  e_e0 = FindValue(measure = ex, x, nx, ex)
  ler  =  1 - e_e0/e0
  return(ler)
}
