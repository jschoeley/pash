## ----set-options, echo=FALSE, cache=FALSE---------------------------------------------------------
options(width = 100)
knitr::opts_chunk$set(tidy = FALSE, size = "small")

## -------------------------------------------------------------------------------------------------
# Average Life-Expectancy in Age x
EDaggerx <- function(nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx*c(ex[-1L], 0) + (1-nAx)*ex)
  edx[length(edx)] = ex[length(ex)]
  return(edx)
}

# Total Life Years Lost due to Death
EDagger <- function(nax, nx, ndx, ex) {
  edx = EDaggerx(nax, nx, ex)
  ed  = sum(ndx*edx)
  return(ed)
}

# Life Table Entropy
LifetableEntropy <- function(nax, nx, ndx, ex, harmonized) {
  ed = EDagger(nax, nx, ndx, ex)
  H  = ed/ex[1L]
  if (!isTRUE(harmonized)) {S = H}
  if (isTRUE(harmonized)) {S = 1-H}
  return(S)
}

## ----eval=FALSE-----------------------------------------------------------------------------------
#  # Life Table Variance
#  LifetableVar <- function(x, ndx, nax, ex) {
#    var = sum(ndx*(x+nax-ex[1L])^2)
#    return(var)
#  }
#  
#  # Life Table Coefficient of Variation
#  LifetableCV <- function(x, ndx, nax, ex, harmonized) {
#    var = LifetableVar(x, ndx, nax, ex)
#    CV  = sqrt(var)/ex[1L]
#    if (!isTRUE(harmonized)) {S = CV}
#    if (isTRUE(harmonized)) {S = 1-CV}
#    return(S)
#  }

## -------------------------------------------------------------------------------------------------
# Life Table Gini-Coefficient
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

## -------------------------------------------------------------------------------------------------
# Mortality Ratio
MortalityRatio <- function(x, nx, nmx, ex, harmonized){
  m0   = nmx[1L]
  m_e0 = approx(x = x, y = nmx, xout = ex[1L])[["y"]]
  MR   = m0/m_e0
  if (!isTRUE(harmonized)) {S = MR}
  if (isTRUE(harmonized)) {S = 1 - MR}
  return(S)
}


## -------------------------------------------------------------------------------------------------
# Life Expectancy Ratio
LER <- function(x, nx, ex, harmonized){
  e_e0 = approx(x = x, y = ex, xout = ex[1L])[["y"]]
  ler = e_e0/ex[1L]
  if (!isTRUE(harmonized)) {S = ler}
  if (isTRUE(harmonized)) {S = 1-ler}
  return(S)
}

## -------------------------------------------------------------------------------------------------
# Average of Change in Force of Mortality with respect to lx
ACFM <- function(nmx, ndx, ex, harmonized){
  acfm_x = (nmx - nmx[1L]) * ndx
  D = ex[1L] * sum(acfm_x)
  if (!isTRUE(harmonized)) {S = D}
  if (isTRUE(harmonized)) {S = 1-exp(-D)}
  return(S)
}

## -------------------------------------------------------------------------------------------------
# Probability to Survive up to the Mean Age at Death
PSMAD <- function(x, nx, lx, ex, harmonized){
  l_e0  = approx(x = x, y = lx, xout = ex[1L])[["y"]]
  if (!isTRUE(harmonized)) {S = l_e0}
  if (isTRUE(harmonized)) {S = 1 + log(l_e0)}
  return(S)
}

