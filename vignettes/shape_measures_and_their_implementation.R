## ----set-options, echo=FALSE, cache=FALSE---------------------------------------------------------
options(width = 100)
knitr::opts_chunk$set(tidy = FALSE, size = "small")

## -------------------------------------------------------------------------------------------------
# Average Years of Life Lost due to Death in Age x
eDaggerx <- function(nax, nx, ex) {
  nAx = nax/nx
  edx = (nAx * c(ex[-1L], 0) + (1 - nAx) * ex)
  return(edx)
}

# Total Life Years Lost due to Death
EDagger <- function(nax, nx, ndx, ex) {
  edx = eDaggerx(nax, nx, ex)
  ed  = sum(ndx*edx)
  return(ed)
}

# Life Table Entropy
LifetableEntropy <- function(nax, nx, ndx, ex) {
  ed = EDagger(nax, nx, ndx, ex)
  H  = 1 - ed / ex[1L]
  return(H)
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
LifetableGini <- function(nax, nx, lx, ex) {
  nAx <- nax/nx
  Gx = nAx * c(lx[-1L], 0) + (1-nAx)*lx
  Gx = Gx^2 * nx
  G  = 1 - 1/ex[1L]*sum(Gx)
  return(G)
}

## -------------------------------------------------------------------------------------------------
LifetableGini2 <- function(nax, nx, lx, ex) {
  nAx  = nax/nx
  lx_1 = c(lx[-1L], 0)
  Gx   = lx_1^2 + nAx*(lx^2 - lx_1^2)
  Gx   = Gx*nx
  G    = 1 - 1/ex[1L] * sum(Gx)
  return(G)
}

## -------------------------------------------------------------------------------------------------
LifetableGini3 <- function(nax, nx, lx, ex, ndx) {
  lx_1 = c(lx[-1L], 0)
  Gx   = (lx^2 + lx_1^2)/2 - (ndx^2)/6
  Gx   = Gx*nx
  G    = 1 - 1/ex[1L]*sum(Gx)
  return(G)
}

## -------------------------------------------------------------------------------------------------
MortalityRatio <- function(x, nx, nmx, ex){
  m0   = nmx[1L]
  m_e0 = FindValue(measure = nmx, x, nx, ex)
  MR   = 1 - m0/m_e0
  return(MR)
}

FindValue <- function(measure, x, nx, ex){
  # I know this looks crazy!
  e0     = ex[1L]
  e0_    = max(x[(x - e0) <= 0])       # find the floor of e0 value
  m_e0_  = measure[x == e0_]           # find the floor of nmx (when x = e0_)
  n_e0_  = nx[x == e0_]
  m_e0up = measure[x == (e0_ + n_e0_)] # find the ceiling of nmx (when x = e0_ + nx)
  E0     = e0 - e0_                    # frac(e0)
  M_e0   = (m_e0_*(n_e0_ - E0) + m_e0up * E0) / n_e0_
  return(M_e0)
}

## -------------------------------------------------------------------------------------------------
LER <- function(x, nx, ex){
  e0   = ex[1L]
  e_e0 = FindValue(measure = ex, x, nx, ex)
  ler  =  1 - e_e0/e0
  return(ler)
}

## -------------------------------------------------------------------------------------------------
ACFM <- function(nmx, ndx, ex){
  acfm_x = (nmx - nmx[1L]) * ndx
  acfm   = 1 - exp(-ex[1L] * sum(acfm_x))
  return(acfm)
}

## -------------------------------------------------------------------------------------------------
PSMAD <- function(x, nx, lx, ex){
  l_e0  = FindValue(measure = lx, x, nx, ex)
  psmad = 1 - log(l_e0)
  return(psmad)
}

