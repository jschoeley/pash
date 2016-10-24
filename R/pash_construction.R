#' pash: Pace-Shape Analysis for Life Tables
#'
#' \code{pash} lets you perform pace-shape analysis on life tables.Calculate a
#' wide array of pace and shape metrics and use them to standardize and compare
#' your data.
#'
#' First: Ceate a pace-shape object from either a life-table column or a
#' population matrix using one of the input function: \code{lxToPash}.
#'
#' Second: Calculate pace measures with \code{GetPace}, calculate shape
#' measures with \code{GetShape}, standardize a life table by pace and
#' shape \code{StandardizeLT}, rebase your life table to a different age
#' \code{RebaseLT}.
#'
#' @docType package
#' @name pash
NULL

# Pace-Shape Object Constructor -------------------------------------------

#' Pace-Shape Object Constructor
ConstructPash <- function (x, nx, nmx, nax, nqx, npx, lx, ndx, nLx, Tx, ex,
                           last_open, time_unit, type, input) {

  lt = data.frame(x = x, nx = nx, nmx = nmx, nax = nax,
                  nqx = nqx, npx = npx, lx = lx, ndx = ndx,
                  nLx = nLx, Tx = Tx, ex = ex)

  pash =
    structure(
      list(lt = lt),
      class = "pash",
      non_destructive_copy = lt,
      last_open = NA,
      time_unit = time_unit,
      source = list(type = type, input = input)
    )

  return(pash)

}

# Aux ---------------------------------------------------------------------

#' Width of Age Groups From Differenced Age Vector
DiffAge <- function (x, last_open = FALSE) {
  k  = length(x)
  nx = diff(x)
  if (identical(last_open, FALSE)) nx = c(nx, nx[k-1])
  if (identical(last_open, TRUE)) nx = c(nx, NA)
  return(nx)
}

#' Life Table ax Values Using The Midpoint Method
naxMidpoint <- function (nx) {
  nax = 0.5*nx
  return(nax)
}

# X To Pash ---------------------------------------------------------------

#' Convert A Life Table Survivorship Function To A Pace-Shape Object
#'
#' Given an age vector and corresponding survival probabilities a complete
#' lifetable is calculated and a pace-shape objectt constructed.
#'
#' @return A pace-shape object (see \code{pash}).
#'
#' @examples
#' lxToPash(x = prestons_lx$x, lx = prestons_lx$lx)
#'
#' @export
lxToPash <- function (x, lx,
                      nax = "midpoint",
                      nx = "auto",
                      last_open = FALSE,
                      time_unit = "years") {

  # Input validation --------------------------------------------------------

  # k: number of age-groups
  k = length(x)

  valid_input = lxToPashInputValidation(x, lx, nax, nx, k, last_open)
  nax = valid_input[["nax"]]
  nx = valid_input[["nx"]]

  # Build life table --------------------------------------------------------

  # set radix to 1
  lx_ = lx / lx[1L]

  # nx: width of age groups [x, x+n)
  if (identical(nx, "auto")) {
    nx = DiffAge(x, last_open)
  }

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  # if no nax value(s) is/are provided assume that death
  # ocours on average mid-interval, i.e. linear surviorship
  # in case of an open last age group and "midpoint" method
  # set the last nax value to the value of the preceeding age group
  if (identical(nax, "midpoint")) {
    nax = naxMidpoint(nx)
    if (identical(last_open, TRUE)) {
      nax[k] = nax[k-1L]
    }
  }
  # ndx: lifetable deaths in age group [x, x+n)
  ndx = c(lx_[-k] - lx_[-1L], lx_[k])
  # nqx: lifetable probability of death in age group [x, x+n)
  # given survival to age x
  nqx = ndx/lx_
  # npx: lifetable probability of surviving age group [x, x+n)
  # given survival to age x
  npx = 1-nqx
  # nLx: amount of subject-time at risk in age group [x, x+n)
  # The nx handling needs to be improved in case of open age groups
  nx_ = if (identical(last_open, TRUE)) c(nx[-k], nx[k-1]) else nx
  nLx = nx_*(lx_-ndx) + nax*ndx
  # nmx: life table mortality rate in age group [x, x+n]
  nmx = ndx/nLx
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx_

  # Construct pash object ---------------------------------------------------

  # construct the pace-shape object, a validated life table
  pash =  ConstructPash(
    x = x, nx = nx, nmx = nmx, nax = nax,
    nqx = nqx, npx = npx, lx = lx_, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, last_open = last_open,
    type = "lx", input = data.frame(x = x, lx = lx)
  )

  return(pash)

}
