# nax Calculation ---------------------------------------------------------

#' nax Midpoint
#'
#' Get life-table nax values from nx using the midpoint assumption.
#'
#' @param nx Width of age interval [x, x+n).
#' @param k  Number of age groups.
#' @param last_open Is the last age group open (TRUE) or closed (FALSE)?
#'
#' @return The nax as a numeric vector of \code{length(nx)}.
#'
#' @details
#' If the last age group is open then set its nax value to that of the
#' preceeding age group.
#'
#' @keywords internal
naxMidpoint <- function (nx, k, last_open) {
  nax = 0.5*nx
  if (identical(last_open, TRUE)) {
    nax[k] = nax[k-1L]
  }
  return(nax)
}

#' nax Constant nmx (nqx)
#'
#' Get life-table nax values from nqx using the constant nmx assumption.
#'
#' @param nx  Width of age interval [x, x+n).
#' @param nqx Probability to die within age interval [x, x+nx) given survival to
#'   x.
#' @param npx Probability to survive age interval [x, x+nx) given survival to x.
#' @param k   Number of age groups.
#' @param last_open Is the last age group open (TRUE) or closed (FALSE)?
#'
#' @return The nax as a numeric vector of \code{length(nx)}.
#'
#' @details
#' For all age groups but the last we calculate:
#' nax = -nx/nqx - nx/log(npx) + nx
#'
#' If the last age group is closed we extrapolate the last nax from the
#' preceeding nax.
#'
#' If the last age group is open we set the last nax to that of the preceeding
#' nax.
#'
#' @keywords internal
naxConstantnmx1 <- function (x, nx, nqx, npx, k, last_open) {
  nax = -nx/nqx - nx/log(npx) + nx
  if (identical(last_open, FALSE)) {
    nAx = nax/nx
    regress_nAx = log(nAx / (1-nAx))[-k]; regress_x = x[-k]
    # the analytic expression above can't return a nax value for the
    # last age group (when survivorship hits 0). Therefore we extrapolate
    # the last nax value based on loess regression of the nax values
    pred = stats::predict(stats::loess(regress_nAx~regress_x,
                                       control = stats::loess.control(surface = "direct")),
                          data.frame(regress_x = x[k]))
    nax[k] = exp(pred)/(1+exp(pred))*nx[k]
  }
  if (identical(last_open, TRUE)) {
    nax[k] = nax[k-1L]
  }
  return(nax)
}

#' nax Constant nmx (nmx)
#'
#' Get life-table nax values from nmx using the constant nmx assumption.
#'
#' @param nx  Width of age interval [x, x+n).
#' @param nmx Mortality rate in age interval [x, x+nx).
#' @param k   Number of age groups.
#' @param last_open Is the last age group open (TRUE) or closed (FALSE)?
#'
#' @return The nax as a numeric vector of \code{length(nx)}.
#'
#' @details
#' For all age groups we calculate:
#' nax = nx + 1/nmx - nx/(1 - exp(-nx*nmx))
#'
#' If the last age group is open we calculate the last nax as the remaining life
#' expectancy at x. Given the constant hazard assumption that is:
#' nax[k] = 1/nmx[k]
#'
#' @keywords internal
naxConstantnmx2 <- function (nx, nmx, k, last_open) {
  nax = nx + 1/nmx - nx/(1 - exp(-nx*nmx))
  if (identical(last_open, TRUE)){
    nax[k] = 1/nmx[k]
  }
  return(nax)
}

# Input lx ----------------------------------------------------------------

#' Convert a Life-table Survivorship Function to a Pace-Shape Object
#'
#' Given an age vector and corresponding survival probabilities a complete
#' lifetable is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param lx Probability to survive up until age x.
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (either numeric scalar, numeric vector or one of \code{c("midpoint",
#'   "constant_nmx")}).
#' @param nx Width of age interval [x, x+n) (either numeric scalar, numeric
#'   vector or \code{"auto"}).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE,
#'   default).
#' @param time_unit The unit of the ages (by default "years").
#'
#' @section nx handling:
#'   For nx you may provide either a numeric scalar, a numeric vector, or
#'   let the function determine the width for you (\code{"auto"}, default). A
#'   scalar will be recycled for each age group. A vector must be as long as the
#'   age vector and allows you to specify the width of each age group
#'   separately. If the last age group is supposed to be open make sure that the
#'   last value of your nx vector is NA. By default the width of the age groups
#'   is calculated from differencing the age vector. Should the last age group
#'   be closed, the width of the last age group is assumed to be equal to the
#'   width of the preceeding age group.
#'
#' @section nax handling:
#'   nax may be provided as either a numeric scalar, a numeric vector, or
#'   calculated via the \code{midpoint} method (default) or the constant nmx
#'   assumption (option "\code{constant_nmx}").
#'
#'   The midpoint method assumes a linear decline of the l(x) function over the
#'   width of an age group, implying that those who die in that age group die on
#'   average halfway into it (uniform distibution of deaths within age group):
#'
#'   nax = n/2 (see Preston, 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n) to be constant
#'   implies an exponentially declining l(x) function within [x, x+n) and will
#'   produce nax values smaller than those calculated via the midpoint method.
#'   Preston (2001), p. 46 provides an expression for nax given the assumption
#'   of constant mortality. Restating this expression in terms of nqx and npx
#'   leads to:
#'
#'   nax = -n/nqx - n/ln(npx) + n
#'
#'   If the last age group is open and the midpoint method is used, then the
#'   last nax value is set to that of the preceeding age group.
#'
#'   If the last age group is open and the constant nmx method is used, then the
#'   last nax value is extrapolated from the preceeding nax values.
#'
#' @source Preston, Samuel H., Patric Heuveline, and Michel Guillot. 2001.
#'   Demography. Oxford, UK: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#'
#' # different nax assumptions
#' Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, nax = "constant_nmx")
#' @export
Inputlx <- function (x, lx,
                     nax = "midpoint",
                     nx = "auto",
                     last_open = FALSE,
                     time_unit = "years") {

  # Input validation --------------------------------------------------------

  # k: number of age-groups
  k = length(x)

  # validate parameters
  ValidateAge(x)
  val_nx = Validatenx(nx, x, last_open)
  nx_ = val_nx[["nx"]]
  val_nax = Validatenax(nax, x, nx, last_open)
  nax_ = val_nax[["nax"]]

  # validate data
  Validatelx(lx)

  # Build life table --------------------------------------------------------

  # set radix to 1
  lx_ = lx / lx[1L]

  # ndx: lifetable deaths in age group [x, x+n)
  ndx = c(lx_[-k] - lx_[-1L], lx_[k])
  # nqx: lifetable probability of death in age group [x, x+n)
  # given survival to age x
  nqx = ndx/lx_
  # npx: lifetable probability of surviving age group [x, x+n)
  # given survival to age x
  npx = 1-nqx

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(nax_, "midpoint")) {
    nax_ = naxMidpoint(nx_, k, last_open)
  }
  if (identical(nax_, "constant_nmx")) {
    nax_ = naxConstantnmx1(x, nx_, nqx, npx, k, last_open)
  }

  # nLx: amount of subject-time at risk in age group [x, x+n)
  # The nx handling needs to be improved in case of open age groups
  nx_ = if (identical(last_open, TRUE)) c(nx_[-k], nx_[k-1L]) else nx_
  nLx = nx_*(lx_-ndx) + nax_*ndx
  # nmx: life table mortality rate in age group [x, x+n]
  nmx = ndx/nLx
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx_
  # when lx becomes 0 ex becomes NaN. set it to 0
  ex[is.nan(ex)] = 0

  # Construct pash object ---------------------------------------------------

  # construct the pace-shape object, a validated life table
  pash =  ConstructPash(
    x = x, nx = nx_, nmx = nmx, nax = nax_,
    nqx = nqx, npx = npx, lx = lx_, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = val_nax[["nax_mode"]], last_open = last_open,
    type = "lx",
    input = list(x = x, lx = lx, nax = nax, nx = nx)
  )

  return(pash)

}

# Input nmx ---------------------------------------------------------------

#' Convert a Life-table Mortality Rate Function to a Pace-Shape Object
#'
#' Given an age vector and corresponding mortality rates a complete life-table
#' is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param nmx Mortality rate in age interval [x, x+nx).
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (either numeric scalar, numeric vector or one of \code{c("midpoint",
#'   "constant_nmx")}).
#' @param nx Width of age interval [x, x+n) (either numeric scalar, numeric
#'   vector or \code{"auto"}).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE,
#'   default).
#' @param time_unit The unit of the ages (by default "years").
#'
#' @section nx handling: For nx you may provide either a numeric scalar, a
#'   numeric vector, or let the function determine the width for you
#'   (\code{"auto"}, default). A scalar will be recycled for each age group. A
#'   vector must be as long as the age vector and allows you to specify the
#'   width of each age group separately. If the last age group is supposed to be
#'   open make sure that the last value of your nx vector is NA. By default the
#'   width of the age groups is calculated from differencing the age vector.
#'   Should the last age group be closed, the width of the last age group is
#'   assumed to be equal to the width of the preceeding age group.
#'
#' @section nax handling: nax may be provided as either a numeric scalar, a
#'   numeric vector, or calculated via the \code{midpoint} method (default) or
#'   the constant nmx assumption (option "\code{constant_nmx}").
#'
#'   The midpoint method assumes a linear decline of the l(x) function over the
#'   width of an age group, implying that those who die in that age group die on
#'   average halfway into it (uniform distibution of deaths within age group):
#'
#'   nax = n/2 (see Preston, 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n) to be constant
#'   implies an exponentially declining l(x) function within [x, x+n) and will
#'   produce nax values smaller than those calculated via the midpoint method.
#'   Preston (2001), p. 46 provides an expression for nax given the assumption
#'   of constant mortality. Restating this expression in terms of nqx and npx
#'   leads to:
#'
#'   nax = -n/nqx - n/ln(npx) + n
#'
#'   If the last age group is open and the midpoint method is used, then the
#'   last nax value is set to that of the preceeding age group.
#'
#'   If the last age group is open and the constant nmx method is used, we
#'   calculate the last nax as the remaining life expectancy at x. Given the
#'   constant hazard assumption that is: nax[k] = 1/nmx[k]
#'
#' @source Preston, Samuel H., Patric Heuveline, and Michel Guillot. 2001.
#'   Demography. Oxford, UK: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' swe <- subset(sweden5x5, sex == "female" & period == "1940-1944")[c("x", "nmx")]
#' Inputnmx(x = swe$x, nmx = swe$nmx, last_open = TRUE, nax = "constant_nmx")
#'
#' @export
Inputnmx <- function (x, nmx,
                      nax = "midpoint",
                      nx = "auto",
                      last_open = FALSE,
                      time_unit = "years") {

  # Input validation --------------------------------------------------------

  # k: number of age-groups
  k = length(x)

  # validate parameters
  ValidateAge(x)
  val_nx = Validatenx(nx, x, last_open)
  nx_ = val_nx[["nx"]]
  val_nax = Validatenax(nax, x, nx, last_open)
  nax_ = val_nax[["nax"]]

  # validate data
  Validatemx(nmx)

  # Build life table --------------------------------------------------------

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(nax_, "midpoint")) {
    nax_ = naxMidpoint(nx_, k, last_open)
  }
  if (identical(nax_, "constant_nmx")) {
    nax_ = naxConstantnmx2(nx_, nmx, k, last_open)
  }

  # nqx from nmx and nax
  nqx = nx_*nmx / (1+(nx_-nax_)*nmx)
  nqx[k] = 1
  npx = 1-nqx
  # lx
  lx = 1*cumprod(npx)
  # ndx: lifetable deaths in age group [x, x+n)
  ndx = c(lx[-k] - lx[-1L], lx[k])
  # nLx: amount of subject-time at risk in age group [x, x+n)
  nLx = nx_*(lx-ndx) + nax_*ndx
  nLx[k] = lx[k]/nmx[k]
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx
  # when lx becomes 0 ex becomes NaN. set it to 0
  ex[is.nan(ex)] = 0


  # Construct pash object ---------------------------------------------------

  # construct the pace-shape object, a validated life table
  pash =  ConstructPash(
    x = x, nx = nx_, nmx = nmx, nax = nax_,
    nqx = nqx, npx = npx, lx = lx, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = val_nax[["nax_mode"]], last_open = last_open,
    type = "nmx",
    input = list(x = x, nmx = nmx, nax = nax, nx = nx)
  )

  return(pash)

}
