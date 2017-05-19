# Input lx ----------------------------------------------------------------

#' Convert a Life-table Survivorship Function into a Pace-Shape Object
#'
#' Given an age vector and corresponding survival probabilities a complete
#' life-table is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param lx Probability to survive up until age x.
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (either numeric scalar, numeric vector or one of \code{c("udd", "cfm")}).
#' @param nx Width of age interval [x, x+n) (either numeric scalar, numeric
#'   vector or \code{"auto"}).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE, default)?
#' @param time_unit The unit of the ages (by default "years").
#'
#' @section nx handling: For nx you may provide either a numeric scalar, a
#'   numeric vector, or let the function determine the width for you
#'   (\code{"auto"}, default). A scalar will be recycled for each age group. A
#'   vector must be as long as the age vector and allows you to specify the
#'   width of each age group separately. By default, the width of the age
#'   groups are calculated from differencing the age vector.
#'
#' @section nax handling: nax may be provided as either a numeric scalar, a
#'   numeric vector, or calculated via the uniform distribution of deaths
#'   (\code{udd}) method (default) or the constant force of mortality assumption
#'   (option \code{cfm}).
#'
#'   The (\code{udd}) method assumes a linear decline of the l(x) function over
#'   the width of an age group, implying that those who die in that age group
#'   die on average halfway into it (also known as the "midpoint" assumption):
#'
#'   nax = n/2 (see Preston et al. 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n) to be constant
#'   (\code{cfm} method) implies an exponentially declining l(x) function
#'   within [x, x+n) and will produce nax values smaller than those calculated
#'   via the \code{udd} method. Preston et al. (2001, p. 46) provide an expression
#'   for nax given the assumption of constant mortality. Restating this expression
#'   in terms of nqx and npx leads to:
#'
#'   nax = -n/nqx - n/log(npx) + n
#'
#'   If the last age group is open and the \code{udd} or \code{cfm} method is
#'   used, then the last nmx value is log-linearly extrapolated based on the
#'   preceding two nmx and the nax, and the ex for the last age group id
#'   calculated using the constant hazard assumption.
#'
#' @source Preston, Samuel H., Patrick Heuveline, and Michel Guillot (2001).
#'   Demography: Measuring and modeling population processes. Oxford: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' # open last age group
#' Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, last_open = TRUE)
#' # different nax assumptions
#' Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, nax = "cfm")
#'
#' @export
Inputlx <- function (x, lx,
                     nax = "udd",
                     nx = "auto",
                     last_open = FALSE,
                     time_unit = "years") {

  # Input validation --------------------------------------------------------

  # validate parameters
  ValidateAge(x)
  val_nx = Validatenx(nx, x, last_open)
  nx_ = val_nx[["nx"]]
  val_nax = Validatenax(nax, x, nx, last_open)
  nax_ = val_nax[["nax"]]

  # validate data (modify in case of lx with 0s in tail)
  Validatelx(lx)
  valid_lx = ValidateTaillx(x, lx, nax_, nx_, last_open)
  x_ = valid_lx[["x"]]
  lx_ = valid_lx[["lx"]]
  nx_ = valid_lx[["nx"]]
  nax_ = valid_lx[["nax"]]
  last_open = valid_lx[["last_open"]]

  # k: number of age-groups
  k = length(x_)

  # Build life-table --------------------------------------------------------

  # set radix to 1
  lx_ = lx_ / lx_[1L]

  # ndx: life-table deaths in age group [x, x+n)
  ndx = c(lx_[-k] - lx_[-1L], lx_[k])
  # nqx: life-table probability of death in age group [x, x+n)
  # given survival to age x
  nqx = ndx/lx_
  # npx: life-table probability of surviving age group [x, x+n)
  # given survival to age x
  npx = 1-nqx

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(val_nax[["nax_mode"]], "udd")) {
    nax_ = naxUDD(nx_, k, last_open)
  }
  if (identical(val_nax[["nax_mode"]], "cfm")) {
    nax_ = naxCFMfromnqx(x_, nx_, nqx, npx, k, last_open)
  }

  # nLx: amount of subject-time at risk in age group [x, x+n)
  nLx = nx_*(lx_-ndx) + nax_*ndx
  # nmx: life-table mortality rate in age group [x, x+n]
  nmx = ndx/nLx
  if (identical(last_open, TRUE)) {
    # in case of an open age group nmx will be NA for this age group (as the
    # width of the open age group is unknown). therefore we log-linearly
    # extrapolate nmx based on the preceding two nmx and calculate nLx of the
    # last age group using the constant hazard assumption.
    if (val_nax[["nax_mode"]] %in% c("udd", "cfm")) {
      nmx[k] = LinearExtrapolation(x = x_[c(k-2, k-1)], y = nmx[c(k-2, k-1)],
                                   xextra = x_[k], loga = TRUE)
      nax_[k] = 1/nmx[k]
      nLx[k] = nax_[k]*lx_[k]
      message("Inputlx() and last_open = TRUE: nmx of open age group log-linearly extrapolated based on preceding two nmx.")
    }
    if (val_nax[["nax_mode"]] %in% c("scalar", "vector")) {
      nLx[k] = nax_[k]*lx_[k]
      nmx[k] = ndx[k]/nLx[k]
    }
  }
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx_

  # Construct pash object ---------------------------------------------------

  # construct the pace-shape object, a validated life-table
  pash =  ConstructPash(
    x = x_, nx = nx_, nmx = nmx, nax = nax_,
    nqx = nqx, npx = npx, lx = lx_, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = val_nax[["nax_mode"]], last_open = last_open,
    type = "lx",
    input = list(x = x, lx = lx, nax = nax, nx = nx)
  )

  return(pash)

}

# Input nmx ---------------------------------------------------------------

#' Convert a Life-table Mortality Rate Function into a Pace-Shape Object
#'
#' Given an age vector and corresponding mortality rates a complete life-table
#' is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param nmx Mortality rate in age interval [x, x+nx).
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (either numeric scalar, numeric vector or one of \code{c("udd", "cfm")}).
#' @param nx Width of age interval [x, x+n) (either numeric scalar, numeric
#'   vector or \code{"auto"}).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE, default)?
#' @param time_unit The unit of the ages (by default "years").
#'
#' @section nx handling: For nx you may provide either a numeric scalar, a
#'   numeric vector, or let the function determine the width for you
#'   (\code{"auto"}, default). A scalar will be recycled for each age group. A
#'   vector must be as long as the age vector and allows you to specify the
#'   width of each age group separately. By default, the width of the age
#'   groups are calculated from differencing the age vector. If the last age
#'   group is supposed to be open make sure that the last value of your nx
#'   vector is NA. Should the last age group be closed, the width of the last
#'   age group is assumed to be equal to the width of the preceeding age group.
#'
#' @section nax handling: nax may be provided as either a numeric scalar, a
#'   numeric vector, or calculated via the uniform distribution of deaths
#'   (\code{udd}) method (default) or the constant force of mortality assumption
#'   (option \code{cfm}).
#'
#'   The (\code{udd}) method assumes a linear decline of the l(x) function over
#'   the width of an age group, implying that those who die in that age group
#'   die on average halfway into it (also known as the "midpoint" assumption):
#'
#'   nax = n/2 (see Preston et al. 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n) to be constant
#'   (\code{cfm} method) implies an exponentially declining l(x) function
#'   within [x, x+n) and will produce nax values smaller than those calculated
#'   via the \code{udd} method. Preston et al. (2001, p. 46) provide an expression
#'   for nax given the assumption of constant mortality. Restating this expression
#'   in terms of nqx and npx leads to:
#'
#'   nax = -n/nqx - n/log(npx) + n
#'
#' @source Preston, Samuel H., Patrick Heuveline, and Michel Guillot (2001).
#'   Demography: Measuring and modeling population processes. Oxford: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' swe <- subset(sweden5x5, sex == "female" & period == "1940-1944")[c("x", "nmx")]
#' Inputnmx(x = swe$x, nmx = swe$nmx, last_open = TRUE, nax = "cfm")
#'
#' @export
Inputnmx <- function (x, nmx,
                      nax = "udd",
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

  # Build life-table --------------------------------------------------------

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(val_nax[["nax_mode"]], "udd")) {
    nax_ = naxUDD(nx_, k, last_open)
    # if last age group is open use cfm assumption for last nax
    if (identical(last_open, TRUE)) { nax_[k] = 1/nmx[k] }
  }
  if (identical(val_nax[["nax_mode"]], "cfm")) {
    nax_ = naxCFMfromnmx(nx_, nmx, k, last_open)
  }

  # nqx from nmx and nax
  if (identical(val_nax[["nax_mode"]], "udd")) {
    nqx = nmxTonqx(nmx, nax_, nx_, mode = "udd")
  }
  if (identical(val_nax[["nax_mode"]], "cfm")) {
    nqx = nmxTonqx(nmx, nax_, nx_, mode = "cfm")
  }
  if (val_nax[["nax_mode"]] %in% c("scalar", "vector")) {
    nqx = nmxTonqx(nmx, nax_, nx_, mode = "chiang")
  }
  nqx[k] = 1
  npx = 1-nqx
  # lx
  lx = c(1, cumprod(npx)[-k])
  # ndx: life-table deaths in age group [x, x+n)
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

  # construct the pace-shape object, a validated life-table
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

# Input nqx ---------------------------------------------------------------

#' Convert a Life-table Death Probability Function into a Pace-Shape Object
#'
#' Given an age vector and corresponding death probabilities, a complete life-table
#' is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param nqx Probability of dying in the age interval [x, x+n). In order to
#'    build the life-table, the last value of nqx should be 1. If that is not the
#'    case and the last age group is open, it will be set to 1; if the last age
#'    group provided by the user is closed, an additional open-last age group
#'    with nqx = 1 will be added.
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (either numeric scalar, numeric vector or one of \code{c("udd", "cfm")}).
#' @param nx Width of age interval [x, x+n) (either numeric scalar, numeric
#'   vector or \code{"auto"}).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE, default)?
#' @param time_unit The unit of the ages (by default "years").
#'
#' @section nx handling: For nx you may provide either a numeric scalar, a
#'   numeric vector, or let the function determine the width for you
#'   (\code{"auto"}, default). A scalar will be recycled for each age group. A
#'   vector must be as long as the age vector and allows you to specify the
#'   width of each age group separately. By default, the width of the age
#'   groups are calculated from differencing the age vector.
#'
#' @section nax handling: nax may be provided as either a numeric scalar, a
#'   numeric vector, or calculated via the uniform distribution of deaths
#'   (\code{udd}) method (default) or the constant force of mortality assumption
#'   (option \code{cfm}).
#'
#'   The (\code{udd}) method assumes a linear decline of the l(x) function over
#'   the width of an age group, implying that those who die in that age group
#'   die on average halfway into it (also known as the "midpoint" assumption):
#'
#'   nax = n/2 (see Preston et al. 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n) to be constant
#'   (\code{cfm} method) implies an exponentially declining l(x) function
#'   within [x, x+n) and will produce nax values smaller than those calculated
#'   via the \code{udd} method. Preston et al. (2001, p. 46) provide an expression
#'   for nax given the assumption of constant mortality. Restating this expression
#'   in terms of nqx and npx leads to:
#'
#'   nax = -n/nqx - n/log(npx) + n
#'
#'   If the last age group is open and the \code{udd} or \code{cfm} method is
#'   used, then the last nmx value is log-linearly extrapolated based on the
#'   preceding two nmx and the nax, and the ex for the last age group id
#'   calculated using the constant hazard assumption.
#'
#' @source Preston, Samuel H., Patrick Heuveline, and Michel Guillot (2001).
#'   Demography: Measuring and modeling population processes. Oxford: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' swe <- subset(sweden5x5, sex == "female" & period == "1940-1944")[c("x", "nqx")]
#' Inputnqx(x = swe$x, nqx = swe$nqx, last_open = TRUE, nax = "udd")
#'
#' @export
Inputnqx <- function (x, nqx,
                      nax = "udd",
                      nx = "auto",
                      last_open = FALSE,
                      time_unit = "years") {

  # Input validation --------------------------------------------------------

  # validate parameters
  ValidateAge(x)
  val_nx = Validatenx(nx, x, last_open)
  nx_ = val_nx[["nx"]]
  val_nax = Validatenax(nax, x, nx, last_open)
  nax_ = val_nax[["nax"]]

  # validate data (modify in case open/close life-table)
  Validatenqx(nqx)
  valid_last_qx = ValidateLastnqx(x, nqx, nax_, nx_, last_open)
  x_ = valid_last_qx[["x"]]
  nqx_ = valid_last_qx[["nqx"]]
  nx_ = valid_last_qx[["nx"]]
  nax_ = valid_last_qx[["nax"]]
  last_open = valid_last_qx[["last_open"]]

  # k: number of age-groups
  k = length(x_)

  # Build life-table --------------------------------------------------------

  # npx: probability of surviving in age group [x, x+n) given survival to age x
  npx = 1-nqx_
  # lx: probability to survive up until age x.
  lx = c(1, cumprod(npx)[-k])
  # ndx: life-table deaths in age group [x, x+n)
  ndx = lx*nqx_
  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(val_nax[["nax_mode"]], "udd")) {
    nax_ = naxUDD(nx_, k, last_open)
  }
  if (identical(val_nax[["nax_mode"]], "cfm")) {
    nax_ = naxCFMfromnqx(x_, nx_, nqx_, npx, k, last_open)
  }

  # nLx: amount of subject-time at risk in age group [x, x+n)
  nLx = nx_*(lx-ndx) + nax_*ndx
  # nmx: life-table mortality rate in age group [x, x+n]
  nmx = ndx/nLx
  if (identical(last_open, TRUE)) {
    # in case of an open age group nmx will be NA for this age group (as the
    # width of the open age group is unknown). therefore we log-linearly
    # extrapolate nmx based on the preceding two nmx and calculate nLx of the
    # last age group using the constant hazard assumption.
    if (val_nax[["nax_mode"]] %in% c("udd", "cfm")) {
      nmx[k] = LinearExtrapolation(x = x_[c(k-2, k-1)], y = nmx[c(k-2, k-1)],
                                   xextra = x_[k], loga = TRUE)
      nax_[k] = 1/nmx[k]
      nLx[k] = nax_[k]*lx[k]
      message("Inputnqx() and last_open = TRUE: nmx of open age group log-linearly extrapolated based on preceding two nmx.")
    }
    if (val_nax[["nax_mode"]] %in% c("scalar", "vector")) {
      nLx[k] = nax_[k]*lx[k]
      nmx[k] = ndx[k]/nLx[k]
    }
  }

  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx
  # when lx becomes 0 ex becomes NaN. set it to 0
  ex[is.nan(ex)] = 0

  # Construct pash object ---------------------------------------------------

  # construct the pace-shape object, a validated life-table
  pash = ConstructPash(
    x = x_, nx = nx_, nmx = nmx, nax = nax_,
    nqx = nqx_, npx = npx, lx = lx, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = val_nax[["nax_mode"]], last_open = last_open,
    type = "nqx",
    input = list(x = x, nmx = nmx, nax = nax, nx = nx)
  )

  return(pash)

}

# nax Calculation ---------------------------------------------------------

#' Calculate nax From nx Using Uniform Distribution of Deaths Assumption
#'
#' Get life-table nax values from nx using the "uniform distribution of deaths"
#' (midpoint) assumption.
#'
#' @param nx Width of age interval [x, x+n).
#' @param k  Number of age groups.
#' @param last_open Is the last age group open (TRUE) or closed (FALSE)?
#'
#' @return The nax as a numeric vector of \code{length(nx)}.
#'
#' @details
#' nax is NA for last open age group.
#'
#' @keywords internal
naxUDD <- function (nx, k, last_open) {
  nax = 0.5*nx
  return(nax)
}

#' Calculate nax From nqx Using Constant Force of Mortality Assumption
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
#' If the last age group is closed we linearly extrapolate the last nax from the
#' preceding two nax.
#'
#' If the last age group is open we set the last nax to NA.
#'
#' @keywords internal
naxCFMfromnqx <- function (x, nx, nqx, npx, k, last_open) {
  nax = -nx/nqx - nx/log(npx) + nx
  if (identical(last_open, FALSE)) {
    # the analytic expression above can't return a nax value for the
    # last age group (when survivorship hits 0). Therefore we extrapolate
    # the last nax value based on the preceding 2 nax values
    nAx = nax/nx
    nAk = LinearExtrapolation(x = x[c(k-2, k-1)], y = nAx[c(k-2, k-1)],
                              xextra = x[k], loga = FALSE)
    nax[k] = nAk*nx[k]
  }
  if (identical(last_open, TRUE)) {
    # without knowing the width of the last age group there is no way to know
    # the last nax value
    nax[k] = NA
  }
  return(nax)
}

#' Calculate nax From nmx Using Constant Force of Mortality Assumption
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
naxCFMfromnmx <- function (nx, nmx, k, last_open) {
  nax = nx + 1/nmx - nx/(1 - exp(-nx*nmx))
  if (identical(last_open, TRUE)){
    nax[k] = 1/nmx[k]
  }
  return(nax)
}

#' Convert Death Rates to Death Probabilities
#'
#' @param nmx Mortality rate in age interval [x, x+nx).
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval.
#' @param nx Width of age interval [x, x+n).
#' @param mode Conversion formula. "chiang", "cfm", or "udd".
#'
#' @details Chiangs formula for the nmx->nqx conversion is
#'
#' nqx = nx*nmx / (1+(nx-nax)*nmx).
#'
#' It may produce probabilities > 1 in cases where age groups are wide, hazards
#' are high and the nax is approximated using the uniform distribution of deaths
#' or the constant force of mortality assumption. Therefore in these cases we
#' use other conversion formulas.
#'
#' nqx = 1 - exp(-nmx*nx),
#'
#' assuming a constant force of mortality within the age interval [x, x+n), and
#'
#' nqx = 1 - (1-nmx / (1+0.5*nmx))^nx,
#'
#' assuming a uniform distribution of deaths within the age interval [x, x+n).
#'
#' @keywords internal
nmxTonqx <- function (nmx, nax, nx, mode) {
  if (identical(mode, "chiang")) { nqx = nx*nmx / (1+(nx-nax)*nmx) }
  if (identical(mode, "cfm")) { nqx = 1 - exp(-nmx*nx) }
  if (identical(mode, "udd")) { nqx = 1 - (1-nmx / (1+0.5*nmx))^nx }
  return(nqx)
}
