#' pash: Pace-Shape Analysis for Life-tables
#'
#' \code{pash} lets you perform pace-shape analysis on life-tables. Calculate a
#' wide array of pace and shape metrics and use them to standardize and compare
#' your data.
#'
#' First: Ceate a pace-shape object from either a life-table column or a
#' population matrix using one of the input function: \code{\link{Inputlx}},
#' \code{\link{Inputmx}}.
#'
#' Second: Calculate pace measures with \code{\link{GetPace}}, calculate shape
#' measures with \code{\link{GetShape}}, standardize a life table by pace and
#' shape with \code{\link{StandardizeLT}}, rebase your life table to a different
#' age with \code{\link{RebaseLT}}.
#'
#' @docType package
#' @name pash
NULL

# Pace-Shape Object Constructor -------------------------------------------

#' Pace-Shape Object Constructor
#'
#' Construct a pace-shape object.
#'
#' @param x Start of age interval.
#' @param nx Width of age interval [x, x+nx). No zero width allowed.
#' @param nmx Mortality rate in age interval [x, x+nx) measured in deaths per
#'   unit of subject-time.
#' @param nax Subject-time spent in age interval [x, x+nx) when dying in that
#'   interval.
#' @param nqx Probability to die within age interval [x, x+nx) given survival to
#'   x. Must be in range [0, 1].
#' @param npx Probability to survive age interval [x, x+nx) given survival to x.
#'   A numeric vector. Must be in range [0, 1].
#' @param lx Life table survivors at age x. Radix must be 1. Must be in range [0, 1].
#' @param ndx Deaths by life table population in age interval [x, x+nx).
#' @param nLx Total subject-time lived by life table population in age interval [x, x+nx).
#' @param Tx Total subject-time yet to live past age x by life table population.
#'   A numeric vector.
#' @param ex Life expectancy at age x. A numeric vector.
#' @param last_open Is the last age group open? Boolean scalar.
#' @param time_unit The unit of the ages. A scalar string.
#' @param type The type of input used to create the pace-shape object (e.g.
#'   "lx). A scalar string.
#' @param input The raw input of the Input[*] function. Free format.
#'
#' @details Unless otherwise noted in the argument description the input
#' arguments must be non-negative numeric vectors of length > 1 and length ==
#' length(x). NA, NaN, Inf, -Inf are not allowed.
#'
#' @return An S3 object of class \code{pash}.
#'
#' @section Warning:
#' Only use when writing new Input functions. For pace-shape object construction
#' use the existing Input functions provided by \code{pash}.
#'
#' @keywords internal
ConstructPash <- function (x, nx, nmx, nax, nqx, npx, lx, ndx, nLx, Tx, ex,
                           last_open, nax_mode, time_unit, type, input) {

  lt = data.frame(x = x, nx = nx, nmx = nmx, nax = nax,
                  nqx = nqx, npx = npx, lx = lx, ndx = ndx,
                  nLx = nLx, Tx = Tx, ex = ex)

  pash =
    structure(
      list(lt = lt),
      class = "pash",
      non_destructive_copy = lt,
      last_open = last_open,
      nax_mode = nax_mode,
      time_unit = time_unit,
      source = list(type = type, input = input)
    )

  return(pash)

}

# Aux ---------------------------------------------------------------------

# Width of Age Groups From Differenced Age Vector
DiffAge <- function (x, last_open = FALSE) {
  k  = length(x)
  nx = diff(x)
  if (identical(last_open, FALSE)) nx = c(nx, nx[k-1])
  if (identical(last_open, TRUE)) nx = c(nx, NA)
  return(nx)
}

# Life Table ax Values Using The Midpoint Method
naxMidpoint <- function (nx, k, last_open) {
  nax = 0.5*nx
  if (identical(last_open, TRUE)) {
    nax[k] = nax[k-1L]
  }
  return(nax)
}

# Life Table ax Values Using Assumption of Constant nmx
# starting with probabilities
naxConstantnmx1 <- function (x, nx, nqx, npx, k, last_open) {
  # analytical style
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

# Life Table ax Values Using Assumption of Constant nmx
# starting with rates
naxConstantnmx2 <- function (nx, nmx, k, last_open) {
  nax = nx + 1/nmx - nx/(1 - exp(-nx*nmx))
  if (identical(last_open, TRUE)){
    nax[k] = 1/nmx[k]
  }
  return(nax)
}

# lx To Pash --------------------------------------------------------------

#' Convert a Life Table Survivorship Function to a Pace-Shape Object
#'
#' Given an age vector and corresponding survival probabilities a complete
#' lifetable is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param lx Probability to survive up until age x.
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (see details for options).
#' @param nx Width of age interval [x, x+n) (see details for options).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE,
#'   default).
#' @param time_unit The unit of the ages (by default "years").
#'
#' @details For nx you may provide a scalar, a vector numeric, or let the
#'   function determine the width for you (default). A scalar will be recycled
#'   for each age group. A vector must be as long as the age vector and allows
#'   you to specify the width of each age group seperately. If the last age
#'   group is supposed to be open make sure that the last value of your nx
#'   vector is NA. By default the width of the age groups is guessed from the
#'   age vector. The width of the last age group is assumed to be equal to the
#'   width of the preceeding age group.
#'
#'   Likewise to nx, nax may be provided as a scalar, a vector, or automatically
#'   calculated via the midpoint method (default) or the constant nmx assumption
#'   (option "\code{constant_nmx}").
#'
#'   The midpoint method assumes a linear decline of the lx function over the
#'   width of an age group, implying that those who die in that age group die on
#'   average halfway into it:
#'
#'   nax = n/2 (see Preston, 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n] to be constant
#'   implies an exponentially declining lx function within [x, x+n] and will
#'   produce ax values smaller than those calculated via the midpoint method.
#'   Preston (2001), p. 46 provides an expression for nax given the assumption
#'   of constant mortality. Restating this expression in terms of nqx and npx
#'   leads to:
#'
#'   nax = -n/nqx - n/ln(npx) + n
#'
#'   Should you specify an open last age group and nax estimation via the
#'   midpoint method (not recomended) the nax of the last open
#'   age group will be set to the nax of the preceeding age group.
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

  # keep the argument values
  nax_arg = nax
  nx_arg = nx

  # validate data
  ValidateAge(x)
  Validatelx(lx)

  # validate options
  valid_input = ValidateOptions(x, nax_arg, nx_arg, k, last_open)
  nax = valid_input[["nax"]]
  nx = valid_input[["nx"]]

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

  # nx: width of age groups [x, x+n)
  if (identical(nx_arg, "auto")) {
    nx = DiffAge(x, last_open)
  }

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(nax_arg, "midpoint")) {
    nax = naxMidpoint(nx, k, last_open)
  }
  if (identical(nax_arg, "constant_nmx")) {
    nax = naxConstantnmx1(x, nx, nqx, npx, k, last_open)
  }

  # nLx: amount of subject-time at risk in age group [x, x+n)
  # The nx handling needs to be improved in case of open age groups
  nx_ = if (identical(last_open, TRUE)) c(nx[-k], nx[k-1L]) else nx
  nLx = nx_*(lx_-ndx) + nax*ndx
  # nmx: life table mortality rate in age group [x, x+n]
  nmx = ndx/nLx
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx_

  # Construct pash object ---------------------------------------------------

  # what kind of nax where provided?
  if (all(is.numeric(nax_arg)) && length(nax_arg) > 1L) {nax_mode = "vector"}
  if (all(is.numeric(nax_arg)) && length(nax_arg) == 1L) {nax_mode = "scalar"}
  if (identical(nax_arg, "midpoint")) {nax_mode = "midpoint"}
  if (identical(nax_arg, "constant_nmx")) {nax_mode = "constant_nmx"}

  # construct the pace-shape object, a validated life table
  pash =  ConstructPash(
    x = x, nx = nx, nmx = nmx, nax = nax,
    nqx = nqx, npx = npx, lx = lx_, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = nax_mode, last_open = last_open,
    type = "lx", input = data.frame(x = x, lx = lx)
  )

  return(pash)

}

# mx To Pash --------------------------------------------------------------

#' Convert a Life Table Mortality Rate Function to a Pace-Shape Object
#'
#' Given an age vector and corresponding mortality rates a complete
#' lifetable is calculated and a pace-shape object constructed.
#'
#' @param x Start of the age interval.
#' @param nmx Risk of death in age interval [x, x+n)
#' @param nax Subject-time alive in [x, x+n) for those who die in same interval
#'   (see details for options).
#' @param nx Width of age interval [x, x+n) (see details for options).
#' @param last_open Is the last age group open (TRUE) or closed (FALSE,
#'   default).
#' @param time_unit The unit of the ages (by default "years").
#'
#' @details For nx you may provide a scalar, a vector numeric, or let the
#'   function determine the width for you (default). A scalar will be recycled
#'   for each age group. A vector must be as long as the age vector and allows
#'   you to specify the width of each age group seperately. If the last age
#'   group is supposed to be open make sure that the last value of your nx
#'   vector is NA. By default the width of the age groups is guessed from the
#'   age vector. The width of the last age group is assumed to be equal to the
#'   width of the preceeding age group.
#'
#'   Likewise to nx, nax may be provided as a scalar, a vector, or automatically
#'   calculated via the midpoint method (default) or the constant nmx assumption
#'   (option "\code{constant_nmx}").
#'
#'   The midpoint method assumes a linear decline of the lx function over the
#'   width of an age group, implying that those who die in that age group die on
#'   average halfway into it:
#'
#'   nax = n/2 (see Preston, 2001, p. 46)
#'
#'   Assuming the mortality rate during age interval [x, x+n] to be constant
#'   implies an exponentially declining lx function within [x, x+n] and will
#'   produce ax values smaller than those calculated via the midpoint method.
#'   Preston (2001), p. 46 provides an expression for nax given the assumption
#'   of constant mortality.
#'
#'   nax = nx + 1/nmx - nx/(1 - exp(-nx*nmx))
#'
#'   Should you specify an open last age group and nax estimation via the
#'   midpoint method (not recomended) the nax of the last open
#'   age group will be set to the nax of the preceeding age group.
#'
#' @source Preston, Samuel H., Patric Heuveline, and Michel Guillot. 2001.
#'   Demography. Oxford, UK: Blackwell.
#'
#' @return A pace-shape object.
#'
#' @examples
#' swe <- subset(sweden5x5, sex == "female" & period == "1940-1944")[c("x", "nmx")]
#' Inputmx(x = swe$x, nmx = swe$nmx, last_open = TRUE, nax = "constant_nmx")
#'
#' @export
Inputmx <- function (x, nmx,
                     nax = "midpoint",
                     nx = "auto",
                     last_open = FALSE,
                     time_unit = "years") {

  # Input validation --------------------------------------------------------

  # k: number of age-groups
  k = length(x)

  # keep the argument values
  nax_arg = nax
  nx_arg = nx

  # validate data
  ValidateAge(x)
  Validatemx(nmx)

  # validate options
  valid_input = ValidateOptions(x, nax_arg, nx_arg, k, last_open)
  nax = valid_input[["nax"]]
  nx = valid_input[["nx"]]

  # Build life table --------------------------------------------------------

  # nx: width of age groups [x, x+n)
  if (identical(nx_arg, "auto")) {
    nx = DiffAge(x, last_open)
  }

  # nax: amount of subject-time at risk in age group [x, x+n)
  # contributed by those who die in that age group
  if (identical(nax_arg, "midpoint")) {
    nax = naxMidpoint(nx, k, last_open)
  }
  if (identical(nax_arg, "constant_nmx")) {
    nax = naxConstantnmx2(nx, nmx, k, last_open)
  }

  # nqx from nmx and nax
  nqx = nx*nmx / (1+(nx-nax)*nmx)
  nqx[k] = 1
  npx = 1-nqx
  # lx
  lx = 1*cumprod(npx)
  # ndx: lifetable deaths in age group [x, x+n)
  ndx = c(lx[-k] - lx[-1L], lx[k])
  # nLx: amount of subject-time at risk in age group [x, x+n)
  nLx = nx*(lx-ndx) + nax*ndx
  nLx[k] = lx[k]/nmx[k]
  # Tx: amount of subject-time at risk above age group [x, x+n)
  Tx = rev(cumsum(rev(nLx)))
  # ex: life expectancy at age x
  ex = Tx/lx

  # Construct pash object ---------------------------------------------------

  # what kind of nax where provided?
  if (all(is.numeric(nax_arg)) && length(nax_arg) > 1L) {nax_mode = "vector"}
  if (all(is.numeric(nax_arg)) && length(nax_arg) == 1L) {nax_mode = "scalar"}
  if (identical(nax_arg, "midpoint")) {nax_mode = "midpoint"}
  if (identical(nax_arg, "constant_nmx")) {nax_mode = "constant_nmx"}

  # construct the pace-shape object, a validated life table
  pash =  ConstructPash(
    x = x, nx = nx, nmx = nmx, nax = nax,
    nqx = nqx, npx = npx, lx = lx, ndx = ndx,
    nLx = nLx, Tx = Tx, ex = ex,
    time_unit = time_unit, nax_mode = nax_mode, last_open = last_open,
    type = "mx", input = data.frame(x = x, nmx = nmx)
  )

  return(pash)

}
