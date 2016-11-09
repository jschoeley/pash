#' Life-table Pace Measures
#'
#' Get life-table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which pace measure should be returned (default \code{"all"})?
#' @param q Quantile specification for age where q percent of the life-table
#'   population is still alive (defaults to median).
#'
#' @details
#' The type argument accepts the following strings:
#' \describe{
#'   \item{\code{"e0"}}{Total life expectancy.}
#'   \item{\code{"qlx"}}{Age where q percent of the life table population is still alive.}
#'   \item{\code{"all"}}{All of the above measures}
#' }
#'
#' @section l(x) quantile interpolation:
#' Exact values for the quantiles are determined by either linear- or
#' exponential interpolation of the l(x) function. Given a value q we first find
#' the x and l(x) at the boundaries of the interval containing l(x) = q. Then we
#' return the x where l(x) = q by either:
#' \describe{
#'   \item{linear interpolation}{In case of nax = \code{"scalar" | "vector" |
#'   "midpoint"} then x = (-x1*q+x0*q-x0*y1+x1*y0)/(y0-y1).}
#'   \item{exponential interpolation}{In case of nax = \code{"constant_nmx"}
#'   then x = ((x0-x1)*log(y)-x0*log(y1)+x1*log(y0))/(log(y0)-log(y1))}
#' }
#'
#' @source Wrycza, Tomasz, and Annette Baudisch. 2014.
#' "The Pace of Aging: Intrinsic Time Scales in Demography."
#' Demographic Research 30 (1): 1571-90. doi:10.4054/DemRes.2014.30.57.
#'
#' @examples
#' pash <- Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetPace(pash)
#'
#' # the age only 30% of the life-table population reaches
#' GetPace(pash, q = 0.3)
#' # the estimate changes with changing nax assumptions
#' pash <- Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, nax = "constant_nmx")
#' GetPace(pash, q = 0.3)
#' @export
GetPace <- function (pash, type = "all", q = 0.5) {
  TestClass(pash)
  lt = pash[["lt"]]
  if (identical(type, "e0")) {S = c(e0 = TotalLifeExpectancy(lt$ex))}
  if (identical(type, "qlx")) {
    S = c(qlx = SurvivalQuantile(lt$x, lt$nx, lt$lx, q,
                                 nax_mode = attr(pash, "nax_mode")))
  }
  if (identical(type, "all")) {
    S = c(e0 =  TotalLifeExpectancy(lt$ex),
          qlx = SurvivalQuantile(lt$x, lt$nx, lt$lx, q,
                                 nax_mode = attr(pash, "nax_mode")))
  }
  return(S)
}

# Total Life Expectancy
TotalLifeExpectancy <- function (ex) {
  return(ex[1])
}

# Get Exact x for a Given y Based on Linear Interpolation Between Two Points
LinearInterpolation <- function (x0, x1, y0, y1, y) {
  x = (-x1*y+x0*y-x0*y1+x1*y0)/(y0-y1)
  return(x)
}
# Get Exact x for a Given y Based on Exponential Interpolation Between Two Points
ExponentialInterpolation <- function (x0, x1, y0, y1, y) {
  x = ((x0-x1)*log(y) - x0*log(y1) + x1*log(y0)) / (log(y0)-log(y1))
  return(x)
}

# Survival Quantile
SurvivalQuantile <- function (x, nx, lx, q, nax_mode) {
  # add 0 to l(x) vector so that a result is returned in case l(k) > q
  # note that exponential interpolation can't be used when l(k) > q
  # therefore we fall back to linear interpolation in this case
  lx = c(lx, 0)
  x  = c(x, x[length(x)]+nx[length(nx)])
  # should an lx correspond exactly to the specified quantile, return it
  if (any(lx == q)) {
    qlx = x[which(lx == q)[1L]] # could be more than 1 if lx stays flat, return 1st
  } else {
    # else, find the two lx's enclosing the the true quantile and interpolate
    lo = rev(which(lx>q))[1]; up = which(lx<q)[1]
    x0 = x[lo]; x1 = x[up]
    y0 = lx[lo]; y1 = lx[up]
    # linear interpolation between two lx in case of midpoint nax method
    if (identical(nax_mode, "midpoint")) {
      qlx = LinearInterpolation(x0, x1, y0, y1, q)
    }
    # exponential interpolation between two lx in case of constant_nmx nax method
    if (identical(nax_mode, "constant_nmx") && !identical(up, length(lx))) {
      qlx = ExponentialInterpolation(x0, x1, y0, y1, q)
    }
    # fallback to linear interpolation in case where l(k) > q and
    # constant_nmx nax method is specified
    if (identical(nax_mode, "constant_nmx") && identical(up, length(lx))) {
      qlx = LinearInterpolation(x0, x1, y0, y1, q)
    }
    # linear interpolation in case of scalar or vector nax
    # until I have a better idea
    if (identical(nax_mode, "scalar") || identical(nax_mode, "vector")) {
      qlx = LinearInterpolation(x0, x1, y0, y1, q)
    }
  }
  return(qlx)
}
