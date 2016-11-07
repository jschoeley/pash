#' Life Table Pace Measures
#'
#' Get life table shape measures from pace-shape object.
#'
#' @param pash A pace-shape object.
#' @param type Which pace measure should be returned (see details for choices)?
#' @param q (optional) Quantile specification for age where q percent of the
#'   life table population is still alive (default 0.5).
#'
#' @details
#' The type argument accepts the following strings:
#' \describe{
#'   \item{\code{"e0"}}{Total life expectancy.}
#'   \item{\code{"qlx"}}{Age where q percent of the life table population is still alive.}
#'   \item{\code{"all"}}{All of the above measures}
#' }
#'
#' Exact values for the quantiles are determined by linear interpolation of the
#' lx function: qlx = (-x1*q+x0*q-x0*y1+x1*y0)/(y0-y1)
#'
#' @source Wrycza, Tomasz, and Annette Baudisch. 2014.
#' "The Pace of Aging: Intrinsic Time Scales in Demography."
#' Demographic Research 30 (1): 1571-90. doi:10.4054/DemRes.2014.30.57.
#'
#' @examples
#' pash <- Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetPace(pash)
#' # the age only 10% of the life-table population reaches
#' GetPace(pash, q = 0.1)
#'
#' @export
GetPace <- function (pash, type = "all", q = 0.5) {
  TestClass(pash)
  lt = pash[["lt"]]
  if (identical(type, "e0")) {S = c(e0 = TotalLifeExpectancy(lt$ex))}
  if (identical(type, "qlx")) {S = c(qlx = SurvivalQuantile(lt$x, lt$lx, q))}
  if (identical(type, "all")) {
    S = c(e0 =  TotalLifeExpectancy(lt$ex),
          qlx = SurvivalQuantile(lt$x, lt$lx, q))
  }
  return(S)
}

# Total Life Expectancy
TotalLifeExpectancy <- function (ex) {
  return(ex[1])
}

# Survival Quantile
SurvivalQuantile <- function (x, lx, q) {
  if (any(lx == q)) {
    qlx = lx[lx == q]
  } else {
    lo = rev(which(lx>q))[1]
    up = which(lx<q)[1]
    x0 = x[lo]; x1 = x[up]
    y0 = lx[lo]; y1 = lx[up]
    # linear interpolation
    qlx = (-x1*q+x0*q-x0*y1+x1*y0)/(y0-y1)
  }
  return(qlx)
}
