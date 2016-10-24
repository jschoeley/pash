#' Lifetable Shape Measures
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
#'   \item{\code{"maxx"}}{Maximum lifespan.}
#'   \item{\code{"all"}}{All of the above measures}
#' }
#'
#' @source Wrycza, Tomasz, and Annette Baudisch. 2014.
#' "The Pace of Aging: Intrinsic Time Scales in Demography."
#' Demographic Research 30 (1): 1571-90. doi:10.4054/DemRes.2014.30.57.
#'
#' @examples
#' pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
#' GetPace(pash)
#'
#' @export
GetPace <- function (pash, type = "all", q = 0.5) {
  TestClass(pash)
  lt = pash[["lt"]]
  if (identical(type, "e0")) S = c(e0 = TotalLifeExpectancy(lt$ex))
  if (identical(type, "qlx")) S = c(qlx = SurvivalQuantile(lt$x, lt$lx, q))
  if (identical(type, "maxx")) S = c(maxx = MaximumLifespan(lt$x, attr(pash, "last_open")))
  if (identical(type, "all")) {
    S = c(e0 =  TotalLifeExpectancy(lt$ex),
          qlx = SurvivalQuantile(lt$x, lt$lx, q),
          maxx = MaximumLifespan(lt$x, attr(pash, "last_open")))
  }
  return(S)
}

# Total Life Expectancy
TotalLifeExpectancy <- function (ex) {
  return(ex[1])
}

# Survival Quantile
SurvivalQuantile <- function (x, lx, q) {
  return(x[lx<=q][1])
}

# Maximum Lifespan
MaximumLifespan <- function (x, last_open) {
  if (identical(last_open, TRUE)) warning("Last age group is open. No precise maximum recorded life-span can be provided")
  return(max(x))
}
