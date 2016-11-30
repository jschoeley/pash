#' pash: Pace-Shape Analysis for Life-tables
#'
#' \code{pash} lets you perform pace-shape analysis on life-tables. Calculate a
#' wide array of pace and shape metrics and use them to standardize and compare
#' your data.
#'
#' First: Create a pace-shape object using one of the input functions:
#' \code{\link{Inputlx}}, \code{\link{Inputnmx}}.
#'
#' Second: Calculate pace measures with \code{\link{GetPace}}, calculate shape
#' measures with \code{\link{GetShape}}, standardize a life table by pace and
#' shape with \code{\link{StandardizeLT}}, rebase your life table to a different
#' age with \code{\link{RebaseLT}}.
#'
#' @docType package
#' @name pash
NULL

#' Pace-Shape Object Constructor
#'
#' Construct a pace-shape object.
#'
#' @param x Start of age interval.
#' @param nx Width of age interval [x, x+nx). Must align with x. No zero width
#'   allowed.
#' @param nmx Mortality rate in age interval [x, x+nx) measured in deaths per
#'   unit of subject-time.
#' @param nax Subject-time spent in age interval [x, x+nx) when dying in that
#'   interval.
#' @param nqx Probability to die within age interval [x, x+nx) given survival to
#'   x. Must be in range [0, 1].
#' @param npx Probability to survive age interval [x, x+nx) given survival to x.
#'   A numeric vector. Must be in range [0, 1].
#' @param lx Life-table survivors at age x. Radix must be 1. Must be in range
#'   [0, 1].
#' @param ndx Deaths by life table population in age interval [x, x+nx).
#' @param nLx Total subject-time lived by life-table population in age interval
#'   [x, x+nx).
#' @param Tx Total subject-time yet to live past age x by life-table population.
#' @param ex Life expectancy at age x.
#' @param last_open Is the last age group open? Boolean scalar.
#' @param nax_mode The nax specification provided by the user. String scalar.
#' @param time_unit The unit of the ages. String scalar.
#' @param type The type of input used to create the pace-shape object. String
#'   scalar.
#' @param input The raw input arguments of the Input[*] function. A list.
#'
#' @details Unless otherwise noted in the argument description the input
#' arguments must be non-negative numeric vectors of length > 1 and length ==
#' length(x). NA, NaN, Inf, -Inf are not allowed.
#'
#' Argument \code{type} must be one of \code{c("lx", "nmx")}. Argument
#' \code{nax_mode} must be one of \code{c("udd", "cfm",
#' "vector", "scalar")}.
#'
#' @return An S3 object of class \code{pash}.
#'
#' @section Warning:
#' Only use when writing new Input functions. For pace-shape object construction
#' use the existing Input functions.
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
