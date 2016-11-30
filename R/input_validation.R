#' Difference Age Vector
#'
#' Get width of age groups from differenced age vector.
#'
#' @param x Start of age interval.
#' @param last_open Is the last age group open (TRUE) or closed (FALSE,
#'   default).
#'
#' @return The widths of the age groups as a numeric vector of \code{length(x)}.
#'
#' @details
#' If the last age group is closed it is assumed to have the same width as the
#' preceeding age group. If the last age group is open, the width is set to
#' \code{NA}.
#'
#' @keywords internal
DiffAge <- function (x, last_open = FALSE) {
  k  = length(x)
  nx = diff(x)
  if (identical(last_open, FALSE)) nx = c(nx, nx[k-1])
  if (identical(last_open, TRUE)) nx = c(nx, NA)
  return(nx)
}

# Various Validation Functions --------------------------------------------

#' Test if Object is Pace-Shape
#' @keywords internal
TestClass <- function (x) {
  if (!is.pash(x)) { stop("x is not a pace-shape object.", call. = FALSE) }
}

#' Set Last Value in Vector to NA
#' @keywords internal
SetLastToNA <- function (x) {
  x[length(x)] = NA
  return(x)
}

#' Test if Age Vector Matches nx Vector
#' @keywords internal
TestnxMatchx <- function (x, nx) {
  if(any(diff(x) != nx[-length(nx)])) { stop("Provided Age and nx vectors don't match.", call. = FALSE) }
}

#' Check Input for NA, NaN and Inf
#' @keywords internal
IsNANanInf <- function (x, name_x, test = 1:3) {
  if (any(is.na(x)) && !any(is.nan(x)) && 1 %in% test) { stop(paste(name_x, "must not contain NA."), call. = FALSE) }
  if (any(is.nan(x)) && 2 %in% test) { stop(paste(name_x, "must not contain NaN."), call. = FALSE) }
  if (any(is.infinite(x)) && 3 %in% test) { stop(paste(name_x, "must not contain (-)Inf."), call. = FALSE) }
}

# Validate Input Data -----------------------------------------------------

#' Validate lx
#' @keywords internal
Validatelx <- function (lx) {
  if (!is.numeric(lx)) { stop("lx must be numeric.", call. = FALSE) }
  if (any(lx < 0L)) { stop("lx can't be negative.", call. = FALSE) }
  if (is.unsorted(rev(lx))) { stop("lx must be monotonically decreasing.", call. = FALSE) }
}

#' Validate mx
#' @keywords internal
Validatemx <- function (mx) {
  if (!is.numeric(mx)) { stop("mx must be numeric.", call. = FALSE) }
  if (any(mx < 0L)) { stop("mx must not be negative.", call. = FALSE) }
}

# Validate Parameters -----------------------------------------------------

#' Validate Age
#' @keywords internal
ValidateAge <- function (x) {
  IsNANanInf(x, "The age vector")
  if (!is.numeric(x)) { stop("Age must be numeric.", call. = FALSE) }
  if (!(length(x) > 2L)) { stop("The age vector must contain at least 3 age groups.", call. = FALSE) }
  if (any(x < 0L)) { stop("Age must not be negative.", call. = FALSE) }
  if (!all(diff(x) > 0L)) { stop("The age vector must be arranged in increasing order.", call. = FALSE) }
}

#' Validate last_open
#' @keywords internal
ValidateLastOpen <- function (last_open) {
  IsNANanInf(last_open, "last_open")
  if (!is.logical(last_open)) { stop("last_open must be logical.", call. = FALSE) }
  if (!identical(length(last_open), 1L)) { stop("last_open must be scalar.", call. = FALSE) }
}

#' Validate nx
#' @keywords internal
Validatenx <- function (nx, x, last_open) {
  IsNANanInf(nx, "nx", test = 2:3)
  # numeric nx
  if (is.numeric(nx)) {
    # numeric vector nx
    if (length(nx) > 1L) {
      if (!identical(length(nx), length(x))) { stop("nx vector must be of same length as x.", call. = FALSE) }
      nx_mode = "vector"
      nx_ = nx
      if (identical(last_open, TRUE)) { nx_ = SetLastToNA(nx_) }
    } else {
      # numeric scalar nx
      if (!identical(length(nx), 1L)) { stop("nx must be of same length as x or scalar.", call. = FALSE) }
      if (is.na(nx)) { stop("nx must not be NA if scalar.") }
      nx_mode = "scalar"
      nx_ = rep(nx, length(x))
      if (identical(last_open, TRUE)) { nx_ = SetLastToNA(nx_) }
    }
    TestnxMatchx(x, nx_)
  } else { # character nx
    if (!is.character(nx)) {stop("nx must be numeric or character.") }
    if (!identical(length(nx), 1L)) { stop("nx must be scalar if character.") }
    if (!identical(nx, "auto")) { stop("nx must be 'auto' if character.") }
    nx_mode = "auto"
    nx_ = DiffAge(x, last_open = last_open)
  }

  return(list(nx = nx_, nx_mode = nx_mode))

}

#' Validate nax
#' @keywords internal
Validatenax <- function (nax, x, nx, last_open) {
  IsNANanInf(nax, "nax")
  # numeric nax
  if (is.numeric(nax)) {
    # numeric vector nax
    if (length(nax) > 1L) {
      if (!identical(length(nax), length(x))) { stop("nax vector must be of same length as x.", call. = FALSE) }
      nax_mode = "vector"
      nax_ = nax
    } else {
      # numeric scalar nax
      if (!identical(length(nax), 1L)) { stop("nax must be of same length as x or scalar.", call. = FALSE) }
      nax_mode = "scalar"
      nax_ = rep(nax, length(x))
    }
    if (any(nax_ > nx)) { stop("nax must not be larger than nx.", call. = FALSE) }
  } else { # character nax
    if (!is.character(nax)) { stop("nax must be numeric or character.") }
    if (!identical(length(nax), 1L)) { stop("nax must be scalar if character.") }
    if (!(nax %in% c("udd", "cfm"))) { stop("nax mode must be either 'udd' or 'cfm'.") }
    nax_mode = nax
    nax_ = nax
  }

  return(list(nax = nax_, nax_mode = nax_mode))

}
