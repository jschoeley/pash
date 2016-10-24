# Test if Object is Pace-Shape
TestClass <- function (x) {
  if (!is.pash(x)) stop("x is not a pace-shape object.")
}

# Test for NAs in Function Parameters
TestNA <- function (x, name) {
  if (any(is.na(x))) stop(paste("NA found in", name, "input."))
}

# Test if Last Value in nx is NA
TestLastnxNA <- function (x) {
  if (!is.na(rev(x)[1L])) warning("Last value in nx vector must be NA if the last age group is open. Last value is set to NA.")
}

# Set Last Value in Vector to NA
SetLastToNA <- function (x) {
  x[length(x)] = NA
  return(x)
}

# Test if Vector is Same Length as Age Vector
TestLength <- function (a, x, name_a) {
  if (!identical(length(a), length(x))) stop(paste("Input", name_a, "is not of same length as age vector."))
}

# Test if Age Vector Matches nx Vector
TestnxMatchx <- function(x, nx) {
  if(any(diff(x) != nx[-length(nx)])) stop("Provided Age and nx vectors don't match.")
}

# Validate Inputlx Arguments
InputlxValidation <- function(x, lx, nax, nx, k, last_open) {

  # check basic input sanity
  TestNA(x, "x")
  if (!is.numeric(x)) stop("Age must be numeric.")
  if (!is.numeric(lx)) stop("lx must be numeric.")
  if (!is.numeric(lx)) stop("lx must be numeric.")
  if (any(x < 0L)) stop("Age can't be negative.")
  if (any(lx < 0L)) stop("lx can't be negative.")

  # for all combinations of possible nax and nx modes,
  # do the appropriate checks and operations

  if (identical(last_open, TRUE)) {
    if (identical(nax, "midpoint")) {
      warning("Midpoint method used in conjuction with open last age group. nax from preceeding age group will stand in for nax in open last age group.")

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        nx = rep(nx, k)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx[-k], "nx")
        nx = rep(nx, k)
        if (!TestLastnxNA(nx)) nx = SetLastToNA(nx)
        TestLength(nx, x , "nx")
      }
    }

    if (!identical(nax, "midpoint") && identical(length(nax), 1L)) {
      warning("nax scalar value will be used for open last age group.")
      TestNA(nax, "nax"); nax = rep(nax, k)

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        nx = rep(nx, k); nx = SetLastToNA(nx)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx[-k], "nx")
        if (!TestLastnxNA(nx)) nx = SetLastToNA(nx)
        TestLength(nx, x, "nx")
        TestnxMatchx(x, nx)
      }
    }

    if (!identical(length(nax), 1L)) {
      TestNA(nax, "nax"); TestLength(nax, x, "nax")

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        nx = rep(nx, k); nx = SetLastToNA(nx)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx[-k], "nx")
        if (!TestLastnxNA(nx)) nx = SetLastToNA(nx)
        TestnxMatchx(x, nx)
        TestLength(nx, x, "nx")
      }
    }
  }

  if (identical(last_open, FALSE)) {
    if (identical(nax, "midpoint")) {

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        nx = rep(nx, k); nx = SetLastToNA(nx)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        TestLength(nx, x, "nx")
        TestnxMatchx(x, nx)
      }
    }

    if (!identical(nax, "midpoint") && identical(length(nax), 1L)) {
      TestNA(nax, "nax"); nax = rep(nax, k)

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx"); rep(nx, k)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        TestLength(nx, x, "nx")
        TestnxMatchx(x, nx)
      }
    }

    if (!identical(length(nax), 1L)) {
      TestNA(nax, "nax"); TestLength(nax, x, "nax")

      if (!identical(nx, "auto") && identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        nx = rep(nx, k)
        TestnxMatchx(x, nx)
      }
      if (!identical(length(nx), 1L)) {
        TestNA(nx, "nx")
        TestLength(nx, x, "nx")
        TestnxMatchx(x, nx)
      }
    }
  }

  return(list(nax = nax, nx = nx))

}
