#' Print Pace-Shape Object
#'
#' Printing method for a pace-shape object
#'
#' @param x Pace-shape object.
#' @param radix Initial life-table population.
#' @param big.mark See \code{\link{formatC}}.
#' @param ... Additional arguments passed on to \code{\link{print.data.frame}}.
#'
#' @keywords internal
#' @export
print.pash <- function (x, radix = 10000, big.mark = ",", ...) {
  lt = x[["lt"]]
  df = data.frame(
    x   = paste0("[", lt[["x"]], ",", lt[["x"]]+lt[["nx"]], ")"),
    nax = formatC(lt[["nax"]],       format   = "f", digits = 2, drop0trailing = TRUE),
    nmx = formatC(lt[["nmx"]],       format   = "e", digits = 3),
    nqx = formatC(lt[["nqx"]],       format   = "e", digits = 3),
    npx = formatC(lt[["npx"]],       format   = "e", digits = 3),
    lx  = formatC(lt[["lx"]]*radix,  format   = "d", digits = 0, big.mark = big.mark),
    ndx = formatC(lt[["ndx"]]*radix, format   = "d", digits = 0, big.mark = big.mark),
    nLx = formatC(lt[["nLx"]]*radix, format   = "d", digits = 0, big.mark = big.mark),
    Tx  = formatC(lt[["Tx"]]*radix,  format   = "d", digits = 0, big.mark = big.mark),
    ex  = formatC(lt[["ex"]],        format   = "f", digits = 2)
  )
  cat("A life table with", length(lt[["x"]]), "age groups.\n")
  print.data.frame(df, row.names = FALSE, quote = FALSE, ...)
  invisible(x)
}

#' Summarize Pace-Shape Object
#'
#' Summary method for the pace-shape object.
#'
#' @param object A pace-shape object.
#' @param ... Additional arguments.
#'
#' @keywords internal
#' @export
summary.pash <- function (object, ...) {
  lt = object[["lt"]]
  cat("A life table with", length(lt[["x"]]), "age groups.\nSource:",
      attr(object, "source")[["type"]], "\n\n")
  cat("Average life expectancy :",
      formatC(lt[["ex"]][1L], format = "f", digits = 2), attr(object, "time_unit"), "\n")
  cat("Avg. e0 lost upon death :",
      formatC(EDagger(lt[["nax"]], lt[["nx"]], lt[["ndx"]], lt[["ex"]]), format = "f", digits = 2), attr(object, "time_unit"), "\n")
  cat("Life table entropy      :",
      formatC(GetShape(object, type = "LTentr"), format = "f", digits = 3))
}

#' Convert Pace-Shape Object to Data Frame
#'
#' Method to convert a pace-shape object to a data frame.
#'
#' @param x A pace-shape object.
#' @param row.names See \code{\link{as.data.frame}}.
#' @param optional See \code{\link{as.data.frame}}.
#' @param ... Additional arguments passed on to \code{\link{as.data.frame}}.
#'
#' @keywords internal
#' @export
as.data.frame.pash <- function (x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x[["lt"]], row.names, optional, ...)
}

#' Convert Pace-Shape Object to Matrix
#'
#' Method to convert a pace-shape object to a matrix.
#'
#' @param x A pace-shape object.
#' @param ... Additional arguments passed on to \code{\link{as.matrix}}.
#'
#' @keywords internal
#' @export
as.matrix.pash <- function (x, ...) {
  as.matrix(x[["lt"]], ...)
}

#' Test if Object is Pace-Shape
#'
#' Method to test if an object is of class \code{pash}.
#'
#' @param x A pace-shape object.
#'
#' @keywords internal
#' @export
is.pash <- function (x) {
  inherits(x, "pash")
}
