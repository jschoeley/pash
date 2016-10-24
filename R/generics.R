#' Print Pace-Shape Object
print.pash <- function (x, radix = 10000, big.mark = ",") {
  lt = x[["lt"]]
  df = data.frame(
    x   = paste0("[", lt$x, ",", lt$x+lt$nx, ")"),
    nax = formatC(lt$nax,       format   = "f", digits = 2, drop0trailing = TRUE),
    nmx = formatC(lt$nmx,       format   = "e", digits = 3),
    nqx = formatC(lt$nqx,       format   = "e", digits = 3),
    npx = formatC(lt$npx,       format   = "e", digits = 3),
    lx  = formatC(lt$lx*radix,  format   = "d", digits = 0, big.mark = big.mark),
    ndx = formatC(lt$ndx*radix, format   = "d", digits = 0, big.mark = big.mark),
    nLx = formatC(lt$nLx*radix, format   = "d", digits = 0, big.mark = big.mark),
    Tx  = formatC(lt$Tx*radix,  format   = "d", digits = 0, big.mark = big.mark),
    ex  = formatC(lt$ex,        format   = "f", digits = 2)
  )
  cat("A life table with", length(lt$x), "age groups.\n")
  print.data.frame(df, row.names = FALSE, quote = FALSE)
  invisible(x)
}

#' Summarise Pace-Shape Object
summary.pash <- function (x) {
  lt = x[["lt"]]
  cat("A life table with", length(lt$x), "age groups.\nSource:",
      attr(x, "source")$type, "\n\n")
  cat("Average life expectancy :",
      formatC(lt$ex[1], format = "f", digits = 2), attr(x, "time_unit"), "\n")
  cat("Avg. e0 lost upon death :",
      formatC(EDagger(lt), format = "f", digits = 2), attr(x, "time_unit"), "\n")
  cat("Life table entropy      :",
      formatC(GetShape(x, type = "LTentr"), format = "f", digits = 3))
}

#' Convert Pace-Shape Object To Data Frame
as.data.frame.pash <- function (x) {
  x[["lt"]]
}

#' Convert Pace-Shape Object To Matrix
as.matrix.pash <- function (x) {
  matrix(unlist(x[["lt"]]), ncol = 11, dimnames = list(NULL, names(x[["lt"]])))
}

#' Test if Object is Pace-Shaoe
is.pash <- function (x) {
  inherits(x, "pash")
}
