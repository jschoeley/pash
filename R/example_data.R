#' Period Lifetable Survival Function for Austrian Males 1992
#'
#' An abridged period life-table lx column for Austrian males in 1992 used in
#' an example on life table construction in Preston's demography textbook.
#' @format
#'   A data frame with 19 rows and 2 variables:
#'   \describe{
#'     \item{x}{Start of age interval in years.}
#'     \item{lx}{Survivors at age x.}
#'   }
#'
#' @source Preston, Samuel H., Patric Heuveline, and Michel Guillot. 2001.
#'         Demography. Oxford, UK: Blackwell, p. 51 and United Nations 1994.
#'
#' @export
prestons_lx <-
  data.frame(x = c(0, 1, seq(5, 85, 5)),
             lx = c(100000, 99133, 98986, 98910, 98815,
                    98334, 97704, 97151, 96492, 95588,
                    94195, 91937, 88711, 83845, 76377,
                    66225, 53803, 37441, 21134))

