#' Period Life-tables for Sweden 1755-2014 by Sex
#'
#' A dataset containing abridged period life-tables for Sweden years 1755-2014
#' by sex in five year period intervals.
#'
#' @details
#'   Infant and early childhood mortality are given in 1 and 4 year age
#'   intervals. Other age groups are given in 5 year intervals.
#'
#'   The dataset has been altered in form from the original provided by the
#'   Human Mortality database.
#'
#' @format
#'   A data frame with 2,496 rows and 12 variables:
#'   \describe{
#'     \item{sex}{Females or Males.}
#'     \item{period}{Period range in years [start, end].}
#'     \item{x}{Start of age interval in years.}
#'     \item{nmx}{Mortality rate in age interval [x, x+nx).}
#'     \item{nqx}{Probability to die within age interval [x, x+nx) given survival to x.}
#'     \item{nax}{Subject-time spent in age interval [x, x+nx) when dying in that interval.}
#'     \item{lx}{Life table survivors at age x.}
#'     \item{ndx}{Deaths by life table population in age interval [x, x+nx).}
#'     \item{nLx}{Total person-years lived by life table population in age interval [x, x+nx).}
#'     \item{Tx}{Total person-years yet to live past age x by life table population.}
#'     \item{ex}{Life expectancy at age x.}
#'   }
#'
#' @source
#'   The Human Mortality Database \url{http://www.mortality.org/}
"sweden5x5"

#' Period Life-table Survival Function for Austrian Males 1992
#'
#' An abridged period life-table lx column for Austrian males in 1992 used in
#' an example on life table construction in Preston's demography textbook.
#'
#' @format
#'   A data frame with 19 rows and 2 variables:
#'   \describe{
#'     \item{x}{Start of age interval in years.}
#'     \item{lx}{Life table survivors at age x.}
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
