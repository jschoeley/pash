% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_data.R
\docType{data}
\name{sweden5x5}
\alias{sweden5x5}
\title{Period Life-tables for Sweden 1755-2014 by Sex}
\format{A data frame with 2,496 rows and 12 variables:
  \describe{
    \item{sex}{Females or Males.}
    \item{period}{Period range in years [start, end].}
    \item{x}{Start of age interval in years.}
    \item{nmx}{Mortality rate in age interval [x, x+nx).}
    \item{nqx}{Probability to die within age interval [x, x+nx) given survival to x.}
    \item{nax}{Subject-time spent in age interval [x, x+nx) when dying in that interval.}
    \item{lx}{Life-table survivors at age x.}
    \item{ndx}{Deaths by life-table population in age interval [x, x+nx).}
    \item{nLx}{Total person-years lived by life-table population in age interval [x, x+nx).}
    \item{Tx}{Total person-years yet to live past age x by life-table population.}
    \item{ex}{Life expectancy at age x.}
  }}
\source{
The Human Mortality Database \url{http://www.mortality.org/}
}
\usage{
sweden5x5
}
\description{
A dataset containing abridged period life-tables for Sweden years 1755-2014
by sex in five year period intervals.
}
\details{
Infant and early childhood mortality are given in 1 and 4 year age
  intervals. Other age groups are given in 5 year intervals.

  The dataset has been altered in form from the original provided by the
  Human Mortality database.
}
\keyword{datasets}
