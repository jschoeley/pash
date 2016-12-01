#' Linear Interpolation
#'
#' Get exact x for a given y based on linear interpolation between two points.
#'
#' @keywords internal
LinearInterpolation <- function (x0, x1, y0, y1, y) {
  x = (-x1*y+x0*y-x0*y1+x1*y0)/(y0-y1)
  return(x)
}

#' Exponential Interpolation
#'
#' Get exact x for a given y based on exponential interpolation between two points.
#'
#' @keywords internal
ExponentialInterpolation <- function (x0, x1, y0, y1, y) {
  x = ((x0-x1)*log(y) - x0*log(y1) + x1*log(y0)) / (log(y0)-log(y1))
  return(x)
}

#' Linear Extrapolation
#'
#' (Log)-linearly extrapolate a single (y,x) tuple.
#'
#' @param x A vector of x values.
#' @param y A vector of y values.
#' @param xextra The x for which y should be extrapolated.
#' @param loga Should a log-linear model be fitted?
#'
#' @examples
#' \dontrun{
#' LinearExtrapolation(x = 1:2, y = c(1, 2), xextra = 3, loga = FALSE)
#' LinearExtrapolation(x = 1:2, y = c(1, 2), xextra = 3, loga = TRUE)
#' }
#'
#' @keywords internal
LinearExtrapolation <- function (x, y, xextra, loga) {
  if (identical(loga, FALSE)) {
    fit = stats::lm(y~x)
    pre = stats::predict.lm(fit, newdata = data.frame(x = xextra))
  }
  if (identical(loga, TRUE)) {
    fit = stats::lm(log(y)~x)
    pre = exp(stats::predict.lm(fit, newdata = data.frame(x = xextra)))
  }
  return(pre)
}
