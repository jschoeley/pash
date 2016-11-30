library(pash)

# Test ValidateLastOpen() -------------------------------------------------

test_that("errors in last_open input are catched", {
  expect_error(ValidateLastOpen(NA), "last_open must not contain NA.")
  expect_error(ValidateLastOpen(NaN), "last_open must not contain NaN.")
  expect_error(ValidateLastOpen(Inf), "last_open must not contain \\(-\\)Inf.")
  expect_error(ValidateLastOpen(-Inf), "last_open must not contain \\(-\\)Inf.")
  expect_error(ValidateLastOpen("TRUE"), "last_open must be logical.")
  expect_error(ValidateLastOpen(c(TRUE, FALSE)), "last_open must be scalar.")
})

# Test ValidateAge() ------------------------------------------------------

test_that("errors in age input are catched", {
  expect_error(ValidateAge(c(0:10, NA)), "The age vector must not contain NA.")
  expect_error(ValidateAge(c(0:10, NaN)), "The age vector must not contain NaN.")
  expect_error(ValidateAge(c(0:10, Inf)), "The age vector must not contain \\(-\\)Inf.")
  expect_error(ValidateAge(c(0:10, -Inf)), "The age vector must not contain \\(-\\)Inf.")
  expect_error(ValidateAge(as.character(0:10)), "Age must be numeric.")
  expect_error(ValidateAge(0), "The age vector must contain at least 3 age groups.")
  expect_error(ValidateAge(-5:5), "Age must not be negative.")
  expect_error(ValidateAge(c(0,0,1)), "The age vector must be arranged in increasing order.")
  expect_error(ValidateAge(c(0,2,1)), "The age vector must be arranged in increasing order.")
  expect_error(ValidateAge(10:1), "The age vector must be arranged in increasing order.")
})

# Test Validatenx() -------------------------------------------------------

test_that("errors in nx input are catched", {
  expect_error(Validatenx(c(rep(5, 11), NaN), seq(0, 55, 5), last_open = TRUE),
               "nx must not contain NaN.")
  expect_error(Validatenx(c(rep(5, 11), Inf), seq(0, 55, 5), last_open = TRUE),
               "nx must not contain \\(-\\)Inf.")
  expect_error(Validatenx(c(rep(5, 11), -Inf), seq(0, 55, 5), last_open = TRUE),
               "nx must not contain \\(-\\)Inf.")
  expect_error(Validatenx(as.factor(rep(5, 12)), seq(0, 55, 5), last_open = FALSE),
               "nx must be numeric or character.")
  expect_error(Validatenx(as.character(rep(5, 12)), seq(0, 55, 5), last_open = FALSE),
               "nx must be scalar if character.")
  expect_error(Validatenx(as.character("NA"), seq(0, 55, 5), last_open = FALSE),
               "nx must be 'auto' if character.")
  expect_equal(Validatenx("auto", seq(0, 55, 5), last_open = FALSE)[["nx_mode"]],
               "auto")
  expect_equal(Validatenx(c(rep(5, 11), NA), seq(0, 55, 5), last_open = TRUE)[["nx"]],
               c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, NA))
  expect_equal(Validatenx(rep(5, 12), seq(0, 55, 5), last_open = TRUE)[["nx"]],
               c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, NA))
  expect_equal(Validatenx(rep(5, 12), seq(0, 55, 5), last_open = FALSE)[["nx"]],
               c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(Validatenx(rep(5, 12), seq(0, 55, 5), last_open = FALSE)[["nx_mode"]],
               "vector")
  expect_error(Validatenx(rep(5, 11), seq(0, 55, 5), last_open = FALSE),
               "nx vector must be of same length as x.")
  expect_error(Validatenx(rep(5, 11), seq(0, 55, 5), last_open = TRUE),
               "nx vector must be of same length as x.")
  expect_error(Validatenx(as.numeric(NA), seq(0, 55, 5), last_open = TRUE),
               "nx must not be NA if scalar.")
  expect_equal(Validatenx(5, seq(0, 55, 5), last_open = FALSE)[["nx_mode"]],
               "scalar")
  expect_equal(Validatenx(5, seq(0, 55, 5), last_open = FALSE)[["nx"]],
               c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(Validatenx(5, seq(0, 55, 5), last_open = TRUE)[["nx"]],
               c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, NA))
  expect_error(Validatenx(6, seq(0, 55, 5), last_open = TRUE),
               "Provided Age and nx vectors don't match.")
  expect_error(Validatenx(rep(6, 12), seq(0, 55, 5), last_open = TRUE),
               "Provided Age and nx vectors don't match.")
})

# Test Validatenax() ------------------------------------------------------

test_that("errors in nax input are catched", {
  expect_error(Validatenax(nax = c(rep(0.5, 10), NA), x = 0:11, nx = 1, last_open = FALSE),
               "nax must not contain NA.")
  expect_error(Validatenax(nax = c(rep(0.5, 10), NaN), x = 0:11, nx = 1, last_open = FALSE),
               "nax must not contain NaN.")
  expect_error(Validatenax(nax = c(rep(0.5, 10), Inf), x = 0:11, nx = 1, last_open = FALSE),
               "nax must not contain \\(-\\)Inf.")
  expect_error(Validatenax(nax = c(rep(0.5, 10), -Inf), x = 0:11, nx = 1, last_open = FALSE),
               "nax must not contain \\(-\\)Inf.")
  expect_error(Validatenax(nax = as.factor(rep(0.5, 11)), x = 0:11, nx = 1, last_open = FALSE),
               "nax must be numeric or character.")
  expect_error(Validatenax(nax = as.character(rep(0.5, 11)), x = 0:11, nx = 1, last_open = FALSE),
               "nax must be scalar if character.")
  expect_error(Validatenax(nax = "auto", x = 0:11, nx = 1, last_open = FALSE),
               "nax mode must be either 'midpoint' or 'constant_nmx'.")
  expect_equal(Validatenax(nax = "midpoint", x = 0:11, nx = 1, last_open = FALSE)[["nax_mode"]],
               "midpoint")
  expect_equal(Validatenax(nax = "constant_nmx", x = 0:11, nx = 1, last_open = FALSE)[["nax_mode"]],
               "constant_nmx")
  expect_equal(Validatenax(nax = rep(0.5, 12), x = 0:11, nx = 1, last_open = FALSE)[["nax_mode"]],
               "vector")
  expect_error(Validatenax(nax = rep(0.5, 11), x = 0:11, nx = 1, last_open = FALSE),
               "nax vector must be of same length as x.")
  expect_equal(Validatenax(nax = 0.5, x = 0:11, nx = 1, last_open = FALSE)[["nax_mode"]],
               "scalar")
  expect_equal(Validatenax(nax = 0.5, x = 0:11, nx = 1, last_open = FALSE)[["nax"]],
               c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
  expect_error(Validatenax(nax = rep(1.1, 12), x = 0:11, nx = 1, last_open = FALSE),
               "nax must not be larger than nx.")
  expect_error(Validatenax(nax = 1.1, x = 0:11, nx = 1, last_open = FALSE),
               "nax must not be larger than nx.")
})
