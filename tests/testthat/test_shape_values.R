######################################
# Test values returned by GetShape() #
######################################

library(pash)

# Test Gini ---------------------------------------------------------------

exact_gini     = 0.1232619
expect_gini1y  = 0.1232084
expect_gini10y = 0.1182637

test_that("GetShape() returns expected Gini index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    expect_gini1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    expect_gini10y, tolerance = 1E-7)
})

test_that("GetShape() returns approximately correct Gini index given exact nax are provided", {
  # correct within +-0.0001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-4)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct Gini index given udd assumption", {
  # correct within +-0.0001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "udd", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-4)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "udd", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct Gini index given cfm assumption", {
  # correct within +-0.0001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "cfm", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-4)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "cfm", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "gini", harmonized = FALSE)[[1]],
    exact_gini, tolerance = 1E-2)
})

# Test Entropy ------------------------------------------------------------

exact_entropy     = 0.1768201
expect_entropy1y  = 0.1768274
expect_entropy10y = 0.1787701

test_that("GetShape() returns expected entropy index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    expect_entropy1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    expect_entropy10y, tolerance = 1E-7)
})

test_that("GetShape() returns approximately correct entropy index given exact nax are provided", {
  # correct within +-0.0001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-4)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct entropy index given udd assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "udd", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-3)
  # correct within +-0.1
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "udd", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-1)
})

test_that("GetShape() returns approximately correct entropy index given cfm assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "cfm", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-3)
  # correct within +-0.1
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "cfm", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "entropy", harmonized = FALSE)[[1]],
    exact_entropy, tolerance = 1E-1)
})

# Test CV -----------------------------------------------------------------

exact_cv     = 0.2349002
expect_cv1y  = 0.2348493
expect_cv10y = 0.2313477

test_that("GetShape() returns expected cv index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    expect_cv1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    expect_cv10y, tolerance = 1E-7)
})

test_that("GetShape() returns approximately correct cv index given exact nax are provided", {
  # correct within +-0.0001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-4)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct cv index given udd assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "udd", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-4)
  # correct within +-0.1
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "udd", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct cv index given cfm assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "cfm", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-4)
  # correct within +-0.1
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "cfm", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "cv", harmonized = FALSE)[[1]],
    exact_cv, tolerance = 1E-2)
})

# Test LER ----------------------------------------------------------------

exact_ler     = 0.1480046
expect_ler1y  = 0.1478210
expect_ler10y = 0.1496099

test_that("GetShape() returns expected ler index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    expect_ler1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    expect_ler10y, tolerance = 1E-7)
})

test_that("GetShape() returns approximately correct ler index given exact nax are provided", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-3)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct ler index given udd assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "udd", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-3)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "udd", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct ler index given cfm assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "cfm", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-3)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "cfm", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "ler", harmonized = FALSE)[[1]],
    exact_ler, tolerance = 1E-2)
})

# Test MR -----------------------------------------------------------------

expect_mr1y  = 0.0731018
expect_mr10y = 0.0502108

test_that("GetShape() returns expected mr index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "mr", harmonized = FALSE)[[1]],
    expect_mr1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "mr", harmonized = FALSE)[[1]],
    expect_mr10y, tolerance = 1E-7)
})

# Test PSMAD --------------------------------------------------------------

exact_psmad     = 0.5833000
expect_psmad1y  = 0.5840483
expect_psmad10y = 0.5758037

test_that("GetShape() returns expected psmad index", {
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    expect_psmad1y, tolerance = 1E-7)
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    expect_psmad10y, tolerance = 1E-7)
})

test_that("GetShape() returns approximately correct psmad index given exact nax are provided", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = australia_1y$nax, lx = australia_1y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-3)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = australia_10y$nax, lx = australia_10y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct psmad index given udd assumption", {
  # correct within +-0.001
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "udd", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-3)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "udd", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-2)
})

test_that("GetShape() returns approximately correct psmad index given cfm assumption", {
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_1y$x, nx = australia_1y$nx,
                     nax = "cfm", lx = australia_1y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-2)
  # correct within +-0.01
  expect_equal(
    GetShape(Inputlx(x = australia_10y$x, nx = australia_10y$nx,
                     nax = "cfm", lx = australia_10y$lx,
                     last_open = TRUE),
             type = "psmad", harmonized = FALSE)[[1]],
    exact_psmad, tolerance = 1E-2)
})
