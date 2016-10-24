pash - Pace and Shape Analysis in R
-----------------------------------

`pash` lets you perform pace-shape analysis on life tables. Calculate a wide array of pace and shape metrics and use them to standardize and compare your data.

Install with `r devtools::install_github("jschoeley/pash")`.

- Step 1: Ceate a pace-shape object from a life table survivorship function using `Inputlx()`.
- Step 2: Using the pace-shape object as argument, calculate pace measures with `GetPace()`, calculate shape measures with `GetShape()`, standardize a life table by pace and shape with `StandardizeLT()`, rebase your life table to a different age with `RebaseLT()`.