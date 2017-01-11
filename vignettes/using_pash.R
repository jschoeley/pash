## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("jschoeley/pash")

## ------------------------------------------------------------------------
library(pash)

## ------------------------------------------------------------------------
swe <- subset(sweden5x5, sex == "female" & period == "1940-1944")[c("x", "nmx")]
swe

## ------------------------------------------------------------------------
Inputnmx(x = swe$x, nmx = swe$nmx, last_open = TRUE)

## ------------------------------------------------------------------------
Inputnmx(x = swe$x, nmx = swe$nmx, last_open = TRUE, nax = "cfm")

## ------------------------------------------------------------------------
prestons_lx
Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)

## ------------------------------------------------------------------------
pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)

## ------------------------------------------------------------------------
GetPace(pash)

## ------------------------------------------------------------------------
GetPace(pash, type = "qlx", q = 0.2)

## ------------------------------------------------------------------------
GetShape(pash)

## ------------------------------------------------------------------------
GetShape(pash, type = "Entropy")

## ------------------------------------------------------------------------
pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
pash

## ------------------------------------------------------------------------
pash = RebaseLT(pash, origin = 15)
pash

## ------------------------------------------------------------------------
GetPace(pash)
GetShape(pash)

## ------------------------------------------------------------------------
RebaseLT(pash, origin = 0)

## ------------------------------------------------------------------------
pash = Inputlx(x = prestons_lx$x, lx = prestons_lx$lx)
StandardizeLT(pash)

## ------------------------------------------------------------------------
StandardizeLT(pash, pace = "qlx")

## ------------------------------------------------------------------------
StandardizeLT(pash, pace = "qlx", q = 0.7)

## ----message=FALSE-------------------------------------------------------
library(dplyr)
library(ggplot2)

sweden5x5 %>%
  group_by(sex, period) %>%
    do({
     pash = Inputnmx(x = .$x, nmx = .$nmx)
     data.frame(e0 = GetPace(pash, type = "e0"),
                H  = GetShape(pash, type = "Entropy"))
    }) %>%
  ggplot(aes(x = e0, y = H, color = sex)) + geom_point()

