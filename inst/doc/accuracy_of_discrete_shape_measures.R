## ----set-options, echo=FALSE, cache=FALSE---------------------------------------------------------
options(width = 100)
knitr::opts_chunk$set(tidy = FALSE, size = "small")

## ----init-----------------------------------------------------------------------------------------
library(pash)
library(tidyverse)

radix = 10000

# create pash objects
lt_lvl1 <- Inputlx(x = australia_1y$x, lx = australia_1y$lx,
                   nax = australia_1y$nax, nx = australia_1y$nx, last_open = TRUE)
lt_lvl2 <- Inputlx(x = australia_10y$x, lx = australia_10y$lx,
                   nax = australia_10y$nax, nx = australia_10y$nx, last_open = TRUE)

## ----e0-------------------------------------------------------------------------------------------
# not a shape measure, just as a test, should be exactly equal
e0 <- c(
  lvl0 = mean(australia_surv_times),
  lvl1 = GetPace(lt_lvl1, type = "e0"),
  lvl2 = GetPace(lt_lvl1, type = "e0")
)
e0

## ----gini-----------------------------------------------------------------------------------------
X = data.frame(i = 1:length(australia_surv_times), x = sort(australia_surv_times))
G_lvl0 = sum((2*X$i-nrow(X)-1)*X$x)/(nrow(X)^2*mean(australia_surv_times))

gini <- c(
  lvl0 = G_lvl0,
  lvl1 = GetShape(lt_lvl1, type = "gini", harmonized = FALSE),
  lvl2 = GetShape(lt_lvl2, type = "gini", harmonized = FALSE)
)

gini

## ----cv-------------------------------------------------------------------------------------------
cv <- c(
  lvl0 = sd(australia_surv_times) / mean(australia_surv_times),
  lvl1 = GetShape(lt_lvl1, type = "cv", harmonized = FALSE),
  lvl2 = GetShape(lt_lvl2, type = "cv", harmonized = FALSE)
)

cv

## ----entropy--------------------------------------------------------------------------------------
eDaggerx = vector(mode = "double", length = length(australia_surv_times))
for (i in seq_along(australia_surv_times)) {
  if (i < length(australia_surv_times)) {
    eDaggerx[i] =
      mean(australia_surv_times[seq(i+1, length(australia_surv_times))]) -
      australia_surv_times[i]
  }
}

entropy <- c(
  lvl0 = sum(eDaggerx)/radix/mean(australia_surv_times),
  lvl1 = GetShape(lt_lvl1, type = "entropy", harmonized = FALSE),
  lvl2 = GetShape(lt_lvl2, type = "entropy", harmonized = FALSE)
)

entropy

## ----psmad----------------------------------------------------------------------------------------
psmad <- c(
  lvl0 = sum(australia_surv_times > mean(australia_surv_times)) / radix,
  lvl1 = GetShape(lt_lvl1, type = "psmad", harmonized = FALSE),
  lvl2 = GetShape(lt_lvl2, type = "psmad", harmonized = FALSE)
)

psmad

## ----ler------------------------------------------------------------------------------------------
# remaining life-expectancy at mean age of death (expressed as share of total life-expectancy)
ler <- c(
  lvl0 = (mean(australia_surv_times[australia_surv_times > mean(australia_surv_times)])-mean(australia_surv_times))/mean(australia_surv_times),
  lvl1 = GetShape(lt_lvl1, type = "ler", harmonized = FALSE),
  lvl2 = GetShape(lt_lvl2, type = "ler", harmonized = FALSE)
)

ler

## ----results--------------------------------------------------------------------------------------
data.frame(cv, gini, entropy, ler, psmad) %>%
  mutate(lvl = rownames(.)) %>%
  gather(key = measure, value = value, -lvl) %>%
  group_by(measure) %>%
  do({
    rbind(
      data.frame(lvl = 1, error = (.$value[2]-.$value[1])/.$value[1]),
      data.frame(lvl = 2, error = (.$value[3]-.$value[1])/.$value[1])
    )
  }) %>%
  ggplot() +
  geom_col(aes(x = interaction(lvl, measure), y = error, fill = measure)) +
  scale_x_discrete("", labels = c("CV-1Y.", "CV-10Y", "Entropy-1Y", "Entropy-10Y",
                                  "Gini-1Y", "Gini-10Y", "LER-1Y", "LER-10Y",
                                  "PSMAD-1Y", "PSMAD-10Y")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-0.05, 0.05),
                     breaks = seq(-0.05, 0.05, 0.01)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
  guides(fill = FALSE) +
  coord_flip()

