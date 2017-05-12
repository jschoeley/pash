##########################################################
# Simulate survival times for Australian males 1970-1972 #
# Derive single year and abridged life-tables            #
##########################################################

# The simulated individual level data serves as a gold-standard to test pash
# functionality against. Pace & shape measures as well as other mortality
# characteristics are perfectly known from the individual level data -- a
# survival-time simulation of a stationary population with the age-specific
# mortality of Australian males 1970--72. It is therefore possible to quantify
# the aggregation error introduced by the aggregate level pace & shape
# calculations implemented in pash.

# Init --------------------------------------------------------------------

library(dplyr)

# Simulate survival times following Heligman-Pollard law ------------------

# Heligman-Pollard mortality model (the version with qx on the left hand side
# instead of odds)
HP <- function (x, A, B, C, D, E, f, G, H, K) {
  qx = A^((x+B)^C) + D*exp(-E*(log(x)-log(f))^2) + (G*H^x)/(1+K*G*H^x)
  return(qx)
}

# parameters which predict single year qx for a modern human population
# Australian male mortality 1970-1972, Heligman and Pollard (1980), Table 2
A = 0.0016; B = 0.00112; C = 0.1112
D = 0.00163; E = 16.71; f = 20.03
G = 0.0000502; H = 1.1074; K = 2.41

curve(HP(x, A, B, C, D, E, f, G, H, K),
      from = 0, to = 100, log = "y", xlab = "Years of age", ylab = "1qx")

# simulation parameters
radix = lx = 1e4 # cohort size at birth
w = 1/(365*24)   # resolution of simulation time (hours)
i = 0            # starting age
set.seed(1987)   # a random seed for reproducibility


# simulate cohort ages at death following HP qx pattern
# end simulation when everyone is dead
age_at_death = NULL; while (lx > 0) {
  # get mean qx for a given age
  qx = HP(i, A, B, C, D, E, f, G, H, K)
  # transform single year qx into single hour qx
  qx_ = 1-(1-qx)^w
  # determine if subject dies during [x, x+w)
  survival_indicator = rbinom(n = lx, size = 1, prob = qx_)
  # number of deaths during [x, x+w)
  dx = sum(survival_indicator)
  cat(paste(format(round(i, 3), nsmall = 3), lx, dx, sep = "\t"), sep = "\n")
  # add ages at death of those who died to existing records
  age_at_death = c(age_at_death, rep(i, dx))
  # update number of survivors to x+w
  lx = lx-dx
  # update current age
  i=i+w
}
australia_surv_times <- age_at_death
save(australia_surv_times, file = "./data/australia_surv_times.rda")
hist(australia_surv_times, breaks = 0:120)

# Build a single year LT --------------------------------------------------

# starting with individual level ages at death
data_frame(age_at_death = australia_surv_times) %>%
  mutate(
    # add age at death in completed years
    x = floor(age_at_death),
    # discretize in single year age groups with 100+ open age
    x_cat = cut(x, breaks = c(0:100, Inf), right = FALSE)
  ) %>%
  # derive dx by year and corresponding ax
  group_by(x_cat) %>%
  summarise(x = min(x), ndx = n(), nax = mean(age_at_death)-x) %>%
  ungroup() %>%
  mutate(
    nx = c(diff(x), NA),
    lx = rev(cumsum(rev(ndx))),
    nqx = ndx/lx,
    nLx = nx*(c(lx[-1], 0)) + nax*ndx,
    nLx = ifelse(is.na(nLx), rev(lx)[1]*rev(nax)[1], nLx),
    nmx = ndx/nLx,
    Tx = rev(cumsum(rev(nLx))),
    ex = Tx/lx
  ) %>%
  select(x, nx, nmx, nqx, nax, lx, ndx, nLx, Tx, ex) -> australia_1y

australia_1y

save(australia_1y, file = "./data/australia_1y.rda")

# Build a 10 year life-table ----------------------------------------------

# starting with individual level ages at death
data_frame(age_at_death = australia_surv_times) %>%
  mutate(
    # discretize in 10 year age groups with 100+ open age and smaller spacing
    # for infancy and childhood
    x_cat = cut(age_at_death, breaks = c(0, 1, 5, seq(10, 100, 10), Inf), right = FALSE)
  ) %>%
  # derive dx by year and corresponding ax
  group_by(x_cat) %>%
  summarise(x = floor(min(age_at_death)), ndx = n(), nax = mean(age_at_death)-x) %>%
  ungroup() %>%
  mutate(
    nx = c(diff(x), NA),
    lx = rev(cumsum(rev(ndx))),
    nqx = ndx/lx,
    nLx = nx*(c(lx[-1], 0)) + nax*ndx,
    nLx = ifelse(is.na(nLx), rev(lx)[1]*rev(nax)[1], nLx),
    nmx = ndx/nLx,
    Tx = rev(cumsum(rev(nLx))),
    ex = Tx/lx
  ) %>%
  select(x, nx, nmx, nqx, nax, lx, ndx, nLx, Tx, ex) -> australia_10y

australia_10y

save(australia_10y, file = "./data/australia_10y.rda")
