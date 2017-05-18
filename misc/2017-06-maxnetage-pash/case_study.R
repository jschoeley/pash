library(pash); library(dplyr); library(ggplot2)

# Pace standardized age-specific mortality rates
sweden5x5 %>% group_by(period, sex) %>%
  do(Inputnmx(x=.$x, nmx=.$nmx) %>% StandardizeLT()) %>%
  qplot(x=x_s, y=nmx_s, color=period, geom="line", facets=~sex, data=., log="y") +
  guides(color = FALSE)
#ggsave("standardized.png", scale = 0.6, width = 13, height = 8)

# correlation among different shape measures
sweden5x5 %>% group_by(period, sex) %>%
  do(Inputnqx(x=.$x, nqx=.$nqx, last_open=TRUE) %>%
       GetShape(harmonized=FALSE) %>% t() %>% as.data.frame()) %>%
  pairs(x=.[3:9], data=., pch=16)

# the emergence of senescence
sweden5x5 %>% group_by(period, sex) %>%
  do(Inputnmx(x=.$x, nmx=.$nmx, nax = .$nax, last_open=TRUE) %>%
       GetShape(type="acfm", harmonized=FALSE) %>% as.data.frame()) %>%
  qplot(x=as.numeric(substr(period, 1,4)), y=., color=sex, data=.,
        ylab="ACFM", xlab="Period") + geom_hline(yintercept=0)
#ggsave("acfm.png", scale = 0.5, width = 13, height = 8)

chimp <- read.csv("./priv/2017-06-maxnetage-pash/chimpanzee_lt.csv")
chimp_lt <- data.frame(
  x = as.data.frame(Inputnqx(x = chimp$x, nqx = chimp$nqx))$x,
  x_s = StandardizeLT(Inputnqx(x = chimp$x, nqx = chimp$nqx))$x_s,
  lx = as.data.frame(Inputnqx(x = chimp$x, nqx = chimp$nqx))$lx,
  lx_s = StandardizeLT(Inputnqx(x = chimp$x, nqx = chimp$nqx))$lx_s
)
preston_lt <- data.frame(
  x = as.data.frame(Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, last_open = TRUE))$x,
  x_s = StandardizeLT(Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, last_open = TRUE))$x_s,
  lx = as.data.frame(Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, last_open = TRUE))$lx,
  lx_s = StandardizeLT(Inputlx(x = prestons_lx$x, lx = prestons_lx$lx, last_open = TRUE))$lx_s
)

ggplot(chimp_lt, aes(x = x, y = lx)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(data = preston_lt, se = FALSE, span = 0.5, color = "black", size = 1) +
  scale_y_continuous()


ggplot(chimp_lt, aes(x = x_s, y = lx_s)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(data = preston_lt, se = FALSE, span = 0.5, color = "black", size = 1) +
  scale_y_continuous()


lt = Inputlx(x = prestons_lx$x, lx = preston_lt$lx, last_open = TRUE)
summary(lt)
GetPace(lt)

StandardizeLT(lt)
