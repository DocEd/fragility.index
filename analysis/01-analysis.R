# Core Scenarios ====

# Investigating the fragility index across a range of scenarios. What happens
# with studies with a control mortality of 30% and intervention mortality of 25%,
# 20% or 15%. With each powered to 70%, 80% and 90%.

set.seed(2001)

library(fragility.index)
library(dplyr)
library(purrr)

scenarios <- as_tibble(expand.grid(c(0.25, 0.2, 0.15), c(0.7, 0.8, 0.9)))
names(scenarios) <- c("int_mort", "power")

dt <- map2_dfr(
  .x = scenarios$int_mort,
  .y = scenarios$power,
  .f = ~ simulate_fragility(.x, .y, control_mort = 0.3, permit_negative = TRUE)
)

# Simulations of single RCTs ====

## We also want to recreate what would happen should we see lots of RCTs
## each with their own effect size, power and sample size. These represent
## the "single draws" of a frequency statistic (i.e. p value) that we get to
## observe when performing an RCT

## Will simulate control mortality from a beta distribution, with a relative
## risk reduction of the intervention defined on a uniform 0.1, 0.5

df <- tibble(
  cm = rbeta(1000, 2, 15),
  rrr = runif(1000, 0.1, 0.5),
  arr = cm * rrr,
  im = cm - arr,
  pwr = sample(c(0.7, 0.8, 0.9), size = 1000, replace = TRUE)
)

dx <- pmap_dfr(
  .l = list(
    df$im,
    df$pwr,
    df$cm
  ),
  .f = ~ simulate_fragility(..., permit_negative = TRUE, sims = 1)
)

# Convert fixed parameters to factors (useful for plotting)
dt <- dt %>%
  mutate_at(
    vars(power, c_y, t_y, arr, rrr, n),
    factor
  )

dt <- rename(dt, `absolute risk reduction` = arr)

dx <- dx %>%
  mutate_at(
    vars(power),
    factor
  )

use_data(dt, dx, internal = TRUE, overwrite = TRUE)

# Predicting the FI ====

# I want to build the case that the FI is a transformation of the p value
# So we should be able to predict the FI accurately from information in
# the trial directly, without having to do the fishers exact test procedure

# Naive analysis ----
niave_p <- dx %>%
  filter(reverse_effect == FALSE) %>%
  lm(fragility ~ p_value, data = .)

niave_n <- dx %>%
  filter(reverse_effect == FALSE) %>%
  lm(fragility ~ n, data = .)

niave_np <- dx %>%
  filter(reverse_effect == FALSE) %>%
  lm(fragility ~ p_value + n, data = .)

# summary(niave_p)
# summary(niave_n)
# summary(niave_np)

# There is a poor relationship here. This is for 2 reasons:
# - the p value needs to be transformed
# - this is a multivariable problem (because multiple study designs map onto the
#   same p value) and so we need to include some of the study design elements

good_mod <- dx %>%
  filter(reverse_effect == FALSE) %>%
  filter(fragility > 0) %>%
  mutate(logp = log(p_value)) %>%
  lm(fragility ~ ate*logp*n*oc_y, data = .)

# good_mod %>%
#   broom::tidy() %>%
#   mutate(l95 = confint(good_mod)[,1],
#          u95 = confint(good_mod)[,2])

# summary(good_mod)
