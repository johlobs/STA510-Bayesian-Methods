# STA510 study app, Module 6: RStudio tasks with brms
# Prior predictive checks and prior sensitivity.
# All data are simulated here; nothing comes from the course notes.

library(brms)
library(posterior)
library(ggplot2)

options(mc.cores = 4)


## Task 6.1  Prior predictive check for a logistic regression ------------------
# Same simulated vaccination data as Task 4.2.

set.seed(521)
n_people <- 800
vaccine <- data.frame(age = round(runif(n_people, min = 20, max = 85)))
true_logodds <- -1.2 + 0.0008 * (vaccine$age - 20)^2
vaccine$vaccinated <- rbinom(n_people, size = 1, prob = plogis(true_logodds))
vaccine$age10 <- (vaccine$age - 50) / 10

# sample_prior = "only" ignores the data: the "posterior" is the prior
prior_wide <- brm(
  vaccinated ~ age10,
  family = bernoulli(),
  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 10), class = "b")),
  data = vaccine, sample_prior = "only",
  chains = 4, iter = 1000, seed = 610, refresh = 0
)

prior_weak <- brm(
  vaccinated ~ age10,
  family = bernoulli(),
  prior = c(prior(normal(0, 1.5), class = "Intercept"),
            prior(normal(0, 1), class = "b")),
  data = vaccine, sample_prior = "only",
  chains = 4, iter = 1000, seed = 610, refresh = 0
)

# What share vaccinated does each prior expect, before seeing any data?
pp_check(prior_wide, type = "stat", stat = "mean")
pp_check(prior_weak, type = "stat", stat = "mean")

prior_share_wide <- rowMeans(posterior_predict(prior_wide))
prior_share_weak <- rowMeans(posterior_predict(prior_weak))
c(wide_extreme = mean(prior_share_wide < 0.05 | prior_share_wide > 0.95),
  weak_extreme = mean(prior_share_weak < 0.05 | prior_share_weak > 0.95))

# Risk at age 20 and age 80 implied by each prior
extreme_ages <- data.frame(age10 = c(-3, 3))
risk_wide <- posterior_epred(prior_wide, newdata = extreme_ages)
risk_weak <- posterior_epred(prior_weak, newdata = extreme_ages)
c(wide_share_below_1pct_at_20 = mean(risk_wide[, 1] < 0.01),
  weak_share_below_1pct_at_20 = mean(risk_weak[, 1] < 0.01))


## Task 6.2  Prior sensitivity: does the conclusion change? --------------------

fit_weak <- update(prior_weak, sample_prior = "no", seed = 611, refresh = 0)
fit_wide <- update(prior_wide, sample_prior = "no", seed = 611, refresh = 0)

rbind(
  weak = posterior_summary(fit_weak, variable = "b_age10"),
  wide = posterior_summary(fit_wide, variable = "b_age10")
)
