# STA510 study app, Module 4: RStudio tasks with brms
# Posterior predictive checks, LOO, model comparison and stacking.
# All data are simulated here; nothing comes from the course notes.

library(brms)
library(posterior)
library(ggplot2)

options(mc.cores = 4)


## Task 4.1  Normal or Student-t? Sodium values with a few bad readings --------

set.seed(520)
n_samples <- 300
bad_reading <- rbinom(n_samples, size = 1, prob = 0.05)       # 5% faulty
sodium <- data.frame(
  value = ifelse(bad_reading == 1,
                 rnorm(n_samples, mean = 140, sd = 15),       # faulty: very noisy
                 rnorm(n_samples, mean = 140, sd = 3))        # correct readings
)

fit_normal <- brm(
  value ~ 1,
  family = gaussian(),
  prior = c(
    prior(normal(140, 10), class = "Intercept"),
    prior(exponential(0.2), class = "sigma")
  ),
  data = sodium, chains = 4, iter = 2000, seed = 520, refresh = 0
)

fit_student <- brm(
  value ~ 1,
  family = student(),
  prior = c(
    prior(normal(140, 10), class = "Intercept"),
    prior(exponential(0.2), class = "sigma"),
    prior(gamma(2, 0.1), class = "nu")          # brms default for nu
  ),
  data = sodium, chains = 4, iter = 2000, seed = 520, refresh = 0
)

# 1. Both models converge. Convergence says nothing about fit.
summary(fit_normal)
summary(fit_student)

# 2. Graphical check: observed density vs 50 replicated datasets
pp_check(fit_normal, ndraws = 50) + xlim(110, 170)
pp_check(fit_student, ndraws = 50) + xlim(110, 170)

# 3. Two test statistics: how peaked is the centre, how heavy are the tails?
share_near    <- function(y) mean(abs(y - 140) < 3)     # within 3 mmol/L of 140
share_extreme <- function(y) mean(abs(y - 140) > 15)    # more than 15 away
share_near(sodium$value)
share_extreme(sodium$value)
pp_check(fit_normal, type = "stat", stat = "share_near")
pp_check(fit_student, type = "stat", stat = "share_near")
pp_check(fit_normal, type = "stat", stat = "share_extreme")
pp_check(fit_student, type = "stat", stat = "share_extreme")

# The posterior predictive p-values, by hand: P(T(y_rep) >= T(y))
yrep_normal  <- posterior_predict(fit_normal)
yrep_student <- posterior_predict(fit_student)
c(normal_near     = mean(apply(yrep_normal, 1, share_near) >= share_near(sodium$value)),
  student_near    = mean(apply(yrep_student, 1, share_near) >= share_near(sodium$value)),
  normal_extreme  = mean(apply(yrep_normal, 1, share_extreme) >= share_extreme(sodium$value)),
  student_extreme = mean(apply(yrep_student, 1, share_extreme) >= share_extreme(sodium$value)))

# 4. Out-of-sample comparison
loo_normal  <- loo(fit_normal)
loo_student <- loo(fit_student)
loo_normal                 # look at the Pareto k diagnostics
loo_compare(loo_normal, loo_student)


## Task 4.2  Binary outcome: flu vaccination by age ----------------------------

set.seed(521)
n_people <- 800
vaccine <- data.frame(age = round(runif(n_people, min = 20, max = 85)))
true_logodds <- -1.2 + 0.0008 * (vaccine$age - 20)^2            # uptake rises faster with age
vaccine$vaccinated <- rbinom(n_people, size = 1, prob = plogis(true_logodds))
vaccine$age10 <- (vaccine$age - 50) / 10                         # decades from age 50
vaccine$age_group <- cut(vaccine$age, breaks = c(19, 35, 50, 65, 75, 85))

fit_linear <- brm(
  vaccinated ~ age10,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  ),
  data = vaccine, chains = 4, iter = 2000, seed = 521, refresh = 0
)

fit_quadratic <- brm(
  vaccinated ~ age10 + I(age10^2),
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  ),
  data = vaccine, chains = 4, iter = 2000, seed = 521, refresh = 0
)

# 1. Overall share vaccinated: both models reproduce it
pp_check(fit_linear, type = "stat", stat = "mean")

# 2. Share vaccinated within each age group: where does the linear model fail?
# pp_check(..., group = ) only accepts variables that are in the model, and
# age_group is not. So call bayesplot directly with posterior_predict() draws.
yrep_linear    <- posterior_predict(fit_linear)
yrep_quadratic <- posterior_predict(fit_quadratic)
bayesplot::ppc_stat_grouped(y = vaccine$vaccinated, yrep = yrep_linear,
                            group = vaccine$age_group, stat = "mean")
bayesplot::ppc_stat_grouped(y = vaccine$vaccinated, yrep = yrep_quadratic,
                            group = vaccine$age_group, stat = "mean")

# The same check as numbers: observed share, replicated 5% to 95% range,
# and the posterior predictive p-value P(replicated share >= observed share)
for (g in levels(vaccine$age_group)) {
  in_group <- vaccine$age_group == g
  observed_share  <- mean(vaccine$vaccinated[in_group])
  share_linear    <- rowMeans(yrep_linear[, in_group])
  share_quadratic <- rowMeans(yrep_quadratic[, in_group])
  cat(g,
      " observed:", round(observed_share, 3),
      " linear 5%-95%:", round(quantile(share_linear, c(0.05, 0.95)), 3),
      " p_linear:", round(mean(share_linear >= observed_share), 3),
      " p_quadratic:", round(mean(share_quadratic >= observed_share), 3),
      "\n")
}

# 3. Compare with LOO
loo_linear    <- loo(fit_linear)
loo_quadratic <- loo(fit_quadratic)
loo_compare(loo_linear, loo_quadratic)


## Task 4.3  Stacking weights --------------------------------------------------

loo_model_weights(list(linear = loo_linear, quadratic = loo_quadratic),
                  method = "stacking")
loo_model_weights(list(linear = loo_linear, quadratic = loo_quadratic),
                  method = "pseudobma")


## Task 4.4  The estimand: risk difference between age 70 and age 40 -----------

ages_of_interest <- data.frame(age10 = c((40 - 50) / 10, (70 - 50) / 10))
risk_draws <- posterior_epred(fit_quadratic, newdata = ages_of_interest)  # draws x 2
risk_difference <- risk_draws[, 2] - risk_draws[, 1]
quantile(risk_difference, probs = c(0.025, 0.5, 0.975))
mean(risk_difference > 0.3)
