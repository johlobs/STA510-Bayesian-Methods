# STA510 project starter: a Bayesian GLMM on European Social Survey data
#
# Example question: does self-rated good health differ between Swedish
# regions and between survey rounds, after accounting for age, sex and
# education? Replace the question, outcome and predictors with your own.
#
# Data: the Swedish ESS file with all rounds (download from
# https://ess.sikt.no after registering). The ESS conditions of use do not
# allow redistributing the data, so keep the file out of public repositories.

library(haven)
library(brms)
library(posterior)
library(ggplot2)

options(mc.cores = 4)

ess_file <- "PATH/TO/all_years_sweden.sav"     # <- change to your own file


## 1. Read and prepare the data ------------------------------------------------

ess_raw <- read_sav(ess_file)   # ESS missing codes (7, 8, 9, 77, ...) become NA

ess <- data.frame(
  essround  = ess_raw$essround,
  region    = as.character(ess_raw$region),
  health    = as.numeric(ess_raw$health),    # 1 = very good ... 5 = very bad
  age       = as.numeric(ess_raw$agea),
  gender    = as.numeric(ess_raw$gndr),      # 1 = male, 2 = female
  education = as.numeric(ess_raw$eisced)     # ES-ISCED 1 to 7, 55 = other
)

# Region is only recorded from round 5. Rounds 5 to 8 use NUTS3 codes
# (e.g. SE110), rounds 9 and later use NUTS2 (e.g. SE11). The first four
# characters give NUTS2 in every round: 8 regions.
ess <- ess[ess$essround >= 5, ]
ess$region2 <- substr(ess$region, 1, 4)

ess$good_health <- as.integer(ess$health <= 2)          # very good or good
ess$age10       <- (ess$age - 50) / 10                  # decades from age 50
ess$female      <- as.integer(ess$gender == 2)
ess$education[ess$education == 55] <- NA
ess$edu_level <- cut(ess$education, breaks = c(0, 2, 4, 7),
                     labels = c("low", "middle", "high"))
ess$round <- factor(ess$essround)

analysis_vars <- c("good_health", "age10", "female", "edu_level", "region2", "round")
ess <- ess[complete.cases(ess[, analysis_vars]), analysis_vars]

nrow(ess)
table(ess$region2)
table(ess$round)


## 2. Model and priors, written out --------------------------------------------
# good_health_i ~ Bernoulli(p_i)
# logit(p_i) = b0 + b_age * age10 + b_age2 * age10^2 + b_female * female
#              + b_edu[edu_level] + u_region[j] + v_round[k]
# u_region ~ Normal(0, sd_region),  v_round ~ Normal(0, sd_round)

model_formula <- bf(good_health ~ age10 + I(age10^2) + female + edu_level +
                      (1 | region2) + (1 | round))

get_prior(model_formula, data = ess, family = bernoulli())   # what needs a prior?

model_priors <- c(
  prior(normal(1, 1.5), class = "Intercept"),   # most people report good health
  prior(normal(0, 1), class = "b"),             # log-odds ratios mostly within +/- 2
  prior(exponential(2), class = "sd")           # region/round SDs: small, mean 0.5
)


## 3. Prior predictive check ---------------------------------------------------

fit_prior <- brm(model_formula, data = ess, family = bernoulli(),
                 prior = model_priors, sample_prior = "only",
                 chains = 4, iter = 1000, seed = 510, refresh = 0)
pp_check(fit_prior, type = "stat", stat = "mean")          # plausible shares?


## 4. Fit and check convergence ------------------------------------------------

fit <- brm(model_formula, data = ess, family = bernoulli(),
           prior = model_priors,
           chains = 4, iter = 2000, seed = 510, refresh = 100,
           control = list(adapt_delta = 0.95),   # few groups: avoids divergences
           file = "fit_health_glmm")      # saved to disk: rerunning is instant
                                          # (delete the .rds file after changing the model!)

summary(fit)                                 # Rhat, Bulk_ESS, Tail_ESS, divergences
plot(fit, variable = c("b_Intercept", "sd_region2__Intercept", "sd_round__Intercept"))


## 5. Posterior predictive checks ----------------------------------------------

pp_check(fit, type = "stat_grouped", stat = "mean", group = "region2")
pp_check(fit, type = "stat_grouped", stat = "mean", group = "round")
pp_check(fit, type = "stat_grouped", stat = "mean", group = "edu_level")


## 6. Compare with a simpler model ---------------------------------------------

fit_no_region <- update(fit, formula. = ~ . - (1 | region2),
                        control = list(adapt_delta = 0.95),
                        seed = 511, refresh = 0)
loo_compare(loo(fit), loo(fit_no_region))


## 7. The estimand, from the draws ---------------------------------------------
# Example: probability of good health for a 50-year-old woman with middle
# education, in each region, averaged over rounds (round effect set to 0).

regions <- sort(unique(ess$region2))
new_people <- data.frame(age10 = 0, female = 1, edu_level = "middle",
                         region2 = regions, round = NA)
prob_draws <- posterior_epred(fit, newdata = new_people,
                              re_formula = ~ (1 | region2))
region_summary <- data.frame(
  region = regions,
  mean   = colMeans(prob_draws),
  lower  = apply(prob_draws, 2, quantile, probs = 0.025),
  upper  = apply(prob_draws, 2, quantile, probs = 0.975)
)
region_summary

ggplot(region_summary, aes(x = region, y = mean, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  labs(y = "P(good health), 50-year-old woman, middle education",
       x = "NUTS2 region")
