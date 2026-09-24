# STA510 study app, Module 3: RStudio tasks with brms
# All data are simulated here; nothing comes from the course notes.
# Run the tasks one at a time. The first brm() call compiles a Stan
# program, which takes a minute or two.

library(brms)
library(posterior)
library(ggplot2)

options(mc.cores = 4)   # one chain per CPU core


## Task 3.1  Intercept-only model: resting heart rate of 12 runners ------------

set.seed(510)
runners <- data.frame(heart_rate = round(rnorm(12, mean = 58, sd = 7)))
mean(runners$heart_rate)   # the sample mean, for comparison

fit_hr <- brm(
  heart_rate ~ 1,
  family = gaussian(),
  prior = c(
    prior(normal(70, 15), class = "Intercept"),   # adults at rest: roughly 40 to 100
    prior(exponential(0.1), class = "sigma")      # prior mean SD = 10 beats/min
  ),
  data = runners,
  chains = 4, iter = 2000, seed = 510, refresh = 0
)

summary(fit_hr)           # 1. check Rhat and Bulk_ESS/Tail_ESS first
plot(fit_hr)              # 2. trace plots: four chains that overlap, no drift

draws_hr <- as_draws_df(fit_hr)
summarise_draws(draws_hr, "mean", "sd", "rhat", "ess_bulk", "mcse_mean")

# 3. Posterior probability that the true mean heart rate is below 60
mean(draws_hr$b_Intercept < 60)

# 4. 90% equal-tailed credible interval for the mean
quantile(draws_hr$b_Intercept, probs = c(0.05, 0.95))


## Task 3.2  A binary predictor: blood pressure change, drug vs placebo --------

set.seed(511)
n_patients <- 60
trial <- data.frame(treated = rep(c(0, 1), each = n_patients / 2))
trial$sbp_change <- rnorm(n_patients, mean = -2 - 6 * trial$treated, sd = 8)

fit_trial <- brm(
  sbp_change ~ treated,
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), class = "Intercept"),   # mean change on placebo
    prior(normal(0, 10), class = "b"),           # treatment effect in mmHg
    prior(exponential(0.1), class = "sigma")
  ),
  data = trial,
  chains = 4, iter = 2000, seed = 511, refresh = 0
)

summary(fit_trial)

draws_trial <- as_draws_df(fit_trial)

# 1. Standardised effect size, computed draw by draw
draws_trial$std_effect <- draws_trial$b_treated / draws_trial$sigma
quantile(draws_trial$std_effect, probs = c(0.025, 0.5, 0.975))

# 2. Probability that the drug lowers SBP by more than 5 mmHg
mean(draws_trial$b_treated < -5)

# 3. Mean outcome vs a new patient's outcome, for placebo and drug
new_patients <- data.frame(treated = c(0, 1))
mu_draws <- posterior_linpred(fit_trial, newdata = new_patients)   # draws x 2
y_draws  <- posterior_predict(fit_trial, newdata = new_patients)   # draws x 2
apply(mu_draws, 2, sd)   # uncertainty about the group mean
apply(y_draws, 2, sd)    # uncertainty about one new patient: much wider

# 4. The same two quantities for the drug group, by hand from the draws
mu_treated_by_hand <- draws_trial$b_Intercept + draws_trial$b_treated
y_treated_by_hand  <- rnorm(nrow(draws_trial),
                            mean = mu_treated_by_hand,
                            sd   = draws_trial$sigma)
c(sd_mean_by_hand = sd(mu_treated_by_hand),
  sd_new_patient_by_hand = sd(y_treated_by_hand))


## Task 3.3  A poorly identified model: weight recorded twice ------------------

set.seed(512)
n_people <- 50
bodyweight <- data.frame(weight_kg = rnorm(n_people, mean = 75, sd = 12))
bodyweight$weight_lb <- bodyweight$weight_kg * 2.2046   # the same information again
bodyweight$sbp <- rnorm(n_people, mean = 100 + 0.4 * bodyweight$weight_kg, sd = 8)

fit_both_vague <- brm(
  sbp ~ weight_kg + weight_lb,
  family = gaussian(),
  prior = c(
    prior(normal(120, 30), class = "Intercept"),
    prior(normal(0, 1000), class = "b"),          # very vague on both slopes
    prior(exponential(0.1), class = "sigma")
  ),
  data = bodyweight,
  chains = 4, iter = 1000, seed = 512, refresh = 0
)

summary(fit_both_vague)    # look at Rhat and ESS for the two slopes
plot(fit_both_vague, variable = c("b_weight_kg", "b_weight_lb"))

# The combination the data CAN identify: the total effect of 1 kg
draws_vague <- as_draws_df(fit_both_vague)
effect_per_kg <- draws_vague$b_weight_kg + 2.2046 * draws_vague$b_weight_lb
summarise_draws(data.frame(effect_per_kg), "mean", "sd", "rhat", "ess_bulk")

# The same model with a tighter prior on the slopes
fit_both_tight <- brm(
  sbp ~ weight_kg + weight_lb,
  family = gaussian(),
  prior = c(
    prior(normal(120, 30), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(0.1), class = "sigma")
  ),
  data = bodyweight,
  chains = 4, iter = 1000, seed = 512, refresh = 0
)
summary(fit_both_tight)
