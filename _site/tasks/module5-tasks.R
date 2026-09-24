# STA510 study app, Module 5: RStudio tasks with brms
# Pooling, shrinkage, ranks, new groups and varying slopes.
# All data are simulated here; nothing comes from the course notes.

library(brms)
library(posterior)
library(ggplot2)

options(mc.cores = 4)


## Task 5.1  Three ways to pool: 15 primary care clinics -----------------------
# Outcome: number of patients with diabetes whose HbA1c is at target.

set.seed(530)
clinics <- data.frame(
  clinic     = LETTERS[1:15],
  n_patients = c(8, 12, 15, 20, 25, 30, 40, 50, 60, 80, 100, 120, 150, 200, 250)
)
true_logodds <- rnorm(15, mean = qlogis(0.55), sd = 0.5)
clinics$at_target <- rbinom(15, size = clinics$n_patients, prob = plogis(true_logodds))
clinics$observed_share <- clinics$at_target / clinics$n_patients
clinics

fit_complete <- brm(
  at_target | trials(n_patients) ~ 1,
  family = binomial(),
  prior = prior(normal(0, 1.5), class = "Intercept"),
  data = clinics, chains = 4, iter = 2000, seed = 530, refresh = 0
)

fit_none <- brm(
  at_target | trials(n_patients) ~ 0 + clinic,
  family = binomial(),
  prior = prior(normal(0, 1.5), class = "b"),
  data = clinics, chains = 4, iter = 2000, seed = 530, refresh = 0
)

fit_partial <- brm(
  at_target | trials(n_patients) ~ 1 + (1 | clinic),
  family = binomial(),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(exponential(1), class = "sd")          # between-clinic SD on the log-odds scale
  ),
  data = clinics, chains = 4, iter = 2000, seed = 530, refresh = 0
)

summary(fit_partial)   # b_Intercept = average clinic, sd(Intercept) = spread between clinics

# Posterior mean share at target per clinic, under each model
share_none    <- colMeans(posterior_linpred(fit_none, transform = TRUE))
share_partial <- colMeans(posterior_linpred(fit_partial, transform = TRUE))
share_complete <- mean(plogis(as_draws_df(fit_complete)$b_Intercept))

comparison <- data.frame(
  clinic = clinics$clinic,
  n_patients = clinics$n_patients,
  observed = round(clinics$observed_share, 3),
  no_pooling = round(share_none, 3),
  partial_pooling = round(share_partial, 3),
  complete_pooling = round(share_complete, 3)
)
comparison$shift_towards_mean <- round(comparison$no_pooling - comparison$partial_pooling, 3)
comparison

# Plot: how far each clinic moves, against its size
ggplot(comparison, aes(x = n_patients, y = abs(shift_towards_mean))) +
  geom_point() +
  scale_x_log10() +
  labs(x = "patients in clinic (log scale)",
       y = "|no pooling - partial pooling|")


## Task 5.2  Ranks with uncertainty --------------------------------------------

share_draws <- posterior_linpred(fit_partial, transform = TRUE)   # draws x 15
rank_draws <- t(apply(-share_draws, 1, rank))                     # rank 1 = best
colnames(rank_draws) <- clinics$clinic
rank_summary <- data.frame(
  clinic    = clinics$clinic,
  mean_rank = round(colMeans(rank_draws), 1),
  lower_95  = apply(rank_draws, 2, quantile, probs = 0.025),
  upper_95  = apply(rank_draws, 2, quantile, probs = 0.975),
  prob_top3 = round(colMeans(rank_draws <= 3), 2)
)
rank_summary[order(rank_summary$mean_rank), ]


## Task 5.3  A clinic you have not seen yet ------------------------------------

new_clinic <- data.frame(clinic = "new", n_patients = 100)

# Expected number at target (out of 100) in a new clinic
new_draws <- posterior_epred(fit_partial, newdata = new_clinic,
                             allow_new_levels = TRUE,
                             sample_new_levels = "gaussian")
quantile(new_draws, probs = c(0.05, 0.5, 0.95))

# Compare: an existing, large clinic (O, 250 patients) scaled to 100 patients
existing <- data.frame(clinic = "O", n_patients = 100)
quantile(posterior_epred(fit_partial, newdata = existing), probs = c(0.05, 0.5, 0.95))

# Wrong way: plug in posterior means only (ignores uncertainty in mu and tau)
draws_partial <- as_draws_df(fit_partial)
plugin_share <- plogis(mean(draws_partial$b_Intercept) +
                       rnorm(4000, 0, mean(draws_partial$sd_clinic__Intercept)))
quantile(100 * plugin_share, probs = c(0.05, 0.5, 0.95))


## Task 5.4  Varying intercepts and slopes: blood pressure over 5 visits -------

set.seed(531)
n_patients <- 40
visit_months <- 0:4
sd_intercept <- 10      # patients differ in baseline SBP
sd_slope     <- 1.5     # patients differ in change per month
cor_intercept_slope <- -0.4
cov_matrix <- matrix(c(sd_intercept^2,
                       cor_intercept_slope * sd_intercept * sd_slope,
                       cor_intercept_slope * sd_intercept * sd_slope,
                       sd_slope^2),
                     nrow = 2)
patient_effects <- MASS::mvrnorm(n_patients, mu = c(0, 0), Sigma = cov_matrix)

bp <- expand.grid(month = visit_months, patient = 1:n_patients)
bp$sbp <- 150 + patient_effects[bp$patient, 1] +
          (-2 + patient_effects[bp$patient, 2]) * bp$month +
          rnorm(nrow(bp), mean = 0, sd = 5)

fit_slopes <- brm(
  sbp ~ month + (1 + month | patient),
  family = gaussian(),
  prior = c(
    prior(normal(150, 20), class = "Intercept"),
    prior(normal(0, 5), class = "b"),
    prior(exponential(0.2), class = "sd"),
    prior(exponential(0.2), class = "sigma"),
    prior(lkj(2), class = "cor")
  ),
  data = bp, chains = 4, iter = 2000, seed = 531, refresh = 0
)

summary(fit_slopes)   # sd(Intercept), sd(month), cor(Intercept,month)

# Share of patients whose own slope is negative (improving), per draw
draws_slopes <- as_draws_df(fit_slopes)
patient_slope_columns <- grep("^r_patient\\[.*,month\\]$", names(draws_slopes), value = TRUE)
patient_slopes <- draws_slopes$b_month + as.matrix(draws_slopes[, patient_slope_columns])
share_improving <- rowMeans(patient_slopes < 0)
quantile(share_improving, probs = c(0.05, 0.5, 0.95))


## Task 5.5  How much does the hyperprior matter with few groups? --------------

few_clinics <- clinics[clinics$clinic %in% c("A", "C", "E", "G", "I", "K"), ]

fit_tau_exp1 <- brm(
  at_target | trials(n_patients) ~ 1 + (1 | clinic),
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = "Intercept"),
            prior(exponential(1), class = "sd")),
  data = few_clinics, chains = 4, iter = 2000, seed = 532, refresh = 0,
  control = list(adapt_delta = 0.95)
)

fit_tau_exp01 <- brm(
  at_target | trials(n_patients) ~ 1 + (1 | clinic),
  family = binomial(),
  prior = c(prior(normal(0, 1.5), class = "Intercept"),
            prior(exponential(0.1), class = "sd")),     # prior mean SD = 10 on log-odds!
  data = few_clinics, chains = 4, iter = 2000, seed = 532, refresh = 0,
  control = list(adapt_delta = 0.95)
)

rbind(
  exponential_1   = posterior_summary(fit_tau_exp1,  variable = "sd_clinic__Intercept"),
  exponential_0.1 = posterior_summary(fit_tau_exp01, variable = "sd_clinic__Intercept")
)
