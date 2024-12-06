

# Simulate LMER Data -------------------------------------------------------

set.seed(5247)

n1 <- 10
n2 <- 80
n <- n1 * n2

team_id <- rep(1:n2, each = n1)
b_context <- .50
b_within <- .30
b_between <- b_context + b_within 
z <- model.matrix(~ as.factor(team_id) - 1)
x <- 6 + rnorm(n, sd = 1) + z %*% rnorm(n2, sd = sqrt(.50))
x_rd <- round(as.numeric(x), 2)
x_lo <- round(rnorm(n), 2)
x_lead_lo <- rep(round(rnorm(n2, mean = 4, sd = 1), 2), each = n1)
ran_ef_int <- rnorm(n2, sd = 1)
ran_ef_slope <- .10*ran_ef_int + rnorm(n2, sd = sqrt(1 - var(.10*ran_ef_int)))

ran_ef_int <- sqrt(.10)*ran_ef_int
ran_ef_slope <- sqrt(.075)*ran_ef_slope

ran_ef <- c(ran_ef_int, ran_ef_slope)

z_rs <- x_lo * z

z <- cbind(z, z_rs)

data_ps <- 
  tibble::tibble(
    team_id,
    x_inc_lead = x_rd,
    x_learn_ort = x_lo,
    x_lead_learn_ort = x_lead_lo
  ) |>
  dplyr::group_by(
    team_id
  ) |>
  dplyr::mutate(
    x_inc_lead_gpm = mean(x_inc_lead)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    x_inc_lead_gpc = x_inc_lead - x_inc_lead_gpm,
    y_psych_safe = 3 + b_within*x_inc_lead + b_context*x_inc_lead_gpm + .20*x_learn_ort + .20*x_learn_ort*x_lead_learn_ort + 
      rnorm(n, sd = 1) + z%*%ran_ef,
    y_psych_safe = round(as.numeric(y_psych_safe), 2)
  ) 

data_ps <- 
  data_ps |>
  dplyr::group_by(
    team_id
  ) |>
  dplyr::mutate(
    x_learn_ort_gpm = mean(x_learn_ort),
    x_learn_ort_gpc = x_learn_ort - x_learn_ort_gpm
  ) |>
  dplyr::ungroup()


# Fit Psych Safety Models -------------------------------------------------

mod_null <- lmerTest::lmer(y_psych_safe ~ 1 + (1|team_id), data_ps)
mod_1 <- lmerTest::lmer(y_psych_safe ~ x_inc_lead + x_learn_ort_gpc + (x_inc_lead + x_learn_ort_gpc | team_id), data_ps)
mod_2a <- lmerTest::lmer(y_psych_safe ~ x_inc_lead + x_learn_ort_gpc + (x_learn_ort_gpc | team_id), data_ps)
mod_2b <- lmerTest::lmer(y_psych_safe ~ x_inc_lead + x_learn_ort_gpc + (x_inc_lead | team_id), data_ps)

anova(mod_1, mod_null)
anova(mod_1, mod_2a)
anova(mod_1, mod_2b)

mod_3 <- lmerTest::lmer(y_psych_safe ~ x_inc_lead + x_inc_lead_gpm + x_learn_ort_gpc*x_lead_learn_ort + (x_learn_ort_gpc | team_id), data_ps)

anova(mod_2a, mod_3)


# Simulate Growth Model ---------------------------------------------------

set.seed(888645)

n2 <- 200
n1 <- 16
n <- n1*n2

ind_id <- rep(1:n2, each = n1)
Z <- model.matrix(~as.factor(ind_id) - 1)

Z_slope <- rep(0:(n1 - 1), n2) * Z



ran_int <- rnorm(n2, sd = 1)
ran_slope <- -.30*ran_int + rnorm(n2, sd = sqrt(1 - var(-.30*ran_int)))
ran_slope <- sqrt(.25)*ran_slope
ran_int <- sqrt(.50)*ran_int

ran_eff <- matrix(c(ran_int, ran_slope), ncol = 1)

x_workout <- round(3.80 + rnorm(n, sd = sqrt(.50)) + Z%*%t(t(rnorm(n2, sd = sqrt(1)))), 2)
# x_workout[x_workout > 7] <- 7

Z <- cbind(Z, Z_slope)

data_growth <- 
  tibble::tibble(
    id = ind_id,
    time = rep(1:n1, n2),
    x_gender = rep(sample(c(0, 1), n1, prob = c(.60, .40), replace = TRUE), each = n2),
    x_workout = x_workout
  ) |>
  dplyr::group_by(
    id
  ) |>
  dplyr::mutate(
    x_workout_gpm = mean(x_workout),
    time_0 = time - 1
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    y_health = 4 + -.30 * time_0 + .10 * x_workout + .10*x_workout_gpm + .25*x_workout_gpm*time_0  + -.80*x_gender + Z%*%ran_eff + rnorm(n, sd = sqrt(.50)),
    y_health = round(as.numeric(y_health), 2),
    y_health = as.numeric(scale(y_health)),
    y_health = 6 + y_health*sqrt(3),
    x_workout = as.numeric(x_workout)
  ) |>
  dplyr::select(
    person = id,
    time,
    time_0,
    x_sex = x_gender,
    x_exr = x_workout,
    x_exr_gpm = x_workout_gpm,
    y_health
  )


# Fit Growth Models -------------------------------------------------------

mod_null <- lmerTest::lmer(y_health ~ 1 + (1 | person), data_growth)
mod_within <- lmerTest::lmer(y_health ~ time_0 + x_exr + (time_0 + x_exr | person), data_growth)
mod_within_1 <- lmerTest::lmer(y_health ~ time_0 + x_exr + (time_0 | person), data_growth)
mod_within_2 <- lmerTest::lmer(y_health ~ time_0 + x_exr + (x_exr | person), data_growth)

anova(mod_null, mod_within)
anova(mod_within, mod_within_1)
anova(mod_within, mod_within_2)

mod_between <- lmerTest::lmer(y_health ~ time_0 * x_exr_gpm + x_sex + (time_0 | person), data_growth)

anova(mod_within_1, mod_between)