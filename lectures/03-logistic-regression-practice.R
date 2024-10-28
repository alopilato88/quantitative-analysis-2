library(mfx) 
library(tibble)
library(dplyr)
library(ggplot2)


# Simulate data -----------------------------------------------------------

# Simulate our data
set.seed(1065) # Ensures we can reproduce our data
n <- 2500 # Sample Size 

# Generate CSR variable
x_csr <- sample(1:7, size = n, replace = TRUE,
                prob = dnorm(1:7, mean = 3.5, sd = 2) / sum(dnorm(1:7, mean = 3.5, sd = 2)))

# Generate CC (Customer Identification)
x_cc <- sample(0:1, size = n, replace = TRUE, prob = c(.70, .30))

x_csr_c <- x_csr - median(x_csr)

# Create the linear predictor
donate_lin <- -1.50 + .8 * x_csr_c + 1.2 * x_cc

# Simulate the outcome according to a bernoulli distribution
donate <- rbinom(n, size = 1, prob = plogis(donate_lin))

# Save our data in a data frame named data_donate 
data_donate <- 
  tibble::tibble(
    donate,
    x_cc, 
    x_csr
  )

# Describing our outcome --------------------------------------------------

# Calculating the propotion that donate
prop_donate <- mean(data_donate$donate)

# Calculating the odds of donation
odds_donate <- prop_donate / (1 - prop_donate)

# Can invert odds if it makes more sense to talk about the non-event/failure 
1 / odds_donate
(1 - prop_donate) / prop_donate

# Transforming odds back to propotions
odds_donate / (1 + odds_donate)

# Transforming odds into log odds and then back 
log_odds <- log(odds_donate)

exp(log_odds)

# Why we work in log odds -------------------------------------------------

# Odds when probability = 0
lowest_odds <- 0 / (1 - 0)

# Odds when Probability = 1
highest_odds <- 1 / (1 - 1)

# Now lets take the log of lowest_odds -- log odds
log(lowest_odds)

probability_range <- seq(0, 1, by = .05)
odds_range <- probability_range / (1 - probability_range)
log_odds_range <- log(odds_range)

# Log odds are symettric around zero 
tibble::tibble(
  probability = probability_range,
  one_minus_probability = 1 - probability_range,
  log_odds = log_odds_range,
  log_odds_of_one_minus_prob = -log_odds
) |> View()

# Modeling Donations with CSR ---------------------------------------------

# Fit a logistic regression model regressing donate onto x_csr
mod_csr <- glm(donate ~ x_csr, family = binomial(link = "logit"),
               data = data_donate)

summary(mod_csr)

# For every increase in CSR, the log odds of donating increase by .725
mod_csr$coefficients[2]

# Log odds for someone with a 4 on CSR
-4.036 + .725*4

log_odds_4 <- mod_csr$coefficients[1] + mod_csr$coefficients[2]*4

# Log odds fro someone with a 5 on CSR
log_odds_5 <- mod_csr$coefficients[1] + mod_csr$coefficients[2]*5

log_odds_5 - log_odds_4

# But log odds are hard to think in, so we tranform them into odds with `exp()`
odds_4 <- exp(log_odds_4)
odds_5 <- exp(log_odds_5)

# Now using the odds we can create an odds ratio: 
odds_ratio <- odds_5 / odds_4

# So the odds of donating for someone who responds 5 to CSR are 2.06 times greater 
# than the odds of donating for someone who responds 4 to CSR.

# It turns out this odds_ratio is equal to exp(mod_csr$coefficients[2])
exp(mod_csr$coefficients[2])

# So when we use the exponential function, exp(), to transform the logistic regression parameters, 
# we can interpret the transformed number as an odds ratio: For every one unit increase in CSR, 
# a person odds of donating are doubled.

# We can also get the percent change in odds for every unit increase in the predictor as: 

100 * ((odds_5 - odds_4) / (odds_4) )
(exp(mod_csr$coefficients[2]) - 1) * 100

# For every unit increase in CSR, the odds of donating increase by 106%. 

# Finally we can also transform the odds into probabilities: 
prob_4 <- odds_4 / (1 + odds_4)
prob_5 <- odds_5 / (1 + odds_5)
prob_5 - prob_4

# Changing in CSR from 4 to 5 increase the probability of donation from .24 to .39 for a 
# 15 percentage point increase
# But what about if we move from 1 to 2 on CSR?
prob_1 <- predict(mod_csr, data.frame(x_csr = 1), type = "response")
prob_2 <- predict(mod_csr, data.frame(x_csr = 2), type = "response")

# Changing in CSR from 1 to 2 increase the probability from .04 to .07 for a 3 percentage point
# change
prob_2 - prob_1

prob_5 <- predict(mod_csr, data.frame(x_csr = 5), type = "response")
prob_6 <- predict(mod_csr, data.frame(x_csr = 6), type = "response")
prob_6 - prob_5

# To get the averge percentage point change across all values of the predictor we can use the function
# mfx::logitmfx
mfx::logitmfx(mod_csr, atmean = FALSE, data = data_donate)

# Think of this as, on average, a unit increase will lead to about an 11 percentage point increase, 
# but this change may be lower or higher depending on where on CSR they are moving up from. 

# Maximum percentage point change we'll see is: 
mod_csr$coefficients[2]/4

# To find the point on the predictor where this change happens, we can divide 
# -B0 / B1 (coefficient for predictor)
-mod_csr$coefficients[1] / mod_csr$coefficients[2]

prob_5 <- predict(mod_csr, data.frame(x_csr = 5), type = "response")
prob_6 <- predict(mod_csr, data.frame(x_csr = 6), type = "response")
prob_6 - prob_5


# What does this look like in our actual data? 
data_donate |>
  dplyr::summarize(
    prob = mean(donate),
    .by = x_csr
  ) |>
  dplyr::arrange(x_csr) |>
  dplyr::mutate(
    odds = prob / (1 - prob),
    lag_prob = dplyr::lag(prob),
    lag_odds = dplyr::lag(odds),
    diff_prob = prob - lag_prob,
    odds_ratio = odds / lag_odds
  )


# Categorical Predictor ---------------------------------------------------

data_donate <- dplyr::mutate(data_donate, x_cust_id = dplyr::if_else(x_cc == 1, "Yes", "No"))

mod_cust_id <- glm(donate ~ x_cust_id, data = data_donate, 
                   family = binomial(link = "logit"))

