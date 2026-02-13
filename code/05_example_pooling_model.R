######################## 1. BINARY OUTCOMES ########################

# The below code gives an example of running the regression on a specific 
# binary outcome, death_hospice, but is replicated for each binary outcome

# Note that model_coefficients_binary has already been combined such that each
# hospital from all sites has been combined into this one data frame

# Subset to only outcome of interest
example_coeffs_binary <- model_coefficients_binary |>
  filter(outcome == "death_hospice")

# Please see the file "example_coeffs_binary.csv" attached to the email
# to inspect the format of the data

###### 1.1. Log Odds###### 
log_odds_no_mods <- rma(
  data=example_coeffs_binary,
  yi=beta,
  sei=beta_error,
  method="REML"
)

summary(log_odds_no_mods) # See attached word file for outputs

log_odds_with_mods <- rma(
  data=example_coeffs_binary,
  yi=beta,
  sei=beta_error,
  method="REML",
  mods= ~ imc_capable + academic_community
)

summary(log_odds_with_mods) # See attached word file for outputs

###### 1.2. Predictions ###### 

### 1.2.1. ICU ###
icu_pred_no_mods <- rma(
  data=example_coeffs_binary,
  yi=prediction_icu,
  sei=prediction_icu_error,
  method="REML"
)

summary(icu_pred_no_mods) # See attached word file for outputs

icu_pred_with_mods <- rma(
  data=example_coeffs_binary,
  yi=prediction_icu,
  sei=prediction_icu_error,
  method="REML",
  mods= ~ imc_capable + academic_community
)

summary(icu_pred_with_mods) # See attached word file for outputs

### 1.2.2. Ward ###

ward_pred_no_mods <- rma(
  data=example_coeffs_binary,
  yi=prediction_ward,
  sei=prediction_ward_error,
  method="REML"
)

summary(ward_pred_no_mods) # See attached word file for outputs

ward_pred_with_mods <- rma(
  data=example_coeffs_binary,
  yi=prediction_ward,
  sei=prediction_ward_error,
  method="REML",
  mods= ~ imc_capable + academic_community
)

summary(ward_pred_with_mods) # See attached word file for outputs

######################## 2. COUNT OUTCOMES ########################

# The below code gives an example of running the regression on a specific 
# count outcome, resp_support_free_days, but is replicated for each count outcome

# Note that model_coefficients_counts has already been combined such that each
# hospital from all sites has been combined into this one data frame

# Subset to only outcome of interest
example_coeffs_counts <- model_coefficients_counts |>
  filter(outcome == "resp_support_free_days")

# Please see the file "example_coeffs_counts.csv" attached to the email
# to inspect the format of the data

prop_odds_no_mods <- rma(
  data=example_coeffs_counts,
  yi=beta,
  sei=beta_error,
  method="REML"
)

summary(prop_odds_no_mods)

prop_odds_with_mods <- rma(
  data=example_coeffs_counts,
  yi=beta,
  sei=beta_error,
  method="REML",
  mods= ~ imc_capable + academic_community
)

summary(prop_odds_with_mods)