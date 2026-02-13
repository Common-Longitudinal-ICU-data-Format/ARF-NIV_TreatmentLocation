######################## 1. BINARY OUTCOMES ########################

# The below code iterates over each individual *binary* outcome, like death_hospice and organ_failure_yn,
# so outcome=death_hospice in the first iteration, etc.

# Within the outcome loop, it iterates from i = 1 to i = NUMBER_OF_HOSPITALS

# Run logistic regression
# Note: model_equation is defined as outcome ~ icu_admission + covariate_1 + covariate_2 + ...
# Note: model_data includes all patient-level row data, subsetted to the given hospital in the loop
model_1 <- glm(
  model_equation,
  data=model_data,
  family=binomial
)

# Marginal effects

# Risk difference
marginal_effects_slopes <- avg_slopes(
  model=model_1,
  newdata = datagrid(
    newdata=model_data,
    grid_type="counterfactual",
    icu_admission=c(0,1),
    variables="icu_admission"
  )
) |>
filter(term=="icu_admission")

# Predictions of outcome for ICU and ward
marginal_effects_preds <- avg_predictions(
  model=model_1,
  newdata = datagrid(
    newdata=model_data,
    grid_type="counterfactual",
    icu_admission=c(0,1)
  ),
  by = "icu_admission"
) 

# Save all of the model outcomes
model_coefficients <- model_coefficients |>
  add_row(
    site = site,
    hospital = hosp_data$first_hospital_id[i],
    imc_capable = hosp_data$imc_capable[i],
    academic_community = hosp_data$academic_community[i],
    outcome=outcome,
    beta=coeftest(model_1)["icu_admission","Estimate"],
    beta_error=coeftest(model_1)["icu_admission","Std. Error"],
    n_obs=nobs(model_1),
    p_value=coeftest(model_1)["icu_admission","Pr(>|z|)"],
    risk_difference=marginal_effects_slopes$estimate,
    risk_difference_error=marginal_effects_slopes$std.error,
    risk_difference_p=marginal_effects_slopes$p.value,
    prediction_icu=(marginal_effects_preds |> filter(icu_admission==1))$estimate,
    prediction_icu_error=(marginal_effects_preds |> filter(icu_admission==1))$std.error,
    prediction_icu_p=(marginal_effects_preds |> filter(icu_admission==1))$p.value,
    prediction_ward=(marginal_effects_preds |> filter(icu_admission==0))$estimate,
    prediction_ward_error=(marginal_effects_preds |> filter(icu_admission==0))$std.error,
    prediction_ward_p=(marginal_effects_preds |> filter(icu_admission==0))$p.value,
)

# After, I save model_coefficients as a file that has the same format as "model_coefficients_binary" 
# referenced in 05_example_pooling.R, wherein each hospital-outcome pair gets its own row

######################## 2. COUNT OUTCOMES ########################

# The below code iterates over each individual *ordinal* outcome, like los_ordinal (length of stay) 
# and resp_support_free_days, so outcome=los_ordinal in the first iteration, etc.

# Within the outcome loop, it iterates from i = 1 to i = NUMBER_OF_HOSPITALS


# Run culmulative logistic regression
model_2 <- clm(
  model_equation,
  data=model_data,
  link="logit"
)

# Save all of the model outcomes
model_coefficients <- model_coefficients |>
  add_row(
    site = site,
    hospital = hosp_data$first_hospital_id[i],
    imc_capable = hosp_data$imc_capable[i],
    academic_community = hosp_data$academic_community[i],
    outcome=outcome,
    beta=coeftest(model_2)["icu_admission","Estimate"],
    beta_error=coeftest(model_2)["icu_admission","Std. Error"],
    n_obs=nobs(model_2),
    p_value=coeftest(model_2)["icu_admission","Pr(>|t|)"]
  )

# After, I save model_coefficients as a file that has the same format as "model_coefficients_counts" 
# referenced in 05_example_pooling.R, wherein each hospital-outcome pair gets its own row