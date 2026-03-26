####################################################################################
############################### Run at each hospital ###############################
####################################################################################

# Note that each hospital only has access to it's own patient-level data

# Variables
# counterfactual_data_hosp:   each row is a patient's covariates
# bbar:                       covariate estimate vector
# n_patients:                 the number of patients at this hospital
# n_hosps:                    the number of hospitals in the federated network
# hospital_effect:            a vector of intercepts (gamma) of length n_hosps

# Iterate over each hospital in the federated network 
# to simulate the counterfactual if patients from this local hospital
# were sent to the counterfactual hospital
for(k in c(1:n_hosps)){
  
  # Hospital effect the kth hospital in the network
  gamma_i <- hospital_effect[k]
  
  # For each patient, calculate the log odds of the outcome
  val <- gamma_i + as.matrix(counterfactual_data_hosp) %*% as.matrix(bbar)
  
  # For each patient, convert to odds
  odds_val <- exp(val)
  
  # For each patient, convert to risk
  risk_val <- odds_val/(1+odds_val)
  
  # Sum all risks together to get a "sum event rate"
  # Will divide by the number of patients after summing across federation 
  event_rate[k]<-sum(risk_val)
}

# Convert to long
event_rate_long <- as.data.frame(as.table(event_rate))
# Rename columns
colnames(event_rate_long) <- c("local_hospital", 
                               "simulated_federation_hospital", 
                               "event_rate")
# Add column to keep track of number of patients
event_rate_long <- event_rate_long |>
  mutate(n=n_patients)

# Later in the code, we save event_rate_long to a csv

####################################################################################
############################# Run at the central site ##############################
####################################################################################

# Earlier in the code, we pull in each local hospital's "event_rate_long" csv
# Then, we stack them into a matrix called local_event_rates


final_event_rate <- local_event_rates |>
  group_by(simulated_federation_hospital) |>
  summarise(
    # Sums the total number of patients across all local hospitals that generated counterfactual
    # event rates at a given simulated_federation_hospital
    n = sum(n),
    # Sums the total number of summed event rates across all local hospitals
    events = sum(event_rate),
    # Keeps track of the local hospitals that contributed to this counterfactual risk
    contributing_hospitals = paste(sort(unique(local_hospital)), collapse = ", "),
    .groups = "drop"
  ) |>
  # Add column for event rate, calculated as the summed events divided by 
  # the total number of contributing patients
  mutate(event_rate = events/n)

# In the end, we have a vector, final_event_rate, of length n_hospitals
# We interpret this as the overall risk of the outcome (i.e., death) at each
# hospital within the federation