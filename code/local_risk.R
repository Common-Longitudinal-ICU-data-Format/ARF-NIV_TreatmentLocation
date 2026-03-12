
{ # Setup
  { # Load needed packages
    packages <- c(
      "tidyverse",
      "yaml",
      "rprojroot",
      "stringr",
      "lmtest",
      "sandwich",
      "marginaleffects",
      "ordinal",
      "broom",
      "logistf",
      "ridge"
    )
    
    install_if_missing <- function(package) {
      if (!require(package, character.only = TRUE)) {
        install.packages(package, dependencies = TRUE)
        library(package, character.only = TRUE)
      }
    }
    
    sapply(packages, install_if_missing)
    rm(packages, install_if_missing)
  } # Load needed packages
  
  { # Load config to specify local paths
    # Find project root
    project_root <- find_root(rprojroot::has_dir("config"))
    
    # Read YAML config
    config <- yaml::read_yaml(file.path(project_root, "config", "config.yaml"))
    global_config <- yaml::read_yaml(file.path(project_root, "config", "global_config.yaml"))
    
    # Assign config values to R variables
    tables_location <- config$tables_location
    project_location <- config$project_location
    site <- config$institution
    site_time_zone <- config$time_zone
    
    covariates <- global_config$covariates
    outcomes_binary <- c("death_hospice")
    
    rm(config, global_config)
  } # Load config to specify local paths
  
  { # Create folders if needed
    # Check if the output directory exists; if not, create it
    if (!dir.exists(paste0(project_location, "/private_tables"))) {
      dir.create(paste0(project_location, "/private_tables"))
    }
    if (!dir.exists(paste0(project_location, "/", site, "_project_output"))) {
      dir.create(paste0(project_location,"/", site, "_project_output"))
    }
    if (!dir.exists(paste0(project_location, "/", site, "_project_output/local_model_outputs"))) {
      dir.create(paste0(project_location,"/", site, "_project_output/local_model_outputs"))
    }
    
    model_out_dir <- file.path(project_location, paste0(site, "_project_output/local_model_outputs/"))
  } # Create folders if needed
  
  { # Reading in and reformatting data
    final_cohort <- read_csv(paste0(project_location, "/private_tables/one_encounter_per_pt_with_stratification.csv"), show_col_types=FALSE)
    reformat_cohort_cols <- function(df) {
      
      # Set order of triage location for foactoring
      traige_location_factors <- c("ICU", "IMC", "Ward")
      traige_location_imc_factors <- c("ICU (+)", "ICU (-)", "IMC", "Ward (+)", "Ward (-)")
      
      
      return(df |>
               mutate(
                 triage_location = factor(triage_location),
                 # Format triage location
                 triage_location_formatted = case_when(
                   triage_location == "icu" ~ "ICU",
                   triage_location == "stepdown" ~ "IMC",
                   triage_location == "ward" ~ "Ward",
                   TRUE ~ triage_location),
                 # Set triage location as factor
                 triage_location_formatted = factor(triage_location_formatted, levels = traige_location_factors),
                 # Categorize triage location based on IMC availability
                 traige_location_imc_avail = case_when(
                   triage_location_formatted == "ICU" & imc_capable == 1 ~ "ICU (+)",
                   triage_location_formatted == "ICU" ~ "ICU (-)",
                   triage_location_formatted == "IMC" ~ "IMC",
                   triage_location_formatted == "Ward" & imc_capable == 1 ~ "Ward (+)",
                   triage_location_formatted == "Ward" ~ "Ward (-)",
                   TRUE ~ triage_location_formatted
                 ),
                 # Set triage  as factor
                 traige_location_imc_avail = factor(traige_location_imc_avail, levels = traige_location_imc_factors),
                 # Factor last NIV device
                 last_niv_device = factor(last_niv_device, levels=c("NIPPV", "High Flow NC")),
                 
                 # Create binary variable if patient is female
                 is_female = as.factor(ifelse(tolower(sex_category) == "female", 1, 0)),
                 
                 # Binary variable if patient had imv (set as NA if patient was DNI)
                 imv = ifelse(is_dni, NA, ifelse(was_trach == 1 | was_intubated == 1, 1, 0)),
                 
                 # Binary variable if patient died or discharged to hospice at all
                 death_hospice = ifelse(in_hosp_death == 1 | hospice == 1, 1, 0),
                 
                 first_hospital_id = factor(first_hospital_id),
                 
                 imc_capable = factor(imc_capable),
                 
                 # Set sepsis as factor
                 sepsis_in_ed = as.factor(sepsis_in_ed),
                 
                 # Set code status as full/presumed full vs other
                 code_status_full = as.factor(case_when(
                   is.na(code_status_category) ~ 1, # Presume full if no code status charted
                   str_detect(tolower(code_status_category), "full") ~ 1,
                   TRUE ~ 0)),
                 
                 # Make year a factor
                 year = as.factor(year),
                 
                 # Set the era (pre covid, during covid, post covid), with reference being the post covid era
                 era = factor(
                   case_when(
                     start_ed < as.POSIXct("2020-03-01") ~ "Pre",
                     start_ed > as.POSIXct("2022-02-28") ~ "Post",
                     TRUE ~ "During"
                   ),
                   levels=c("Post", "During", "Pre")
                 ),
                 
                 season = factor(season, levels=c("Winter", "Spring", "Summer", "Autumn")),
                 
                 # Set cutoffs for SF
                 sf_factor = factor(
                   case_when(
                     is.na(sf_value) ~ "Not present",
                     sf_value > 300 ~ "> 300",
                     sf_value > 250 ~ "> 250",
                     sf_value > 200 ~ "> 200",
                     sf_value > 120 ~ "> 120",
                     TRUE ~ "<= 120"
                   ),
                   levels=c("Not present", "> 300","> 250", "> 200","> 120", "<= 120")
                 ),
                 
                 # Set cutoffs for pH
                 ph_factor = factor(
                   case_when(
                     is.na(ph_value) ~ "Not present",
                     ph_value > 7.35 ~ "> 7.35",
                     ph_value > 7.30 ~ "> 7.30",
                     ph_value > 7.25 ~ "> 7.25",
                     TRUE ~ "<= 7.20"
                   ),
                   levels=c("Not present", "> 7.35", "> 7.30", "> 7.25", "<= 7.20")
                 ),
                 
                 # Center continuous covariates
                 age_sub_65 = age_at_admission - 60,
                 bmi_sub_30 = first_bmi - 30
                 
               ))
    }
    
    final_cohort <- reformat_cohort_cols(final_cohort)
    
    hospital_data <- read_csv(paste0(project_location,"/", 
                                     site, "_project_output/", site,"_hospital_data.csv"), 
                              show_col_types=FALSE)
    cov_names <- read_csv(paste0(project_location, 
                                 "/global_model_outputs/cov_names.csv"),
                          show_col_types = FALSE)|>pull(term)
    
  } # Reading in and reformatting data
  
} # Setup

# Returns df of estimates for each term, with standard error
run_direct_standardization <- function(cohort_data, hosp_data, strat_var){
  
  # Initialize the model output dataframe
  model_output <- data.frame(
    site = character(),
    regression_model=character(),
    hospital = character(),
    imc_capable = character(),
    academic_community = character(),
    outcome=character(),
    unit=character(),
    term=character(),
    estimate=numeric(),
    std.error=numeric(),
    p.value=numeric(),
    n.obs=integer(),
    stringsAsFactors = FALSE
  )
  
  # Run logistic regressions
  run_regression_per_unit <- function(model_data_unit, unit, i){
    
    # Subset to only cases within relevant unit (icu, stepdown, ward)
    model_data_unit <- model_data_unit |>
      filter(triage_location==unit)
    
    cat(paste0("      > Running for ", unit,
               " cases (n = ", nrow(model_data_unit),")...\n"))
    
    # Record which model was used
    used_model <- NA
    
    # 1) Try running regression as glm
    tryCatch({
      cat("            > Attempting GLM... ")
      unit_model <- glm(
        model_equation,
        data=model_data_unit,
        family=binomial
      )
      used_model <- "GLM"
      cat("GLM successful!\n")
    },
    warning=function(w){
      cat("GLM failed!\n")
      message(paste0("              ",w$message))
    }
    )
    
    # 2) If failed, attempt logistf
    if(is.na(used_model)){
      tryCatch({
        cat("            > Attempting logistf... ")
        unit_model <- logistf(
          model_equation,
          data=model_data_unit,
          family=binomial,
          control = logistf.control(
            maxit = 5000,      # increase iterations
            maxstep = 0.1       # reduce step size if unstable
          )
        )
        used_model <- "logistf"
        cat("logistf successful!\n")
      },
      warning=function(w){
        cat("logistf failed!\n")
        message(paste0("              ",w$message))
      })
    }
    
    # # 3) If failed, attempt ridge regression
    # if(is.na(used_model)){
    #   tryCatch({
    #       cat("            > Attempting ridge... ")
    #     unit_model <- logisticRidge(
    #       model_equation,
    #       data = model_data_unit,
    #       na.action=na.omit)
    #       used_model <- "ridge"
    #       cat("ridge successful!\n")
    #     },
    #     warning=function(w){
    #       cat("ridge failed!\n")
    #       message(paste0("              ",w$message))
    #     })
    # }
    
    # If all failed, return warning
    if(is.na(used_model)){
      
      cat("            > --- ALL MODELS FAILED --- !\n")
      
      return(data.frame(
        site = site,
        regression_model="None: all failed",
        imc_capable = ifelse(strat_var == "first_hospital_id", as.character(hosp_data$imc_capable[i]),i),
        hospital = ifelse(strat_var == "first_hospital_id", hosp_data$first_hospital_id[i],NA),
        academic_community =ifelse(strat_var == "first_hospital_id", hosp_data$academic_community[i],NA),
        outcome=outcome,
        unit=unit,
        term=NA,
        estimate=NA,
        std.error=NA,
        p.value=NA,
        n.obs=NA
      ))
    }
    
    term_list <- switch(
      used_model,
      "GLM" = names(unit_model$coeff),
      "logistf" = unit_model$terms,
      "ridge" = names(coef(unit_model)),
      NA
    )
    estimate_list <- switch(
      used_model,
      "GLM" = unit_model$coeff,
      "logistf" = unit_model$coefficients,
      "ridge" = coef(unit_model),
      NA
    )
    std.error_list <- switch(
      used_model,
      "GLM" = summary(unit_model)$coefficients[,"Std. Error"],
      "logistf" = sqrt(diag(unit_model$var)),
      "ridge" = NaN,
      NA
    )
    p.value_list <- switch(
      used_model,
      "GLM" = summary(unit_model)$coefficients[,"Pr(>|z|)"],
      "logistf" = unit_model$prob,
      "ridge" = NaN,
      NA
    )
    n.obs_list <- switch(
      used_model,
      "GLM" = nobs(unit_model),
      "logistf" = unit_model$n,
      "ridge" = length(unit_model$y),
      NA
    )
    
    regression_output <- data.frame(
      site=site,
      regression_model = used_model,
      imc_capable=i,
      outcome=outcome,
      unit=unit,
      term=term_list,
      estimate=estimate_list,
      std.error=std.error_list,
      p.value=p.value_list,
      n.obs=n.obs_list
    )
    
    if(strat_var == "first_hospital_id"){
      regression_output <- regression_output |> 
        mutate(
          hospital=hosp_data$first_hospital_id[i],
          imc_capable = as.character(hosp_data$imc_capable[i]),
          academic_community = hosp_data$academic_community[i]
        )
    }
    
    return(regression_output)
    
  }
  
  # Iterate over each binary outcome of interest
  for(outcome in outcomes_binary){
    model_equation <- paste0(outcome, " ~ ", paste(covariates, collapse = " + "))
    cat("\n---------------------\n")
    cat("Cohort: ", deparse(substitute(cohort_data)), "\n")
    cat("Stratifying and running separate models by: ", deparse(substitute(strat_var)), "\n")
    cat(paste0("Running models using equation:\n", model_equation, "\n\n"))
    
    # Iterate over each hospital at this local site if stratifying by hospital
    if(strat_var=="first_hospital_id"){
      for(i in seq_len(nrow(hosp_data))){
        cat(paste0("   > Running for: ",hosp_data$first_hospital_id[i], "...\n"))
        
        # Only select rows at the given hospital
        model_data <- cohort_data |> 
          filter(first_hospital_id == hosp_data$first_hospital_id[i])
        
        model_output <- model_output |>
          add_row(run_regression_per_unit(model_data, "icu", i))|>
          add_row(run_regression_per_unit(model_data, "ward", i))
        
        if(hospital_data$imc_capable[i] == 1){
          model_output <- model_output |>
            add_row(run_regression_per_unit(model_data, "stepdown", i))
        }
        else{
          cat(paste0("      > Not IMC capable\n"))
        }
      }
    }
    else if (strat_var == "imc_capable"){
      for(i in levels(cohort_data$imc_capable)){
        cat(paste0("   > Running for IMC capable == ", i, "...\n"))
        
        # Only select rows with correct IMC designation
        model_data <- cohort_data |> 
          filter(imc_capable == i)
        
        model_output <- model_output |>
          add_row(run_regression_per_unit(model_data, "icu", i))|>
          add_row(run_regression_per_unit(model_data, "ward", i))
        
        if(i == 1){
          model_output <- model_output |>
            add_row(run_regression_per_unit(model_data, "stepdown", i))
        }
      }
    }
    else{
      stop("Stratifying variable must be hospital (first_hospital_id) or imc capability (imc_capable)")
    }
    
  }
  return(model_output)
  
}

# Run stratified based on hospital or IMC capability
local_fixed_effects <- run_direct_standardization(
  final_cohort, 
  hospital_data, 
  strat_var="first_hospital_id")

######################################################################
reshape_patient_covariate_cols <- function(cohort_data){
  
  # Identify which covariate columns are factors
  factor_cols <- names(cohort_data |> select(covariates))[sapply(cohort_data |> select(covariates), is.factor)]
  
  # Reshape matrix to be dummy variables for factors
  dummy_matrix <- model.matrix(
    ~ . - 1,
    data = cohort_data[factor_cols]
  )[,-1] |> as.data.frame()
  
  # Identify which columns need to be converted back to factors
  factorized_covs <- cov_names[grepl(paste(factor_cols, collapse="|"), cov_names)]
  
  # Reshape overall patient row level data with factorized dummy variables
  return(cohort_data |>
           select(-all_of(factor_cols)) |>
           bind_cols(dummy_matrix) |>
           # re-order them to match
           select(c("triage_location", 
                    "first_hospital_id", 
                    "imc_capable",
                    cov_names, 
                    outcomes_binary)) |>
           # Set as factors rather than numeric
           mutate(across(all_of(factorized_covs), ~factor(.)))
  )
}

reshaped_final_cohort <- reshape_patient_covariate_cols(final_cohort)

######################################################################

model_equation <- paste0("death_hospice  ~ ", paste(covariates, collapse = " + "))
print(model_equation)

pred_patient_risks <- c()

for(unit_i in units){
  for(hospital_i in hospital_data$first_hospital_id){
    
    model_data_unit <- final_cohort |>
      filter(triage_location==unit_i, first_hospital_id==hospital_i) |>
      # NEED TO REMOVE NAs HERE
      filter(if_all(all_of(covariates), ~!is.na(.)))
    
    print(paste0(hospital_i, ".", unit_i,"       ", nrow(model_data_unit)))
    
    if(nrow(model_data_unit)>0){
      
    unit_model <- glm(
      as.formula(model_equation),
      data=model_data_unit,
      family=binomial
    )
    
    patient_predictions <- predict(unit_model)
    patient_risk <- exp(patient_predictions)/(1+exp(patient_predictions))
    
    pred_patient_risks[paste0(hospital_i, ".", unit_i)] <- mean(patient_risk)
    print( mean(patient_risk))
    }
    else{
      pred_patient_risks[paste0(hospital_i, ".", unit_i)] <- NA
      print("Not IMC capable")
    }
    
  }
}

######################################################################

# Definitions:
# hospital_data$first_hospital_id: includes c("BMC", "JHH", etc.)
# reshaped_final_cohort: patient row-level data with columns for each covariate, outcome, unit, hospital_id
# local_fixed_effects: fixed effects for the intercept and slopes for the model death_hospice ~ covariates run as a glm
# cov_names: a list of the covariate names (e.g., "age_sub_65", "bmi_sub_30", "seasonAutmn")

units <- c("icu", "stepdown", "ward")
mean_risk_list <- c()

for(unit_i in units){
  for(hospital_i in unique(hospital_data$first_hospital_id)){
    
    # Select only patients at this hospital in this unit
    model_data <- reshaped_final_cohort |>
      filter(triage_location==unit_i,
             first_hospital_id==hospital_i)
    
    # Ensure data has rows (e.g., for unit_i == stepdown in an INCAPABLE hospital, the rows would be 0)
    if(nrow(model_data) > 0){
      
      print(cov_names)
      print(local_fixed_effects|>
              filter(hospital==hospital_i, unit==unit_i, outcome=="death_hospice", term!="(Intercept)") |>
              pull(term))
      print(identical(
        cov_names,local_fixed_effects|>
          filter(hospital==hospital_i, unit==unit_i, outcome=="death_hospice", term!="(Intercept)") |>
          pull(term)
      ))
      
      # Local fixed effects generated for this outcome within a given unit at THIS HOSPITAL only
      local_beta_meta_fix <- local_fixed_effects|>
        filter(hospital==hospital_i, unit==unit_i, outcome=="death_hospice", term!="(Intercept)") |>
        pull(estimate)
      
      # Local gamma generated for this outcome within a given unit at THIS HOSPITAL only
      local_intercept <- local_fixed_effects|>
        filter(hospital==hospital_i, unit==unit_i, outcome=="death_hospice", term=="(Intercept)") |>
        pull(estimate)
      
      if(length(local_beta_meta_fix)==0 || length(local_intercept)==0){
        mean_risk_list[paste0(hospital_i, ".", unit_i)] = NA
        next
      }
      
      # Calculate os here
      os = as.matrix(model_data |>
                       # Select only covairate cols
                       select(cov_names) |>
                       # Convert to numeric
                       mutate(across(everything(), as.character))|>
                       # Convert to numeric
                       mutate(across(everything(), as.numeric))
      ) %*% 
        as.matrix(local_beta_meta_fix)
      
      # Add os, val, and risk to patient-level data 
      # (note: val and risk are not usually added here; just here for troubleshooting now)
      model_data <- transform(model_data,os=os) |>
        mutate(val = os + local_intercept,
               patient_risk = exp(val)/(1+exp(val)))
      
      # Save the mean risk
      mean_risk_list[paste0(hospital_i, ".", unit_i)] = mean(model_data$patient_risk, na.rm=T)
      
    }
    else{
      mean_risk_list[paste0(hospital_i, ".", unit_i)] = NA
    }
  }
}



###################
View(
as.data.frame(lapply(mean_risk_list,function(x)sprintf("%.3f",x))) |>
  pivot_longer(cols=everything(),names_to="hospital_unit", values_to="average_risk")
)