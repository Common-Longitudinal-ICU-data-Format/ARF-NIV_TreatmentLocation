# Sarah Goldfarb
# 03/04/2026

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
    outcomes_binary <- global_config$outcomes_binary
    outcomes_counts <- global_config$outcomes_counts
    
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
  
  } # Reading in and reformatting data

} # Setup

{ # Report covariate distributions, stratified
  
  # Returns a list of two dfs:distributions for categorical and continuous covariates
  get_covariate_distributions <- function(cohort_data, strat_var = "first_hospital_id"){
    
    continuous_covariates <- data.frame(
      site=character(),
      stratum=character(),
      unit=character(),
      covariate=character(),
      data_type=character(),
      n_obs=integer(),
      mean=numeric(),
      std.dev=numeric(),
      stringsAsFactors = FALSE
    )
    
    categorical_covariates <- data.frame(
      site=character(),
      stratum=character(),
      unit=character(),
      covariate=character(),
      data_type=character(),
      n_obs=integer(),
      level=character(),
      count=character(),
      stringsAsFactors = FALSE
    )
    
    for(covariate in covariates){
      
      # Switch to actual covariate if shifted continuous one
      if(startsWith(covariate, "age_sub")){
        covariate <- "age_at_admission"
      }
      else if (startsWith(covariate, "bmi_sub")){
        covariate <- "first_bmi"
      }
      
      data_type <- class(cohort_data[[covariate]])
      
      cat(sprintf("%-10s %-25s %-5s %-15s\n", "Covariate:", covariate, "Type:", data_type))
      
      if(data_type=="numeric"){
        
        for(stratum in levels(cohort_data[[strat_var]])){
          
          for(unit in levels(cohort_data$triage_location)){
            numeric_data <- cohort_data |>
              filter(triage_location == unit,
                     !!sym(strat_var) == stratum)
            
            numeric_data <- numeric_data[[covariate]]
            
            continuous_covariates <- continuous_covariates |>
              add_row(
                site=site,
                stratum=stratum,
                unit=unit,
                covariate=covariate,
                data_type=data_type,
                n_obs=length(!is.na(numeric_data)),
                mean= ifelse(n_obs == 0, NA, mean(numeric_data, na.rm=TRUE)),
                std.dev=ifelse(n_obs == 0, NA, sd(numeric_data, na.rm=TRUE))
              )
          }
        }
        
      }
      # Otherwise, would be factor
      else if(data_type=="factor"){
        factor_data <- as.data.frame(
          table(cohort_data[[covariate]], 
                cohort_data[[strat_var]], 
                cohort_data$triage_location)
        )
        
        colnames(factor_data) <- c("level", "stratum", "unit", "count")
        
        factor_data <- factor_data |>
          mutate(site=site, data_type=data_type, covariate=covariate) |>
          group_by(stratum, unit)|>
          mutate(n_obs=sum(count),
                 count=case_when(
                   unit=="stepdown" & n_obs==0 ~ NA,
                   TRUE ~ count
                 )) |>
          ungroup() |>
          mutate(
            count = case_when(
              count < 5 ~ "< 5",
              TRUE ~ as.character(count)
            )
          )
        
        categorical_covariates <- categorical_covariates |> 
          add_row(factor_data)
      }
      else{
        stop("Data type must be numeric or factor")
      }
    }
    return(list(categorical_covariates=categorical_covariates, continuous_covariates=continuous_covariates))
  }
  
  { # Run function and save files
    
    # For stratification by hospital (default)
    cat("\nFor stratification by hospital...\n")
    covariate_distributions_by_hosp <- get_covariate_distributions(final_cohort)
    
    write_csv(covariate_distributions_by_hosp[["categorical_covariates"]], 
              paste0(model_out_dir, site,"_categorical_covariate_distributions_by_hosp.csv"))
    write_csv(covariate_distributions_by_hosp[["continuous_covariates"]], 
              paste0(model_out_dir, site,"_continuous_covariate_distributions_by_hosp.csv"))
    
    # For stratification by IMC admitting capability
    cat("\nFor stratification by IMC admitting capability...\n")
    covariate_distributions_by_imc_cap <- get_covariate_distributions(final_cohort, strat_var="imc_capable")
    
    write_csv(covariate_distributions_by_imc_cap[["categorical_covariates"]], 
              paste0(model_out_dir, site,"_categorical_covariate_distributions_by_imc_cap.csv"))
    write_csv(covariate_distributions_by_imc_cap[["continuous_covariates"]], 
              paste0(model_out_dir, site,"_continuous_covariate_distributions_by_imc_cap.csv"))
  } # Run function and save files
  
} # Report covariate distributions, stratified

{ # Run logit models (old)
  run_logit_models_old <- function(cohort_data, hosp_data, locations_incl=c("icu", "ward")){
    
    # Remove IMC patients and add indicator if admitted to ICU
    all_model_data <- cohort_data |>
      filter(triage_location %in% locations_incl) |>
      mutate(icu_admission = ifelse(tolower(triage_location)=="icu", 1, 0)) |>
      # Remove unused factor levels if not in use (ie for sensitivity analysis without pandemic years)
      droplevels()
    
    # Initialize the model coefficients dataframe
    model_coefficients <- data.frame(
      site = character(),
      hospital = character(),
      imc_capable = numeric(),
      academic_community = character(),
      outcome=character(),
      beta=numeric(),
      beta_error=numeric(),
      n_obs=integer(),
      p_value=numeric(),
      risk_difference=numeric(),
      risk_difference_error=numeric(),
      risk_difference_p=numeric(),
      prediction_icu=numeric(),
      prediction_icu_error=numeric(),
      prediction_icu_p=numeric(),
      prediction_non_icu=numeric(),
      prediction_non_icu_error=numeric(),
      prediction_non_icu_p=numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over each binary outcome of interest
    for(outcome in outcomes_binary){
      model_equation <- paste0(outcome, " ~ icu_admission + ", paste(covariates, collapse = " + "))
      cat("\n---------------------\n")
      cat("Cohort: ", deparse(substitute(cohort_data)), "\n")
      cat(paste0("Running models using equation:\n", model_equation, "\n\n"))
      
      # Iterate over each hospital at this local site
      for(i in seq_len(nrow(hosp_data))){
        cat(paste0("   > Running for: ",hosp_data$first_hospital_id[i], "...\n   "))
        
        if(setequal(locations_incl,c("icu", "stepdown")) & hosp_data$imc_capable[i]==0){
          cat(paste0(" This hospital is not IMC capable, skipped!\n"))
          next
        }
        
        # Only select rows at the given hospital
        model_data <- all_model_data |>
          filter(first_hospital_id == hosp_data$first_hospital_id[i])
        
        # Run logistic regression
        model_1 <- glm(
          model_equation,
          data=model_data,
          family=binomial
        )
        
        # Marginal effects
        marginal_effects_slopes <- avg_slopes(
          model=model_1,
          newdata = datagrid(
            #model=model_1,
            newdata=model_data,
            grid_type="counterfactual",
            icu_admission=c(0,1),
            variables="icu_admission"
          )
        ) |>
          filter(term=="icu_admission")
        
        
        marginal_effects_preds <- avg_predictions(
          model=model_1,
          newdata = datagrid(
            #model=model_1,
            newdata=model_data,
            grid_type="counterfactual",
            icu_admission=c(0,1)
          ),
          by = "icu_admission"
        )
        
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
            prediction_non_icu=(marginal_effects_preds |> filter(icu_admission==0))$estimate,
            prediction_non_icu_error=(marginal_effects_preds |> filter(icu_admission==0))$std.error,
            prediction_non_icu_p=(marginal_effects_preds |> filter(icu_admission==0))$p.value,
          )
        
        cat(paste0(" Complete!\n"))
      }
    }
    
    if(nrow(model_coefficients>0))
      return(model_coefficients)
    
    return(
      data.frame(
        site = site,
        hospital = NA_character_,
        imc_capable = NA,
        academic_community = NA_character_,
        outcome=NA_character_,
        beta=NA,
        beta_error=NA,
        n_obs=NA,
        p_value=NA,
        risk_difference=NA,
        risk_difference_error=NA,
        risk_difference_p=NA,
        prediction_icu=NA,
        prediction_icu_error=NA,
        prediction_icu_p=NA,
        prediction_non_icu=NA,
        prediction_non_icu_error=NA,
        prediction_non_icu_p=NA
      )
    )
  }
  
  # Run and save
  model_coefficients_old <- run_logit_models_old(final_cohort, hospital_data)
  
  write_csv(model_coefficients_old, paste0(model_out_dir, site,
                                           "_old_logit_model_coefficients.csv"))
} # Run logit models (old)

{ # Run direct standardization to obtain local coefficients
  
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
  direct_stand_outcomes_by_hosp <- run_direct_standardization(
    final_cohort, 
    hospital_data, 
    strat_var="first_hospital_id")
  direct_stand_outcomes_by_imc_cap <- run_direct_standardization(
    final_cohort, 
    hospital_data, 
    strat_var="imc_capable")
  
  # Save outcomes
  write_csv(direct_stand_outcomes_by_hosp, 
            paste0(model_out_dir, site,"_direct_standardization_by_hosp.csv"))
  write_csv(direct_stand_outcomes_by_imc_cap, 
            paste0(model_out_dir, site,"_direct_standardization_by_imc_cap.csv"))
  
} # Run direct standardization to obtain local coefficients

{ # Run process outcome of ICU admission
  run_secondary_models <- function(cohort_data, hosp_data){
    
    # KEEP IMC patients for the secondary analysis
    all_model_data <- cohort_data |>
      mutate(icu_admission = ifelse(tolower(triage_location)=="icu", 1, 0)) |>
      # Remove unused factor levels if not in use (ie for sensitivity analysis without pandemic years)
      droplevels()
    
    # Initialize the model output dataframe
    model_output <- data.frame(
      site = character(),
      hospital = character(),
      imc_capable = character(),
      academic_community = character(),
      term=character(),
      estimate=numeric(),
      std.error=numeric(),
      p.value=numeric(),
      n.obs=integer(),
      stringsAsFactors = FALSE
    )
    
    # Initialize the model predictions dataframe
    model_predictions <- data.frame(
      site = character(),
      hospital = character(),
      imc_capable = numeric(),
      academic_community = character(),
      outcome=character(),
      n_obs=integer(),
      prediction_icu_adm=numeric(),
      prediction_icu_error=numeric(),
      prediction_icu_p=numeric(),
      stringsAsFactors = FALSE
    )
    
    # Model ICU admission as the process outcome, using only covariates
    model_equation <- paste0("icu_admission ~ ", paste(covariates, collapse = " + "))
    cat("\n---------------------\n")
    cat("Cohort: ", deparse(substitute(cohort_data)), "\n")
    cat(paste0("Running models using equation:\n", model_equation, "\n\n"))
    
    # Iterate over each hospital at this local site
    for(i in seq_len(nrow(hosp_data))){
      cat(paste0("   > Running for: ",hosp_data$first_hospital_id[i], "...\n   "))
      
      # Only select rows at the given hospital
      model_data <- all_model_data |>
        filter(first_hospital_id == hosp_data$first_hospital_id[i])
      
      # Run logistic regression
      model_1 <- glm(
        model_equation,
        data=model_data,
        family=binomial
      )
      
      model_output <- model_output |>
        add_row(data.frame(
          site = site,
          hospital =hosp_data$first_hospital_id[i],
          imc_capable = as.character(hosp_data$imc_capable[i]),
          academic_community = hosp_data$academic_community[i],
          term=names(model_1$coeff),
          estimate=model_1$coeff,
          std.error=summary(model_1)$coefficients[,"Std. Error"],
          p.value=summary(model_1)$coefficients[,"Pr(>|z|)"],
          n.obs=nobs(model_1)
        ))
      
      # No counterfactual since ICU admission is dependent variable
      marginal_effects_preds <- avg_predictions(model=model_1)
      
      model_predictions <- model_predictions |>
        add_row(
          site = site,
          hospital = hosp_data$first_hospital_id[i],
          imc_capable = hosp_data$imc_capable[i],
          academic_community = hosp_data$academic_community[i],
          outcome="icu_admission",
          prediction_icu_adm=marginal_effects_preds$estimate,
          prediction_icu_error=marginal_effects_preds$std.error,
          prediction_icu_p=marginal_effects_preds$p.value,
          n_obs=nobs(model_1)
        )
      
      cat(paste0(" Complete!\n"))
    }
    return(list(model_coeff=model_output, model_predictions=model_predictions))
  }
  
  icu_adm_regression_outputs <- run_secondary_models(final_cohort, hospital_data)
  
  write_csv(icu_adm_regression_outputs$model_coeff, 
            paste0(model_out_dir, 
                   site,"_icu_adm_model_coefficients.csv"))
  write_csv(icu_adm_regression_outputs$model_predictions, 
            paste0(model_out_dir, 
                   site,"_icu_adm_model_predictions.csv"))
  
} # Run process outcome of ICU admission

{ # Compare outcomes between icu vs imc at imc-capable hospitals
  
  # Run and save
  model_coefficients_imc_icu <- run_logit_models_old(final_cohort, 
                                                 hospital_data, 
                                                 locations_incl=c("icu", "stepdown"))
  
  write_csv(model_coefficients_imc_icu, paste0(model_out_dir, site,
                                           "_imc_v_icu_logit_model_coefficients.csv"))
} # Compare outcomes between icu vs imc at imc-capable hospitals