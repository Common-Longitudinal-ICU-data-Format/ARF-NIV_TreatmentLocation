# Sarah Goldfarb
# 03/09/2026

units <- c("icu", "ward", "stepdown")

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
      "logistf"#,
      #"ridge"
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
    cat("Load config to specify local paths\n")
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
    cat("Create folders if needed\n")
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
  
  { # Reading in data
    cat("Reading in data\n")
    final_cohort <- read_csv(paste0(
      project_location, 
      "/private_tables/model_data.csv"), 
      show_col_types=FALSE)
    
    # gamma blups (predicted log odds of death at each hospital) (determined in file 5b)
    hospital_effect_all <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_intercepts_by_hosp.csv"
    ), 
    show_col_types=FALSE)
      
    # Global fixed effect of each covariate (determined in file 4b)
    bbar_all <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_by_hosp.csv"
    ), 
    show_col_types=FALSE)
    
    cov_names <- read_csv(paste0(project_location, 
                                 "/global_model_outputs/cov_names.csv"),
                          show_col_types = FALSE)|>pull(term)

  } # Reading in data
  
} # Setup

{ # Calculate event rate
  
  cat("Calculating event rate\n")
  
  all_event_rates <- data.frame(
    outcome=character(),
    unit=character(),
    local_hospital=character(),
    n=integer(),
    clif_hospital=character(),
    event_rate=numeric(),
    stringsAsFactors = FALSE
  )
  
  for(outcome_i in outcomes_binary){
    cat(paste0("> For outcome = ", outcome_i, "\n"))
    for(unit_i in units){
        cat(paste0("     > In unit = ", unit_i, "\n"))
      counterfactual_data <- final_cohort |>
        filter(triage_location==unit_i) |>
        select(first_hospital_id,
               all_of(cov_names)) |>
        drop_na()
      
      hosp_ids_local <- unique(counterfactual_data$first_hospital_id)
      n_hosp_local <- length(hosp_ids_local)
      
      
      hospital_effect_df <- hospital_effect_all|>
        filter(outcome==outcome_i, unit==unit_i)
      
      hospital_effect <- hospital_effect_df$gamma_blup
      hosp_ids_CLIF <- hospital_effect_df$first_hospital_id
      n_hosp_CLIF <- length(hosp_ids_CLIF)
      
      event_rate <- matrix(data=NA, 
                           nrow=n_hosp_local, 
                           ncol=n_hosp_CLIF,
                           dimnames=list(hosp_ids_local, hosp_ids_CLIF))
      
      bbar <- bbar_all[[paste0(outcome_i, ".", unit_i)]]
      
      n_patients <- data.frame(first_hospital_id=hosp_ids_local, n=NA_integer_)
      
      # Iterate over each local hospital
      for(ind in c(1:n_hosp_local)){
        cat(paste0("          > At local hospital = ", hosp_ids_local[ind], "\n"))
        # Subset to only data at this hospital
        counterfactual_data_hosp <- counterfactual_data |>
          filter(first_hospital_id == hosp_ids_local[ind]) |>
          # Remove hospital column
          select(-first_hospital_id)
        
        # Save how many patients are going into this model for this hospital
        n_patients$n[ind] <- nrow(counterfactual_data_hosp)
        
        # Iterate over each CLIF hospital
        for(k in c(1:n_hosp_CLIF)){
          cat(paste0("               > At CLIF hospital = ", hosp_ids_CLIF[k], "\n"))
          # Hospital effect at THIS clif hospital
          gamma_i <- hospital_effect[k]
          
          # Log odds for each patient
          val <- gamma_i + as.matrix(counterfactual_data_hosp) %*% as.matrix(bbar)
          # Convert to risk for each patient
          risk_val <- exp(val)/(1+exp(val))
          
          # Put into matrix, taking sum of risk across all patients
          event_rate[ind,k]<-sum(risk_val)
        }
      }
      
      # DOUBLE CHECK WITH JESSIE, since N is not constant
      # nrow(counterfactual_data) is the number of patients used for the model
      # at the entire site (all hospitals at a single site) for a given unit (e.g., icu)
      # and for a given outcome (e.g., death_hospice)
      # event_rate_final <- apply(event_rate,2,sum)/nrow(counterfactual_data)
      
      event_rate_long <- as.data.frame(as.table(event_rate))
      colnames(event_rate_long) <- c("local_hospital", "clif_hospital", "event_rate")
      
      all_event_rates <- all_event_rates |>
        add_row(event_rate_long |>
                  left_join(n_patients, by=c("local_hospital"="first_hospital_id")) |> 
                  mutate(outcome=outcome_i,
                         unit=unit_i))
      
    }
  }
  
  cat("Event rates saved... saving output...\n")
  write_csv(all_event_rates, paste0(project_location, site,"_project_output/local_model_outputs/",
                                    site, "_all_event_rates.csv"))

  cat("Run successfully!\n")
  
} # Calculate event rate