# Sarah Goldfarb
# 03/09/2026

units_all <- c("icu", "ward", "stepdown")

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
    
    # For secondary analysis
    hospital_effect_secondary <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_intercepts_imc_vs_icu.csv"
    ), 
    show_col_types=FALSE)
    bbar_secondary <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_imc_icu_together.csv"
    ), 
    show_col_types=FALSE)
    
    hospital_data <- read_csv(paste0(
      project_location, "/", site, "_project_output/",
      site,"_hospital_data.csv"), 
      show_col_types=FALSE)
    
    imc_capable_hospitals <- hospital_data|>
      filter(imc_capable==1) |>
      pull(first_hospital_id)

  } # Reading in data
  
} # Setup

{ # Calculate event rate
  
  calc_event_rate <- function(my_hospital_effects, my_bbar, 
                              my_cohort=final_cohort, units=units_all,
                              incl_unit_in_colname = T, imc_cap_only=F){
  
    all_event_rates <- data.frame(
      outcome=character(),
      unit=character(),
      local_hospital=character(),
      n=integer(),
      clif_hospital=character(),
      sum_risks=numeric(),
      sum_w=numeric(),
      stringsAsFactors = FALSE
    )
    
    all_xw <- data.frame(
      outcome = character(),
      unit = character(),
      local_hospital = character(),
      clif_hospital = character(),
      setNames(
        replicate(length(cov_names), numeric(0), simplify = FALSE),
        cov_names
      ),
      check.names=FALSE
    )
    
    for(outcome_i in outcomes_binary){
      cat(paste0("> For outcome = ", outcome_i, "\n"))
      for(unit_i in units){
        
        cat(paste0("     > In unit = ", unit_i, "\n"))
        
        counterfactual_data <- my_cohort |>
          filter(triage_location==unit_i) |>
          select(first_hospital_id,
                 all_of(cov_names)) |>
          drop_na()
        
        hosp_ids_local <- unique(counterfactual_data$first_hospital_id)
        # Only include hospitals that are IMC capable for secondary analysis
        if(imc_cap_only==TRUE){
          hosp_ids_local <- hosp_ids_local[hosp_ids_local %in% imc_capable_hospitals]
        }
        n_hosp_local <- length(hosp_ids_local)
        
        
        hospital_effect_df <- my_hospital_effects |>
          filter(outcome==outcome_i, unit==unit_i)
        
        hospital_effect <- hospital_effect_df$gamma_blup
        hosp_ids_CLIF <- hospital_effect_df$first_hospital_id
        n_hosp_CLIF <- length(hosp_ids_CLIF)
        
        sum_risks <- matrix(data=NA, 
                             nrow=n_hosp_local, 
                             ncol=n_hosp_CLIF,
                             dimnames=list(hosp_ids_local, hosp_ids_CLIF))
        sum_w <- matrix(data=NA, 
                            nrow=n_hosp_local, 
                            ncol=n_hosp_CLIF,
                            dimnames=list(hosp_ids_local, hosp_ids_CLIF))
        
        sum_xw <- list()
        
        if(incl_unit_in_colname){
          bbar <- my_bbar[[paste0(outcome_i, ".", unit_i)]]
        }
        else{
          bbar <- my_bbar[[outcome_i]]
        }
        
        n_patients <- data.frame(first_hospital_id=hosp_ids_local, n=NA_integer_)
        
        # Iterate over each local hospital
        for(ind in c(1:n_hosp_local)){
          cat(paste0("          > At local hospital = ", hosp_ids_local[ind], "\n"))
          
          sum_xw[[hosp_ids_local[ind]]] <- list()
          
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
            # Mathematical derivative of the risk
            w <- risk_val * (1-risk_val)
            
            # Put into matrix, taking sum of risk across all patients
            sum_risks[ind,k]<-sum(risk_val)
            sum_w[ind, k] <- sum(w)
            sum_xw[[hosp_ids_local[ind]]][[hosp_ids_CLIF[k]]] <- colSums(as.vector(risk_val) *
                                            as.matrix(counterfactual_data_hosp))
            
          }
        }
        
        sum_risks_long <- as.data.frame(as.table(sum_risks))
        colnames(sum_risks_long) <- c("local_hospital", "clif_hospital", "sum_risks")
        
        sum_w_long <- as.data.frame(as.table(sum_w))
        colnames(sum_w_long) <- c("local_hospital", "clif_hospital", "sum_w")
        
        # Reshape this to a readable df
        sum_xw <- imap_dfr(sum_xw, function(inner_list, local_name) {
          imap_dfr(inner_list, function(vec, clif_name) {
            as_tibble_row(vec) %>%
              mutate(
                local_hospital = local_name,
                clif_hospital = clif_name,
                .before = 1
              )
          })
        })
        
        all_event_rates <- all_event_rates |>
          add_row(sum_risks_long |>
                    left_join(sum_w_long, by=c("local_hospital", "clif_hospital")) |>
                    left_join(n_patients, by=c("local_hospital"="first_hospital_id")) |> 
                    mutate(outcome=outcome_i,
                           unit=unit_i))
        
        all_xw <- all_xw |>
          add_row(sum_xw |>
                    mutate(outcome=outcome_i, unit=unit_i))
        
      }
    }
    
    return(list(all_event_rates = all_event_rates, all_xw=all_xw))
  
  }
  
  cat("Calculating event rate for primary analysis\n")
  primary_outcomes <- calc_event_rate(hospital_effect_all, bbar_all)
  
  
  cat("Event rates saved... saving output...\n")
  write_csv(primary_outcomes[["all_event_rates"]], paste0(project_location, site,"_project_output/local_model_outputs/",
                                    site, "_all_event_rates.csv"))
  write_csv(primary_outcomes[["all_xw"]], paste0(project_location, site,"_project_output/local_model_outputs/",
                                    site, "_all_xw.csv"))
  
  if(length(imc_capable_hospitals > 0)){
    cat("Calculating event rate for secondary analysis\n")
    secondary_outcomes <- calc_event_rate(hospital_effect_secondary, bbar_secondary, 
                                          units=c("icu", "stepdown"), 
                                          incl_unit_in_colname=F,
                                          imc_cap_only=T)
    
    cat("Event rates saved... saving output...\n")
    write_csv(secondary_outcomes[["all_event_rates"]], paste0(project_location, site,"_project_output/local_model_outputs/",
                                                              site, "_secondary_event_rates.csv"))
    write_csv(secondary_outcomes[["all_xw"]], paste0(project_location, site,"_project_output/local_model_outputs/",
                                                     site, "_secondary_xw.csv"))
  }
  else{
    cat("No IMC capable hospitals at this site; skipping secondary analysis")
  }

  cat("Run successfully!\n")
  
} # Calculate event rate