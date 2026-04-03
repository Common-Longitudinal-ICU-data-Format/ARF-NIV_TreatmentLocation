# Sarah Goldfarb
# 03/09/2026

# TODO BEFORE RUNNING: define which sites have contributed
sites <- c("Hopkins", "UCMC")
units <- c("icu", "ward", "stepdown")

{ # Setup
  
  { # Load needed packages
    packages <- c(
      "tidyverse",
      "yaml",
      "rprojroot",
      "metafor"
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
    project_location <- config$project_location
    
    covariates <- global_config$covariates
    outcomes_binary <- global_config$outcomes_binary
    outcomes_counts <- global_config$outcomes_counts
    
    rm(config, global_config)
  } # Load config to specify local paths
  
  { # Create directories as needed
    
    output_dir <- paste0(project_location, "/global_model_outputs/")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
  } # Create directories as needed
  
  { # Load all hospital data
    
    hospital_information <- data.frame(
      first_hospital_id = character(),
      #n_patients = integer(),
      imc_capable = character(),
      academic_community = character(),
      stringsAsFactors = FALSE
    )
    
    # Loop through each site to load them
    for(site in sites){
      hospital_information <- hospital_information |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/", site,
                                "_hospital_data.csv"),
                         show_col_types = FALSE) |>
                  select(-c(n_patients)) |>
                  mutate(imc_capable = as.character(imc_capable)))
    }
    
  } # Load all hospital data
  
  { # Load all gammas and their errors
    
    # Identify column names
    gamma_cols <- c("measure",
                    "site",
                    "first_hospital_id", 
                    as.vector(outer(outcomes_binary, units, paste, sep=".")))
    no_ward_cols <- gamma_cols[!grepl("ward", gamma_cols)]
    
    # Initialize the dataframe of gammas
    gamma_by_hosp = #gamma_by_imc = 
      as.data.frame(
        setNames(
          c(list(character(), character(), character()),
            rep(list(numeric()), length(gamma_cols) - 3)),
          gamma_cols
        )
      )
    
    imc_vs_icu_gamma =
      as.data.frame(
        setNames(
          c(list(character(), character(), character()),
            rep(list(numeric()), length(no_ward_cols) - 3)),
          no_ward_cols
        )
      )
    
    # Helper function to read in gammas
    read_in_gammas <- function(measure, stratum, site){
      return(read_csv(paste0(
        project_location,
        site, "_project_output/local_model_outputs/",
        site,"_", measure, "_by_", stratum, ".csv"),
        show_col_types =FALSE) |>
          mutate(measure=measure, site=site))
    }
    
    # Iterate over each site and read in the gamma values
    for(site in sites){
      gamma_by_hosp <- gamma_by_hosp |>
        add_row(read_in_gammas("gamma", "hosp", site)) |>
        add_row(read_in_gammas("gamma_error", "hosp", site))
      
      # gamma_by_imc <- gamma_by_imc |>
      #   add_row(read_in_gammas("gamma", "imc_cap", site)) |>
      #   add_row(read_in_gammas("gamma_error", "imc_cap", site))
      
      hospital_data <- read_csv(paste0(
        project_location,site, "_project_output/", site,"_hospital_data.csv"
      ),
                                show_col_types =FALSE)
      
      if(nrow(hospital_data|>filter(imc_capable==1))){
        imc_vs_icu_gamma <- imc_vs_icu_gamma |>
          add_row(read_csv(paste0(
            project_location,
            site, "_project_output/local_model_outputs/",
            site,"_imc_vs_icu_gamma.csv"),
            show_col_types =FALSE) |>
              mutate(measure="gamma", site=site)) |>
          add_row(read_csv(paste0(
            project_location,
            site, "_project_output/local_model_outputs/",
            site,"_imc_vs_icu_gamma_error.csv"),
            show_col_types =FALSE) |>
              mutate(measure="gamma_error", site=site))
      }

    }
    
  } # Load all gammas and their errors
   
} # Setup

{ # Run meta regressions (only for hospital-level)
  run_rma <- function(gamma_list_all, units, mod_vars = c("imc_capable", "academic_community")){
    gamma_BLUPs <- list()
    for(outcome_i in outcomes_binary){
      for(unit_i in units){
        # Identify column name with relevant data (e.g., "death_hospice.icu")
        col_name <- paste0(outcome_i, ".", unit_i)
        
        # Ensure each gamma and error are matched to the right hospital
        matched_df <- gamma_list_all |>
          left_join(hospital_information, by="first_hospital_id") |>
          select(first_hospital_id, 
                 measure, 
                 !!sym(col_name), 
                 imc_capable, 
                 academic_community) |>
          pivot_wider(
            names_from=measure,
            values_from=!!sym(col_name)
          ) |>
          mutate(
            imc_capable= factor(imc_capable),
            academic_community = factor(academic_community)
          ) |>
          # Remove NAs
          drop_na()
        
        gamma_met_reg_new <- rma.uni(yi=gamma,
                                     vi=gamma_error^2,
                                     #mods = ~ academic_community,
                                     mods = reformulate(mod_vars),
                                     data=matched_df)
        
        gamma_BLUPs[[outcome_i]][[unit_i]] <- data.frame(
          first_hospital_id = unique(matched_df$first_hospital_id),
          gamma_blup = blup(gamma_met_reg_new)$pred,
          gamma_blup_se = blup(gamma_met_reg_new)$se)
        
      }
    }
    
    return(gamma_BLUPs)
  }
  
  gamma_preds <- run_rma(gamma_by_hosp, units=units)
  gamma_preds_imc_vs_icu <- run_rma(imc_vs_icu_gamma, units=c("icu", "stepdown"), 
                                    mod_vars="academic_community")
  
  gamma_preds_df <- imap_dfr(gamma_preds, \(outcome_list, outcome_name) {
    imap_dfr(outcome_list, \(df, unit_name) {
      df %>%
        mutate(
          outcome = outcome_name,
          unit = unit_name
        )
    })
  })
  
  gamma_preds_imc_vs_icu_df <- imap_dfr(gamma_preds_imc_vs_icu, \(outcome_list, outcome_name) {
    imap_dfr(outcome_list, \(df, unit_name) {
      df %>%
        mutate(
          outcome = outcome_name,
          unit = unit_name
        )
    })
  })
  
  write_csv(gamma_preds_df, 
            paste0(output_dir,"global_intercepts_by_hosp.csv"))
  
  write_csv(gamma_preds_imc_vs_icu_df, 
            paste0(output_dir,"global_intercepts_imc_vs_icu.csv"))
  
} # Run meta regressions (only for hospital-level)