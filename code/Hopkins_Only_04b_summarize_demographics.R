# Sarah Goldfarb
# 03/11/2026


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
    
    output_dir <- paste0(project_location, "/compiled_final_results/")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
  } # Create directories as needed
  
  { # Load in demographic output
    
    load_demographic_csv <- function(filename, site){
      return(read_csv(paste0(project_location, site, "_project_output/", 
                             site, "_", filename, ".csv"),
                      show_col_types = FALSE))
    }
    
    # TODO (load in actually table 1 and 2)
    
  } # Load in demographic output
  
  # { # Load in logit regression outputs
  #   
  #   load_logit_output <- function(filename, site){
  #     return(read_csv(
  #       paste0(project_location, site, "_project_output/local_model_outputs/",
  #              site, "_", filename, ".csv"),
  #       show_col_types = FALSE
  #     ))
  #   }
  #   
  #   # Initialize outcomes dfs
  #   # logit_icu_adm <- load_logit_output()
  #   logit_icu_v_ward <- load_logit_output("old_logit_model_coefficients", sites[1])
  #   logit_icu_v_imc <- load_logit_output("imc_v_icu_logit_model_coefficients", sites[1])
  #   
  #   # Append results from other sites, if exist
  #   if(length(sites) > 1){
  #     for(site in sites[2:length(sites)]){
  #       temp_df <- load_logit_output("old_logit_model_coefficients", site)
  #       if(nrow(temp_df) > 0 & !is.na(temp_df$hospital[1])){
  #         logit_icu_v_ward <- logit_icu_v_ward|>
  #           add_row(temp_df)
  #       }
  #       temp_df <- load_logit_output("imc_v_icu_logit_model_coefficients", site)
  #       if(nrow(temp_df) > 0 & !is.na(temp_df$hospital[1])){
  #         logit_icu_v_imc <- logit_icu_v_imc|>
  #           add_row(temp_df)
  #       }
  #     }
  #     rm(temp_df)
  #   }
  #   
  # } # Load in logit regression outputs
  
} # Setup

# { # Meta analysis for logit regressions
#   for(outcome_i in outcomes_binary){
#     icu_v_ward_rma_data <- logit_icu_v_ward |>
#       filter(outcome==outcome_i)
#     icu_v_imc_rma_data <- logit_icu_v_imc |>
#       filter(outcome==outcome_i)
#     
#     icu_v_ward_log_odds <- rma(data=icu_v_ward_rma_data,yi=beta,sei=beta_error, method="REML")
#     
#     
#     icu_v_ward_icu_preds <- rma(data=icu_v_ward_rma_data,
#                                 yi=prediction_icu,
#                                 sei=prediction_icu_error, 
#                                 method="REML")
#     icu_v_ward_ward_preds <- rma(data=icu_v_ward_rma_data,
#                                 yi=prediction_non_icu,
#                                 sei=prediction_non_icu_error, 
#                                 method="REML")
#     
#     icu_v_ward_log_odds_w_mods <- rma(data=icu_v_ward_rma_data,yi=beta,sei=beta_error, 
#                                       method="REML", mods = ~imc_capable+academic_community)
#     
#     
#     icu_v_ward_icu_preds_w_mods <- rma(data=icu_v_ward_rma_data,
#                                 yi=prediction_icu,
#                                 sei=prediction_icu_error, 
#                                 method="REML", mods = ~imc_capable+academic_community)
#     icu_v_ward_ward_preds_w_mods <- rma(data=icu_v_ward_rma_data,
#                                  yi=prediction_non_icu,
#                                  sei=prediction_non_icu_error, 
#                                  method="REML", mods = ~imc_capable+academic_community)
#     
#     # Include modifers for imc_capable for above three
#     
#     icu_v_imc_log_odds <- rma(data=icu_v_imc_rma_data,yi=beta,sei=beta_error, method="REML")
#     
#     icu_v_imc_icu_preds <- rma(data=icu_v_imc_rma_data,
#                                 yi=prediction_icu,
#                                 sei=prediction_icu_error, 
#                                 method="REML")
#     icu_v_imc_imc_preds <- rma(data=icu_v_imc_rma_data,
#                                  yi=prediction_non_icu,
#                                  sei=prediction_non_icu_error, 
#                                  method="REML")
#     
#     results <- data.frame(
#       icu_v_ward_odds = exp(icu_v_ward_log_odds$b[1]),
#       icu_v_imc_odds = exp(icu_v_imc_log_odds$b),
#       icu_v_ward_icu_pred = icu_v_ward_icu_preds$b,
#       icu_v_ward_ward_pred = icu_v_ward_ward_preds$b,
#       icu_v_imc_icu_pred = icu_v_imc_icu_preds$b,
#       icu_v_imc_imc_pred = icu_v_imc_imc_preds$b
#     )
#     
#   }
# } # Meta analysis for logit regressions

