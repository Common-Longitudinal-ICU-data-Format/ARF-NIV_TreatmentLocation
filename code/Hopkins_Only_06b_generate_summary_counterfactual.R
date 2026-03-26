# Sarah Goldfarb
# 03/09/2026

# TODO BEFORE RUNNING: define which sites have contributed
sites <- c("Hopkins")#, "UCMC")
units_all <- c("icu", "ward", "stepdown")

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
  
  { # Load covariate names and beta error
    cov_names <- read_csv(paste0(project_location, 
                                 "/global_model_outputs/cov_names.csv"),
                          show_col_types = FALSE)|>pull(term)
    # Global fixed effect of each covariate (determined in file 4b)
    global_coeff_vars <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_vars_by_hosp.csv"
    ), 
    show_col_types=FALSE)
    
    global_coeff_vars_secondary <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_vars_imc_icu_together.csv"
    ), 
    show_col_types=FALSE)
  } # Load covariate names
  
  { # Load all event rates
    
    # Initialize the dataframe of event rates
    all_event_rates = secondary_event_rates = data.frame(
      outcome=character(),
      unit=character(),
      local_hospital=character(),
      n=integer(),
      clif_hospital=character(),
      sum_risks=numeric(),
      sum_w=numeric(),
      stringsAsFactors = FALSE
    )
    
    all_xw = secondary_xw <- data.frame(
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
    
    
    
    # Iterate over each site and read in the gamma values
    for(site in sites){
      all_event_rates <- all_event_rates |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_all_event_rates.csv"),
                         show_col_types = FALSE))
      
      all_xw <- all_xw |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_all_xw.csv"),
                         show_col_types = FALSE))
      
      
      secondary_event_rates <- secondary_event_rates |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_secondary_event_rates.csv"),
                         show_col_types = FALSE))
      
      secondary_xw <- secondary_xw |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_secondary_xw.csv"),
                         show_col_types = FALSE))
      
    }
    
  } # Load all event rates
  
} # Setup


rser_ci_from_site_summaries <- function(
    N_total,
    sum_p,
    sum_w,
    sum_xw,
    var_beta, # covariance matrix of covariate coeffs
    z = qnorm(0.975)
    ) {       
  
  # RSER estimate is the weighted average across sites
  p_hat <- sum_p / N_total
  
  # gradients of RSER
  g_gamma <- sum_w / N_total
  g_beta  <- sum_xw / N_total
  
  # variance for gamma_hat
  I11 <- sum_w
  I12 <- sum_xw
  var_gamma <- as.numeric(1 / I11 + t(I12) %*% var_beta %*% I12 / (I11^2))
  
  # delta method variance for RSER
  var_rser <- as.numeric(g_gamma^2 * var_gamma + t(g_beta) %*% var_beta %*% g_beta)
  se_rser <- sqrt(var_rser)
  
  data.frame(p_hat = p_hat,
             var   = var_rser,
             se    = se_rser,
             lower = p_hat - z * se_rser,
             upper = p_hat + z * se_rser)
}

{ # Calculate final event rate for each hospital at each level of care for each outcome
  
  calc_final_event_rate <- function(my_event_rates,
                                    my_xw,
                                    my_coeff_vars,
                                    units=units_all,
                                    incl_unit_in_colname = T){
    
    final_event_rate_combined <- data.frame(
      outcome=character(),
      unit=character(),
      clif_hospital = character(),
      event_rate = numeric(),
      n_patients=integer(),
      contributing_hospitals=character(),
      stringsAsFactors = FALSE
    )
    
    rser_ci_results <- data.frame(
      outcome=character(),
      unit=character(),
      clif_hospital = character(),
      p_hat = numeric(),
      var = numeric(),
      se = numeric(),
      lower = numeric(),
      upper=numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over each outcome and unit
    for(outcome_i in outcomes_binary){
      for(unit_i in units){
        
        # Primary analysis
        if(incl_unit_in_colname == TRUE){
          # Saved variance of the covariate coefficient vector from prior file
          # Save as diagonal matrix
          var_beta <- diag(my_coeff_vars[[paste0(outcome_i, ".", unit_i)]])
        }
        # Secondary analysis
        else{
          var_beta <- diag(my_coeff_vars[[outcome_i]])
        }
        
        # Subset event rates to only for this outcome and unit
        event_rate <- my_event_rates |>
          filter(outcome==outcome_i, unit==unit_i) |>
          select(-c(outcome, unit)) |>
          # Summarize the event rate, grouped by counterfactual clif hospital
          group_by(clif_hospital) |>
          summarise(
            n = sum(n),
            sum_risks = sum(sum_risks),
            sum_w = sum(sum_w),
            contributing_hospitals = paste(sort(unique(local_hospital)), collapse = ", "),
            .groups = "drop"
          ) |>
          mutate(event_rate = sum_risks/n, 
                 outcome=outcome_i, unit=unit_i)
        
        # Subset xw (derivative) to only for this outcome and unit
        xw_i <- my_xw |> 
          filter(outcome==outcome_i, unit==unit_i) |>
          select(-c(outcome, unit)) |>
          # Summarize the xw (derivative), grouped by counterfactual clif hospital
          group_by(clif_hospital) |>
          summarize(
            across(all_of(cov_names), sum, na.rm = TRUE),
            .groups = "drop"
          )
        
        for(clif_hosp_j in unique(event_rate$clif_hospital)){
          
          event_rate_j <- event_rate|>filter(clif_hospital==clif_hosp_j)
          
          xw_j <- unlist((xw_i|>filter(clif_hospital==clif_hosp_j)|>select(-clif_hospital))[1,], use.names=TRUE)
          
          rser_ci_results <- rser_ci_results |>
            add_row(rser_ci_from_site_summaries(event_rate_j$n, 
                                      event_rate_j$sum_risks, 
                                      event_rate_j$sum_w,
                                      xw_j,
                                      var_beta) |>
                      mutate(
                        clif_hospital = clif_hosp_j,
                        unit=unit_i,
                        outcome=outcome_i
                      ))
        }
        
        final_event_rate_combined <- final_event_rate_combined |>
          add_row(event_rate |>
                    select(outcome, unit, clif_hospital, event_rate,
                           n_patients=n,
                           contributing_hospitals))
        
      }
    }
    
    if(nrow(final_event_rate_combined) != nrow(rser_ci_results)){
      stop("Error: final event rate and CI results do not have the same number of rows")
    }
    
    final_output <- final_event_rate_combined |>
      left_join(rser_ci_results, by=c("outcome", "unit", "clif_hospital"))
    
    if(!isTRUE(all.equal(final_output$event_rate, final_output$p_hat))){
      stop("Error: final event rate and CI results do not have the same number of rows")
    }
    
    return(final_output)
  }
  
  final_output <- calc_final_event_rate(all_event_rates,
                                                    all_xw,
                                                    global_coeff_vars)
  
  secondary_output <- calc_final_event_rate(secondary_event_rates,
                                            secondary_xw,
                                            global_coeff_vars_secondary,
                                            incl_unit_in_colname = F)
  
  write_csv(final_output, paste0(output_dir,"final_output.csv"))
  write_csv(final_output, paste0(output_dir,"secondary_output"))
  
} # Calculate final event rate for each hospital at each level of care for each outcome