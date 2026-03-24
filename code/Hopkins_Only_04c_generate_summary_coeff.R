# Sarah Goldfarb
# 03/04/2026

# TODO BEFORE RUNNING: define which sites have contributed
sites <- c("Hopkins")
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
  
  { # Load in local results
    
    { # Direct standardization results
      # Load in the direct standardization results for all sites, depending on file name
      # Strata can be "imc_cap" or "hosp"
      load_direct_standardization <- function(strata){
        
        # Initialize the direct_standardization dataframe
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
        
        # Loop through each site to load them
        for(site in sites){
          model_output <- model_output |>
            add_row(read_csv(paste0(project_location, site, 
                                    "_project_output/local_model_outputs/",
                                    site, "_direct_standardization_by_",
                                    strata, ".csv"), show_col_types = FALSE) |>
                      mutate(imc_capable = as.character(imc_capable)))
        }
        return(model_output)
      }
      
      direct_standardization_by_hosp <- load_direct_standardization("hosp")
      direct_standardization_by_imc_cap <- load_direct_standardization("imc_cap")
    } # Direct standardization results

    { # Hospital data
      # Load hospital data
      hospital_data <- data.frame(site=character(), first_hospital_id=character(), 
                                  n_patients=integer(), imc_capable=character(), 
                                  academic_community=character(), stringsAsFactors = FALSE)
      
      for(site in sites){
        hospital_data <- hospital_data |>
          add_row(read_csv(paste0(project_location, site, 
                                  "_project_output/", site, "_hospital_data.csv"), show_col_types = FALSE)|>
                    mutate(site=site, imc_capable = as.character(imc_capable)))
      }
    } # Hospital data
    
    { # Secondary analysis, imc vs icu direct standardization
      site_imc_capability <- hospital_data |> 
        group_by(site) |>
        summarise(any_imc_capable=any(imc_capable=="1", na.rm=TRUE))
      
      sites_with_imc <- site_imc_capability |>
        filter(any_imc_capable == TRUE) |>
        pull(site)
      
      hospials_with_imc <-  hospital_data |> 
        filter(imc_capable=="1") |>
        pull(first_hospital_id)
      
      # # Initialize the direct_standardization dataframe
      # imc_icu_direct_standardization <- data.frame(
      #   site = character(),
      #   hospital = character(),
      #   outcome=character(),
      #   term=character(),
      #   estimate=numeric(),
      #   std.error=numeric(),
      #   p.value=numeric(),
      #   n.obs=integer(),
      #   stringsAsFactors = FALSE
      # )
      # 
      # for(site in sites_with_imc){
      #   imc_icu_direct_standardization <- imc_icu_direct_standardization |>
      #     add_row(read_csv(paste0(project_location, site, 
      #                             "_project_output/local_model_outputs/",
      #                             site, "_imc_v_icu_direct_standardization.csv"), 
      #                      show_col_types = FALSE))
      # }
      
      
    } # Secondary analysis, imc vs icu direct standardization
    
  } # Load in local results
  
} # Setup

{ # Set up and run meta regression
  
  { # Restructure data for meta regression
    
    # Restructure the data from output
    # Need to send in which direct standardization output to apply this to 
    create_beta_list <- function(direct_standardization_data, strata, units){
      
      # Define strata column
      if(strata=="hosp"){
        direct_standardization_data <- direct_standardization_data |>
          mutate(strata_col = paste(site, hospital))
      }
      else{
        direct_standardization_data <- direct_standardization_data |>
          mutate(strata_col = paste(site, "IMC cap =",imc_capable))
      }
      
      # For each outcome-unit combo (e.g., icu death/hospice)
      # Need to create a matrix of coeffs where there is
      # a row for each hospital
      # a column for each covariate
      beta_list_all <- setNames(
        lapply(outcomes_binary, function(x) {
          setNames(vector("list", length(units)), units)
        }),
        outcomes_binary
      )
      
      beta_error_list_all <- setNames(
        lapply(outcomes_binary, function(x) {
          setNames(vector("list", length(units)), units)
        }),
        outcomes_binary
      )
      
      for(outcome_i in outcomes_binary){
        for(unit_i in units){
          
          beta_list_rows <- direct_standardization_data |>
            filter(outcome==outcome_i,
                   unit==unit_i) |>
            select(strata_col, term, estimate, std.error) |>
            filter(
              # Do not keep failed models
              !is.na(term),
              # Do not need intercept
              !str_detect(term, regex("Intercept")))
          
          # save estimates as matrix with dimensions (# strata, # covariates)
          beta_list_all[[outcome_i]][[unit_i]] <- beta_list_rows |>
            select(strata_col, term, estimate) |>
            pivot_wider(
              names_from=term,
              values_from=estimate,
              values_fill=NA # If not all strata have same covariates
            ) |>
            column_to_rownames("strata_col") |>
            as.matrix()
          
          # save standard errors as matrix with dimensions (# strata, # covariates)
          beta_error_list_all[[outcome_i]][[unit_i]] <- beta_list_rows |>
            select(strata_col, term, std.error) |>
            pivot_wider(
              names_from=term,
              values_from=std.error,
              values_fill=NA # If not all strata have same covariates
            ) |>
            column_to_rownames("strata_col") |>
            as.matrix()
          
        }
      }
      return(list(beta=beta_list_all, error=beta_error_list_all))
    }
    
    lists_by_hosp <- create_beta_list(direct_standardization_by_hosp, "hosp", units=units)
    lists_by_imc <- create_beta_list(direct_standardization_by_imc_cap, "imc_cap", units=units)
    
    imc_icu_lists <- create_beta_list(direct_standardization_by_hosp |>
                                        filter(hospital %in% hospials_with_imc,
                                               unit %in% c("icu", "stepdown")), "hosp", 
                                      units=c("icu", "stepdown"))
    
    # imc_icu_lists <- create_beta_list(imc_icu_direct_standardization |>
    #                                     mutate(unit="imc_and_icu"), "hosp", units=c("imc_and_icu"))
    
  } # Restructure data for meta regression
  
  { # Run meta regression
    run_meta_regression <- function(my_list, units){
      
      beta_list_all <- my_list$beta
      beta_error_list_all <- my_list$error
      
      summary_coeff_all <- list()
      
      # Iterate over each outcome (deathhospice, etc) and unit (icu, stepdown, ward)
      for(outcome_i in outcomes_binary){
        for(unit_i in units){
          beta_list <- beta_list_all[[outcome_i]][[unit_i]]
          beta_error_list <- beta_error_list_all[[outcome_i]][[unit_i]]
          
          summary_coeff <- c()
          
          # Iterate over each column in the beta list
          # This corresponds to each covariate
          for(i in c(1:dim(beta_list)[2])){
            meta.fit = rma.uni(na.omit(beta_list[,i]), na.omit(beta_error_list[,i])^2)
            summary_coeff[i] <- meta.fit[["beta"]][,1]
          }
          ### Added names here
          names(summary_coeff)<- colnames(beta_list)
          ###
          summary_coeff_all[[outcome_i]][[unit_i]] <- summary_coeff
        }
      }
      return(summary_coeff_all)
    }
    
    # Find way to order the outputted results so they are not just 21 numbers
    meta_hosp <- run_meta_regression(lists_by_hosp, units=units)
    meta_imc <- run_meta_regression(lists_by_imc, units= units)
    
    
  } # Run meta regression
  
  { # Run meta regression for icu vs imc
    # meta_imc_vs_icu <- run_meta_regression(imc_icu_lists, units=c("imc_and_icu"))
    run_meta_regression_imc_vs_icu <- function(my_list){
      
      beta_list_all <- my_list$beta
      beta_error_list_all <- my_list$error
      
      summary_coeff_all <- list()
      
      # Iterate over each outcome (deathhospice, etc)
      for(outcome_i in outcomes_binary){
          beta_list <- rbind(beta_list_all[[outcome_i]]$icu, beta_list_all[[outcome_i]]$stepdown)
          beta_error_list <- rbind(beta_error_list_all[[outcome_i]]$icu, beta_error_list_all[[outcome_i]]$stepdown)
          
          summary_coeff <- c()
          
          # Iterate over each column in the beta list
          # This corresponds to each covariate
          for(i in c(1:dim(beta_list)[2])){
            meta.fit = rma.uni(na.omit(beta_list[,i]), na.omit(beta_error_list[,i])^2)
            summary_coeff[i] <- meta.fit[["beta"]][,1]
          }
          ### Added names here
          names(summary_coeff)<- colnames(beta_list)
          ###
          summary_coeff_all[[outcome_i]] <- summary_coeff
      }
      return(summary_coeff_all)
    }
    
    meta_imc_vs_icu <- run_meta_regression_imc_vs_icu(imc_icu_lists)
    
    
  } # Run meta regression for icu vs imc
  
  { # Save global coeffs
    
    meta_hosp_df <- as.data.frame(meta_hosp)
    meta_imc_df <- as.data.frame(meta_imc)
    meta_imc_vs_icu_df <- as.data.frame(meta_imc_vs_icu)
    # meta_imc_vs_icu_df <- as.data.frame(do.call(cbind, lapply(meta_imc_vs_icu, `[[`, 1)))
    
    cov_names <- direct_standardization_by_hosp |> 
      filter(term!="(Intercept)") |> 
      select(term) |> 
      distinct()
    
    write_csv(meta_hosp_df, 
              paste0(output_dir,"global_coeff_by_hosp.csv"))
    write_csv(meta_imc_df, 
              paste0(output_dir,"global_coeff_by_imc_capable.csv"))
    write_csv(meta_imc_vs_icu_df, 
              paste0(output_dir,"global_coeff_imc_icu_together.csv"))
    write_csv(cov_names, 
              paste0(output_dir,"cov_names.csv"))
  } # Save global coeffs
  
} # Set up and run meta regression
