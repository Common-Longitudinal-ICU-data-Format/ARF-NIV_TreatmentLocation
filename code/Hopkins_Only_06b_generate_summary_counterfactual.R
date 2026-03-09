# Sarah Goldfarb
# 03/09/2026

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
  
  { # Load all event rates
    
    # Initialize the dataframe of event rates
    all_event_rates = data.frame(
      outcome=character(),
      unit=character(),
      local_hospital=character(),
      n=integer(),
      clif_hospital=character(),
      event_rate=numeric(),
      stringsAsFactors = FALSE
    )
    
    
    # Iterate over each site and read in the gamma values
    for(site in sites){
      all_event_rates <- all_event_rates |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_all_event_rates.csv"),
                         show_col_types = FALSE))
    }
    
  } # Load all event rates
  
} # Setup

{ # Calculate final event rate for each hospital at each level of care for each outcome
  
  final_event_rate_combined <- data.frame(
    outcome=character(),
    unit=character(),
    clif_hospital = character(),
    event_rate = numeric(),
    n_patients=integer(),
    contributing_hospitals=character(),
    stringsAsFactors = FALSE
  )
  
  for(outcome_i in outcomes_binary){
    for(unit_i in units){
      event_rate <- all_event_rates |>
        filter(outcome==outcome_i, unit==unit_i) |>
        select(-c(outcome, unit))
      
      final_event_rate <- event_rate |>
        group_by(clif_hospital) |>
        summarise(
          n = sum(n),
          events = sum(event_rate),
          contributing_hospitals = paste(sort(unique(local_hospital)), collapse = ", "),
          .groups = "drop"
        ) |>
        mutate(event_rate = events/n, outcome=outcome_i, unit=unit_i)
      
      final_event_rate_combined <- final_event_rate_combined |>
        add_row(final_event_rate |>
                  select(outcome, unit, clif_hospital, event_rate,
                         n_patients=n,
                         contributing_hospitals))
      
    }
  }
  
  write_csv(final_event_rate_combined, paste0(output_dir,"final_event_rate_combined.csv"))
  
} # Calculate final event rate for each hospital at each level of care for each outcome