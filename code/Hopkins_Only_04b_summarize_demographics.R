# Sarah Goldfarb
# 03/11/2026


# TODO BEFORE RUNNING: define which sites have contributed
sites <- c("Hopkins", "UCMC", "emory", "NU", "OHSU", 'UMN', "UCSF", "rush", "Michigan", "penn")
units <- c("icu", "ward", "stepdown")
# Ask about NU and ICU (should we have them re-run with only ICU patients)

TRIAGE_COLORS <- c(
  "ICU" = "red",
  "IMC" = "purple",
  "Ward" = "blue"
)

# No scientific notation
options(scipen = 999)

{ # Setup
  
  { # Load needed packages
    packages <- c(
      "tidyverse",
      "yaml",
      "rprojroot",
      "metafor",
      "patchwork"
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
  
  load_site_csv <- function(filename, site){
    return(read_csv(paste0(project_location, site, "_project_output/", 
                           site, "_", filename, ".csv"),
                    show_col_types = FALSE))
  }
  
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

{ # Load and output total cohort size
  
  raw_cohort_size <- data.frame(step=integer(), description=character(), 
                                n_encounters=integer(), n_patients=integer(),
                                site=character(),
                                stringsAsFactors = FALSE)
  
  for(site in sites){
    raw_cohort_size <- raw_cohort_size |>
      add_row(load_site_csv(filename="cohort_size", site=site) |> mutate(site=site))
  }
  
  all_cohort_size <- raw_cohort_size |>
    group_by(step, description) |>
    summarise(n_encounters_total = sum(n_encounters),
              n_patients_total = sum(n_patients),
              .groups="drop")
  
  write_csv(raw_cohort_size, paste0(output_dir, "cohort_size_raw.csv"))
  write_csv(all_cohort_size, paste0(output_dir, "cohort_size_all.csv"))
  
  rm(raw_cohort_size)
  
} # Load and output total cohort size

{ # Load and output table 1
  
  raw_table_1_3group_cat <- load_site_csv(site=sites[1], filename=paste0("tab_1_3group_catagorical"))
  raw_table_1_3group_cont <- load_site_csv(site=sites[1], filename=paste0("tab_1_3group_continuous"))
  
  raw_table_1_5group_cat <- load_site_csv(site=sites[1], filename=paste0("tab_1_5group_catagorical"))
  raw_table_1_5group_cont <- load_site_csv(site=sites[1], filename=paste0("tab_1_5group_continuous"))
  
  for(site in sites[-1]){
    if(site != "Michigan"){
      raw_table_1_3group_cat <- raw_table_1_3group_cat |>
        add_row(load_site_csv(site=site, filename=paste0("tab_1_3group_catagorical")))
    }else{        
      raw_table_1_3group_cat <- raw_table_1_3group_cat |>
        add_row(load_site_csv(site=site, filename=paste0("tab_1_3group_catagorical"))|>
                  mutate(
                    across(-any_of(c("site", "triage_location_formatted")), ~ {
                      x <- trimws(.x)              # handle stray spaces
                      x[x == "< 5"] <- "0" # replace string condition
                      suppressWarnings(as.numeric(x))  # coerce safely
                    })
                  )|>
                  select(-race_NA))
    }
    raw_table_1_3group_cont <- raw_table_1_3group_cont |>
      add_row(load_site_csv(site=site, filename=paste0("tab_1_3group_continuous")))
    
    if(site != "Michigan"){
    raw_table_1_5group_cat <- raw_table_1_5group_cat |>
      add_row(load_site_csv(site=site, filename=paste0("tab_1_5group_catagorical")))
    }else{
      raw_table_1_5group_cat <- raw_table_1_5group_cat |>
        add_row(load_site_csv(site=site, filename=paste0("tab_1_5group_catagorical"))|>
                  mutate(
                    across(-any_of(c("site", "traige_location_imc_avail")), ~ {
                      x <- trimws(.x)              # handle stray spaces
                      x[x == "< 5"] <- "0" # replace string condition
                      suppressWarnings(as.numeric(x))  # coerce safely
                    })
                  )|>
                  select(-race_NA))
    }
    raw_table_1_5group_cont <- raw_table_1_5group_cont |>
      add_row(load_site_csv(site=site, filename=paste0("tab_1_5group_continuous")))
  }
  
  
  continuous_vars <- c("age", "bmi", "elix", "ed_hours", 
                       "sf_value", "pf_value", "fio2", "sofa")
  
  pool_continuous <- function(data, var) {
    
    # Pull the count, mean, and sd vectors for the variable var
    n   <- data[[paste0("n_", var)]]
    m   <- data[[paste0("mean_", var)]]
    s   <- data[[paste0("sd_", var)]]
    
    # keep only rows where all 3 needed values are present
    keep <- !is.na(n) & !is.na(m) & !is.na(s)
    n <- n[keep]
    m <- m[keep]
    s <- s[keep]
    
    # Total sample size across rows (sites)
    total_n     <- sum(n)
    # Weighted (by n) pooled mean
    pooled_mean <- sum(n * m) / total_n
    
    # Cochran-style pooled variance that accounts for within- and between-group variability:
    #   sum( (n_i - 1)*s_i^2 + n_i*(m_i - pooled_mean)^2 ) / (N_total - 1)
    pooled_var  <- (sum((n - 1) * s^2 + n * (m - pooled_mean)^2)) / (total_n - 1)
    pooled_sd   <- sqrt(pooled_var)
    
    # Return a tibble with the variable column names for this variable
    tibble(
      !!paste0("n_", var)    := total_n,
      !!paste0("mean_", var) := pooled_mean,
      !!paste0("sd_", var)   := pooled_sd
    )
  }
  
  
  all_table_1_3group_cont <- raw_table_1_3group_cont |>
    select(-c(site)) |>
    group_by(triage_location_formatted) |>
    summarise(
      bind_cols(map(continuous_vars, ~ pool_continuous(pick(everything()), .x))),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -triage_location_formatted,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = triage_location_formatted,
      values_from = count
    )  %>%
    separate(
      variable,
      into = c("measure", "variable"),
      sep = "_",
      extra="merge"
    ) |>
    select(variable, measure, ICU, IMC, Ward) |>
    mutate(ICU=ifelse(measure=="n", as.character(round(ICU)), as.character(round(ICU, 2))),
           IMC=ifelse(measure=="n", as.character(round(IMC)), as.character(round(IMC, 2))),
           Ward=ifelse(measure=="n", as.character(round(Ward)), as.character(round(Ward, 2))))
  
  N_ICU_TOTAL <- as.integer(all_table_1_3group_cont|>filter(variable=="age", measure=="n")|>pull(ICU))
  N_IMC_TOTAL <- as.integer(all_table_1_3group_cont|>filter(variable=="age", measure=="n")|>pull(IMC))
  N_WARD_TOTAL <- as.integer(all_table_1_3group_cont|>filter(variable=="age", measure=="n")|>pull(Ward))
  
  all_table_1_3group_cont_display <- all_table_1_3group_cont |>
    pivot_wider(names_from = measure, values_from = c(ICU,IMC,Ward)) |>
    mutate(across(c(ICU_n, IMC_n, Ward_n), ~ as.integer(.x))) |>
    transmute(variable, 
              ICU=paste0(ICU_mean," ± ", ICU_sd), 
              IMC=paste0(IMC_mean," ± ", IMC_sd), 
              Ward=paste0(Ward_mean," ± ", Ward_sd), 
              ICU_missing=paste0(N_ICU_TOTAL-ICU_n, " (", 100-round(100*ICU_n/N_ICU_TOTAL),"%)"), 
              IMC_missing=paste0(N_IMC_TOTAL-IMC_n, " (", 100-round(100*IMC_n/N_IMC_TOTAL),"%)"),
              Ward_missing=paste0(N_WARD_TOTAL-Ward_n, " (", 100-round(100*Ward_n/N_WARD_TOTAL),"%)"))
  
  all_table_1_5group_cont <- raw_table_1_5group_cont |>
    select(-c(site)) |>
    group_by(traige_location_imc_avail) |>
    summarise(
      bind_cols(map(continuous_vars, ~ pool_continuous(pick(everything()), .x))),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -traige_location_imc_avail,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = traige_location_imc_avail,
      values_from = count
    )  %>%
    separate(
      variable,
      into = c("measure", "variable"),
      sep = "_",
      extra="merge"
    ) |>
    select(variable, measure, "ICU (+)", "ICU (-)", IMC, "Ward (+)", "Ward (-)") |>
    mutate(
      across(
        c("ICU (+)", "ICU (-)", IMC, "Ward (+)", "Ward (-)"),
        ~ ifelse(measure == "n",
                 as.character(round(.x)),
                 as.character(round(.x, 2)))
      )
    )
  
  N_ICU_PLUS_TOTAL <- as.integer(all_table_1_5group_cont|>filter(variable=="age", measure=="n")|>pull("ICU (+)"))
  N_ICU_MINUS_TOTAL <- as.integer(all_table_1_5group_cont|>filter(variable=="age", measure=="n")|>pull("ICU (-)"))
  N_WARD_PLUS_TOTAL <- as.integer(all_table_1_5group_cont|>filter(variable=="age", measure=="n")|>pull("Ward (+)"))
  N_WARD_MINUS_TOTAL <- as.integer(all_table_1_5group_cont|>filter(variable=="age", measure=="n")|>pull("Ward (-)"))
  
  all_table_1_5group_cont_display <- all_table_1_5group_cont |>
    pivot_wider(names_from = measure, values_from = c("ICU (+)", "ICU (-)",IMC,"Ward (+)", "Ward (-)")) |>
    mutate(across( c("ICU (+)_n", "ICU (-)_n",IMC_n,"Ward (+)_n", "Ward (-)_n"), ~ as.integer(.x))) |>
    transmute(variable, 
              `ICU (+)`=paste0(`ICU (+)_mean`," ± ", `ICU (+)_sd`), 
              `ICU (-)`=paste0(`ICU (-)_mean`," ± ", `ICU (-)_sd`), 
              IMC=paste0(IMC_mean," ± ", IMC_sd), 
              `Ward (+)`=paste0(`Ward (+)_mean`," ± ", `Ward (+)_sd`), 
              `Ward (-)`=paste0(`Ward (-)_mean`," ± ", `Ward (-)_sd`), 
              `ICU (+)_missing`=paste0(N_ICU_PLUS_TOTAL-`ICU (+)_n`, " (", 100-round(100*`ICU (+)_n`/N_ICU_PLUS_TOTAL),"%)"), 
              `ICU (-)_missing`=paste0(N_ICU_MINUS_TOTAL-`ICU (-)_n`, " (", 100-round(100*`ICU (-)_n`/N_ICU_MINUS_TOTAL),"%)"), 
              IMC_missing=paste0(N_IMC_TOTAL-IMC_n, " (", 100-round(100*IMC_n/N_IMC_TOTAL),"%)"),
              `Ward (+)_missing`=paste0(N_WARD_PLUS_TOTAL-`Ward (+)_n`, " (", 100-round(100*`Ward (+)_n`/N_WARD_PLUS_TOTAL),"%)"), 
              `Ward (-)_missing`=paste0(N_WARD_MINUS_TOTAL-`Ward (-)_n`, " (", 100-round(100*`Ward (-)_n`/N_WARD_MINUS_TOTAL),"%)"))
  
  
  all_table_1_3group_cat <- raw_table_1_3group_cat |>
    select(-c(site)) |>
    group_by(triage_location_formatted) |>
    # Take the sum of each col for each group
    summarise(across(everything(),~sum(.x, na.rm=T)), .groups="drop") %>%
    pivot_longer(
      cols = -triage_location_formatted,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = triage_location_formatted,
      values_from = count
    ) %>%
    separate(
      variable,
      into = c("variable", "measure"),
      sep = "_(?=[^_]+$)"
    ) 
  
  all_table_1_3group_cat_display <- all_table_1_3group_cat |>
    mutate(
      ICU=paste0(ICU, " (", round(100*as.numeric(ICU)/N_ICU_TOTAL), "%)"),
      IMC=paste0(IMC, " (", round(100*as.numeric(IMC)/N_IMC_TOTAL), "%)"),
      Ward=paste0(Ward, " (", round(100*as.numeric(Ward)/N_WARD_TOTAL), "%)")
      )
  
  # a <- all_table_1_3group_cat|>
  #   group_by(variable) |>
  #   summarise(
  #     ICU = sum(ICU, na.rm = TRUE),
  #     IMC = sum(IMC, na.rm = TRUE),
  #     Ward = sum(Ward, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   mutate(measure = "n_measured") %>%
  #   select(variable, measure, ICU, IMC, Ward)

  all_table_1_5group_cat <- raw_table_1_5group_cat |>
    select(-c(site)) |>
    group_by(traige_location_imc_avail) |>
    # Take the sum of each col for each group
    summarise(across(everything(),~sum(.x, na.rm=T)), .groups="drop") %>%
    pivot_longer(
      cols = -traige_location_imc_avail,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = traige_location_imc_avail,
      values_from = count
    ) %>%
    separate(
      variable,
      into = c("variable", "measure"),
      sep = "_(?=[^_]+$)"
    )
  
  all_table_1_5group_cat_display <- all_table_1_5group_cat |>
    mutate(
      "ICU (+)"=paste0(`ICU (+)`, " (", round(100*as.numeric(`ICU (+)`)/N_ICU_PLUS_TOTAL), "%)"),
      "ICU (-)"=paste0(`ICU (-)`, " (", round(100*as.numeric(`ICU (-)`)/N_ICU_MINUS_TOTAL), "%)"),
      IMC=paste0(IMC, " (", round(100*as.numeric(IMC)/N_IMC_TOTAL), "%)"),
      "Ward (+)"=paste0(`Ward (+)`, " (", round(100*as.numeric(`Ward (+)`)/N_WARD_PLUS_TOTAL), "%)"),
      "Ward (-)"=paste0(`Ward (-)`, " (", round(100*as.numeric(`Ward (-)`)/N_WARD_MINUS_TOTAL), "%)")
    )
  
  
  all_table_1_3group <- rbind(all_table_1_3group_cont_display,
                              all_table_1_3group_cat_display|>
                                mutate(variable=paste0(variable, " = ", measure),
                                       ICU_missing=NA, IMC_missing=NA, Ward_missing=NA) |>
                                select(-measure)) |>
    mutate(across(c(ICU_missing, IMC_missing, Ward_missing), ~ifelse(.x=="0 (0%)", NA, .x)))
  
  all_table_1_5group <- rbind(all_table_1_5group_cont_display,
                              all_table_1_5group_cat_display|>
                                mutate(variable=paste0(variable, " = ", measure),
                                       `ICU (+)_missing`=NA,`ICU (-)_missing`=NA, IMC_missing=NA, 
                                       `Ward (+)_missing`=NA,`Ward (-)_missing`=NA) |>
                                select(-measure)) |>
    mutate(across(c(`ICU (+)_missing`, `ICU (-)_missing`, IMC_missing, `Ward (+)_missing`,`Ward (-)_missing`), ~ifelse(.x=="0 (0%)", NA, .x)))

  
  write_csv(all_table_1_3group, paste0(output_dir, "table_1_3group.csv"))
  write_csv(all_table_1_5group, paste0(output_dir, "table_1_5group.csv"))
  
  
  rm(raw_table_1_3group_cat, raw_table_1_3group_cont, # raw_table_1_5group_cat,  # Keep this one because we use it later for hospital information
     raw_table_1_5group_cont,
     all_table_1_3group_cat, all_table_1_3group_cat_display, all_table_1_3group_cont, all_table_1_3group_cont_display,
     all_table_1_5group_cat, all_table_1_5group_cat_display, all_table_1_5group_cont, all_table_1_5group_cont_display)
} # Load and output table 1

{ # Additional hospital information for table 1
  hosp_info <- load_site_csv(site=sites[1], filename=paste0("hospital_data")) |> mutate(site=sites[1])
  
  for(site in sites[-1]){
    hosp_info <- hosp_info |>
      add_row(load_site_csv(site=site, filename=paste0("hospital_data")) |>
                mutate(first_hospital_id = as.character (first_hospital_id)), site=site)
  }
  
  table_1_hospital_info <- hosp_info |>
    group_by(imc_capable) |>
    summarise(n_hosp = n(), 
              n_hosp_academic=sum(ifelse(academic_community=="academic", 1, 0)),
              percent_hosp_academic = round(100*n_hosp_academic/n()),
              percent_patients_academic = round(100*sum(ifelse(academic_community=="academic", 1, 0)*n_patients)/sum(n_patients)))
  
  table_1_academic_comm_by_pt <- raw_table_1_5group_cont |>
    select(site, traige_location_imc_avail, n_patients_site_this_unit=n_age) |>
    mutate(imc_capable = case_when(
      traige_location_imc_avail == "IMC" ~ 1,
      traige_location_imc_avail == "ICU (+)" ~ 1,
      traige_location_imc_avail == "Ward (+)" ~ 1,
      TRUE ~ 0
      
    )) |>
    left_join(hosp_info |> select(site, first_hospital_id, academic_community, imc_capable, n_patients_hosp_all_units=n_patients), by=c("site", "imc_capable")) 
  
  # n row of this will be number of imc capable hospitails contributing imc data (since some imc capable == 0 hospitals only have icu data)
  n_hosp_imc <- table_1_academic_comm_by_pt |>
    filter(traige_location_imc_avail == "IMC")
  
  academic_comm_imc <- n_hosp_imc |>
    summarize(
      n_hosp_academic=sum(ifelse(academic_community=="academic", 1, 0)),
      percent_hosp_academic = round(100*n_hosp_academic/n())
    )
  
  
  
} # Additional hospital information for table 1

write_csv(data.frame(
  N_ICU=N_ICU_TOTAL,
  N_IMC=N_IMC_TOTAL,
  N_Ward=N_WARD_TOTAL,
  N_ICU_plus=N_ICU_PLUS_TOTAL,
  N_ICU_minus=N_ICU_MINUS_TOTAL,
  N_Ward_plus=N_WARD_PLUS_TOTAL,
  N_Ward_minus=N_WARD_MINUS_TOTAL,
  stringsAsFactors = F
), paste0(output_dir, "n_tot.csv"))

{ # Load and output table 2
  raw_table_2_3group_cat <- load_site_csv(site=sites[1], filename=paste0("tab_2_3group_catagorical"))
  raw_table_2_3group_cont <- load_site_csv(site=sites[1], filename=paste0("tab_2_3group_continuous"))
  
  raw_table_2_5group_cat <- load_site_csv(site=sites[1], filename=paste0("tab_2_5group_catagorical"))
  raw_table_2_5group_cont <- load_site_csv(site=sites[1], filename=paste0("tab_2_5group_continuous"))
  
  for(site in sites[-1]){
    raw_table_2_3group_cat <- raw_table_2_3group_cat |>
      add_row(load_site_csv(site=site, filename=paste0("tab_2_3group_catagorical")))
    raw_table_2_3group_cont <- raw_table_2_3group_cont |>
      add_row(load_site_csv(site=site, filename=paste0("tab_2_3group_continuous")))
    
    raw_table_2_5group_cat <- raw_table_2_5group_cat |>
      add_row(load_site_csv(site=site, filename=paste0("tab_2_5group_catagorical")))
    raw_table_2_5group_cont <- raw_table_2_5group_cont |>
      add_row(load_site_csv(site=site, filename=paste0("tab_2_5group_continuous")))
  }
  
  all_table_2_3group_cont <- raw_table_2_3group_cont |>
    select(-c(site)) |>
    group_by(triage_location_formatted) |>
    summarise(
      bind_cols(map(c("los", "days_off"), ~ pool_continuous(pick(everything()), .x))),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -triage_location_formatted,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = triage_location_formatted,
      values_from = count
    )  %>%
    separate(
      variable,
      into = c("measure", "variable"),
      sep = "_",
      extra="merge"
    ) |>
    select(variable, measure, ICU, IMC, Ward) |>
    mutate(ICU=ifelse(measure=="n", as.character(round(ICU)), as.character(round(ICU, 2))),
           IMC=ifelse(measure=="n", as.character(round(IMC)), as.character(round(IMC, 2))),
           Ward=ifelse(measure=="n", as.character(round(Ward)), as.character(round(Ward, 2))))
  
  all_table_2_3group_cont_display <- all_table_2_3group_cont |>
    pivot_wider(names_from = measure, values_from = c(ICU,IMC,Ward)) |>
    mutate(across(c(ICU_n, IMC_n, Ward_n), ~ as.integer(.x))) |>
    transmute(variable, 
              ICU=paste0(ICU_mean," ± ", ICU_sd), 
              IMC=paste0(IMC_mean," ± ", IMC_sd), 
              Ward=paste0(Ward_mean," ± ", Ward_sd), 
              ICU_missing=paste0(N_ICU_TOTAL-ICU_n, " (", 100-round(100*ICU_n/N_ICU_TOTAL),"%)"), 
              IMC_missing=paste0(N_IMC_TOTAL-IMC_n, " (", 100-round(100*IMC_n/N_IMC_TOTAL),"%)"),
              Ward_missing=paste0(N_WARD_TOTAL-Ward_n, " (", 100-round(100*Ward_n/N_WARD_TOTAL),"%)")) |>
    mutate(variable=case_when(
      variable == "los" ~ "Length of stay",
      variable == "days_off" ~ "Respiratory support-free days",
      TRUE ~ variable
    ))
  
  all_table_2_5group_cont <- raw_table_2_5group_cont |>
    select(-c(site)) |>
    group_by(traige_location_imc_avail) |>
    summarise(
      bind_cols(map(c("los", "days_off"), ~ pool_continuous(pick(everything()), .x))),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -traige_location_imc_avail,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = traige_location_imc_avail,
      values_from = count
    )  %>%
    separate(
      variable,
      into = c("measure", "variable"),
      sep = "_",
      extra="merge"
    ) |>
    select(variable, measure, "ICU (+)", "ICU (-)", IMC, "Ward (+)", "Ward (-)") |>
    mutate(
      across(
        c("ICU (+)", "ICU (-)", IMC, "Ward (+)", "Ward (-)"),
        ~ ifelse(measure == "n",
                 as.character(round(.x)),
                 as.character(round(.x, 2)))
      )
    )
  
  all_table_2_5group_cont_display <- all_table_2_5group_cont |>
    pivot_wider(names_from = measure, values_from = c("ICU (+)", "ICU (-)",IMC,"Ward (+)", "Ward (-)")) |>
    mutate(across( c("ICU (+)_n", "ICU (-)_n",IMC_n,"Ward (+)_n", "Ward (-)_n"), ~ as.integer(.x))) |>
    transmute(variable, 
              `ICU (+)`=paste0(`ICU (+)_mean`," ± ", `ICU (+)_sd`), 
              `ICU (-)`=paste0(`ICU (-)_mean`," ± ", `ICU (-)_sd`), 
              IMC=paste0(IMC_mean," ± ", IMC_sd), 
              `Ward (+)`=paste0(`Ward (+)_mean`," ± ", `Ward (+)_sd`), 
              `Ward (-)`=paste0(`Ward (-)_mean`," ± ", `Ward (-)_sd`), 
              `ICU (+)_missing`=paste0(N_ICU_PLUS_TOTAL-`ICU (+)_n`, " (", 100-round(100*`ICU (+)_n`/N_ICU_PLUS_TOTAL),"%)"), 
              `ICU (-)_missing`=paste0(N_ICU_MINUS_TOTAL-`ICU (-)_n`, " (", 100-round(100*`ICU (-)_n`/N_ICU_MINUS_TOTAL),"%)"), 
              IMC_missing=paste0(N_IMC_TOTAL-IMC_n, " (", 100-round(100*IMC_n/N_IMC_TOTAL),"%)"),
              `Ward (+)_missing`=paste0(N_WARD_PLUS_TOTAL-`Ward (+)_n`, " (", 100-round(100*`Ward (+)_n`/N_WARD_PLUS_TOTAL),"%)"), 
              `Ward (-)_missing`=paste0(N_WARD_MINUS_TOTAL-`Ward (-)_n`, " (", 100-round(100*`Ward (-)_n`/N_WARD_MINUS_TOTAL),"%)")) |>
    mutate(variable=case_when(
      variable == "los" ~ "Length of stay",
      variable == "days_off" ~ "Respiratory support-free days",
      TRUE ~ variable
    ))
  
  
  all_table_2_3group_cat <- raw_table_2_3group_cat |>
    select(-c(site)) |>
    group_by(triage_location_formatted) |>
    # Take the sum of each col for each group
    summarise(across(everything(),~sum(.x, na.rm=T)), .groups="drop") %>%
    pivot_longer(
      cols = -triage_location_formatted,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = triage_location_formatted,
      values_from = count
    ) %>%
    separate(
      variable,
      into = c("variable", "measure"),
      sep = "_(?=[^_]+$)"
    ) 
  
  all_table_2_3group_cat_display <- all_table_2_3group_cat |>
    mutate(
      ICU=ifelse(variable != "imv" | measure == "NA",
                 paste0(ICU, " (", round(100*as.numeric(ICU)/N_ICU_TOTAL), "%)"),
                 paste0(ICU, " (", round(100*as.numeric(ICU)/(N_ICU_TOTAL-ICU[measure == "NA"])), "%)")
                 ),
      IMC=ifelse(variable != "imv" | measure=="NA",
                 paste0(IMC, " (", round(100*as.numeric(IMC)/N_IMC_TOTAL), "%)"),
                 paste0(IMC, " (", round(100*as.numeric(IMC)/(N_IMC_TOTAL-IMC[measure == "NA"])), "%)")
                 ),
      Ward=ifelse(variable != "imv" | measure=="NA",
        paste0(Ward, " (", round(100*as.numeric(Ward)/N_WARD_TOTAL), "%)"),
        paste0(Ward, " (", round(100*as.numeric(Ward)/(N_WARD_TOTAL-Ward[measure == "NA"])), "%)")
      )
    ) |>
    # Remove binary "nos"
    filter(measure=="1" | measure == "NA")|>
    # Rename for table
    mutate(
      variable=case_when(
        variable=="dh" ~ "Death/hospice at any time",
        variable=="dh_28" ~ "Death/hospice by 28 days",
        variable=="dh_60" ~ "Death/hospice by 60 days",
        variable=="imv" &  measure!="NA" ~ "Escalated to IMV",
        measure=="NA" ~ "     Not included, code status DNI",
        variable=="was_escalated" ~ "Escalated to higher acuity unit",
        variable=="organ_failure" ~ "Escalated to organ failure",
        TRUE ~ variable
      )
    ) |>
    select(-measure)
  
  all_table_2_5group_cat <- raw_table_2_5group_cat |>
    select(-c(site)) |>
    group_by(traige_location_imc_avail) |>
    # Take the sum of each col for each group
    summarise(across(everything(),~sum(.x, na.rm=T)), .groups="drop") %>%
    pivot_longer(
      cols = -traige_location_imc_avail,
      names_to = "variable",
      values_to = "count"
    ) %>%
    pivot_wider(
      names_from = traige_location_imc_avail,
      values_from = count
    ) %>%
    separate(
      variable,
      into = c("variable", "measure"),
      sep = "_(?=[^_]+$)"
    ) 
  
  
  # all_table_2_5group_cat_display <- all_table_2_5group_cat |>
  #   mutate(
  #     "ICU (+)"=paste0(`ICU (+)`, " (", round(100*as.numeric(`ICU (+)`)/N_ICU_PLUS_TOTAL), "%)"),
  #     "ICU (-)"=paste0(`ICU (-)`, " (", round(100*as.numeric(`ICU (-)`)/N_ICU_MINUS_TOTAL), "%)"),
  #     IMC=paste0(IMC, " (", round(100*as.numeric(IMC)/N_IMC_TOTAL), "%)"),
  #     "Ward (+)"=paste0(`Ward (+)`, " (", round(100*as.numeric(`Ward (+)`)/N_WARD_PLUS_TOTAL), "%)"),
  #     "Ward (-)"=paste0(`Ward (-)`, " (", round(100*as.numeric(`Ward (-)`)/N_WARD_MINUS_TOTAL), "%)")
  #   )
  
  all_table_2_5group_cat_display <- all_table_2_5group_cat |>
    mutate(
      "ICU (+)"=ifelse(variable != "imv" | measure == "NA",
                 paste0(`ICU (+)`, " (", round(100*as.numeric(`ICU (+)`)/N_ICU_PLUS_TOTAL), "%)"),
                 paste0("[no DNI] ",`ICU (+)`, " (", round(100*as.numeric(`ICU (+)`)/(N_ICU_PLUS_TOTAL-`ICU (+)`[measure == "NA"])), "%)")
      ),
      "ICU (-)"=ifelse(variable != "imv" | measure == "NA",
                       paste0(`ICU (-)`, " (", round(100*as.numeric(`ICU (-)`)/N_ICU_MINUS_TOTAL), "%)"),
                       paste0("[no DNI] ", `ICU (-)`, " (", round(100*as.numeric(`ICU (-)`)/(N_ICU_MINUS_TOTAL-`ICU (-)`[measure == "NA"])), "%)")
      ),
      IMC=ifelse(variable != "imv" | measure=="NA",
                 paste0(IMC, " (", round(100*as.numeric(IMC)/N_IMC_TOTAL), "%)"),
                 paste0("[no DNI] ",IMC, " (", round(100*as.numeric(IMC)/(N_IMC_TOTAL-IMC[measure == "NA"])), "%)")
      ),
      "Ward (+)"=ifelse(variable != "imv" | measure == "NA",
                       paste0(`Ward (+)`, " (", round(100*as.numeric(`Ward (+)`)/N_WARD_PLUS_TOTAL), "%)"),
                       paste0("[no DNI] ", `Ward (+)`, " (", round(100*as.numeric(`Ward (+)`)/(N_WARD_PLUS_TOTAL-`Ward (+)`[measure == "NA"])), "%)")
      ),
      "Ward (-)"=ifelse(variable != "imv" | measure == "NA",
                       paste0(`Ward (-)`, " (", round(100*as.numeric(`Ward (-)`)/N_WARD_MINUS_TOTAL), "%)"),
                       paste0("[no DNI] ",`Ward (-)`, " (", round(100*as.numeric(`Ward (-)`)/(N_WARD_MINUS_TOTAL-`Ward (-)`[measure == "NA"])), "%)")
      )
    ) |>
    # Remove binary "nos"
    filter(measure=="1" | measure == "NA")|>
    # Rename for table
    mutate(
      variable=case_when(
        variable=="dh" ~ "Death/hospice at any time",
        variable=="dh_28" ~ "Death/hospice by 28 days",
        variable=="dh_60" ~ "Death/hospice by 60 days",
        variable=="imv" &  measure!="NA" ~ "Escalated to IMV",
        measure=="NA" ~ "     Not included, code status DNI",
        variable=="was_escalated" ~ "Escalated to higher acuity unit",
        variable=="organ_failure" ~ "Escalated to organ failure",
        TRUE ~ variable
      )
    ) |>
    select(-measure)
  
  all_table_2_3group <- rbind(all_table_2_3group_cont_display,
                              all_table_2_3group_cat_display|>
                                mutate(ICU_missing=NA, IMC_missing=NA, Ward_missing=NA)) |>
    mutate(across(c(ICU_missing, IMC_missing, Ward_missing), ~ifelse(.x=="0 (0%)", NA, .x)))
  
  all_table_2_5group <- rbind(all_table_2_5group_cont_display,
                              all_table_2_5group_cat_display|>
                                mutate(`ICU (+)_missing`=NA,`ICU (-)_missing`=NA, IMC_missing=NA, 
                                       `Ward (+)_missing`=NA,`Ward (-)_missing`=NA)) |>
    mutate(across(c(`ICU (+)_missing`, `ICU (-)_missing`, IMC_missing, `Ward (+)_missing`,`Ward (-)_missing`), ~ifelse(.x=="0 (0%)", NA, .x)))
  
  
  write_csv(all_table_2_3group, paste0(output_dir, "table_2_3group.csv"))
  write_csv(all_table_2_5group, paste0(output_dir, "table_2_5group.csv"))
  
  # Statistical tests
  
  #
  # For binary outcomes
  run_chisq <- function(df, var_name){
    tab <- df |>
      filter(variable==var_name, measure %in% c("0", "1")) |>
      pivot_longer(cols=-c(variable, measure),
                   names_to = "group",
                   values_to = "count") |>
      pivot_wider(names_from=group, values_from=count)|>
      select(-variable)|>
      column_to_rownames("measure")|>
      mutate(across(everything(), ~parse_number(as.character(.))))|>
      as.matrix()

    cat("\n\n############################################\n")
    cat(paste0("                  ", var_name,"                  \n\n"))
    print(tab)
    return(chisq.test(tab))
  }
  
  run_chisq(all_table_2_5group_cat, "dh")
  run_chisq(all_table_2_5group_cat, "organ_failure")
  run_chisq(all_table_2_5group_cat |> select(-c(`ICU (+)`, `ICU (-)`)), "was_escalated")
  
  # for continuous
  run_anova <-  function(df, var_name){
    tab <- df |>
      filter(variable==var_name) |>
      select(-variable)|>
      pivot_longer(cols=-measure,
                   names_to = "group",
                   values_to = "value") |>
      mutate(value=as.numeric(value)) |>
      pivot_wider(names_from=measure, values_from=value)
    
    print(tab)
    
    k <- nrow(tab)
    N <- sum(tab$n)
    
    grand_mean <- sum(tab$n * tab$mean) / N
    
    ss_between <- sum(tab$n * (tab$mean - grand_mean)^2)
    ss_within <- sum((tab$n - 1) * tab$sd^2)
    
    df_between <- k - 1
    df_within <- N - k
    
    ms_between <- ss_between / df_between
    ms_within <- ss_within / df_within
    
    F_stat <- ms_between / ms_within
    p_value <- pf(F_stat, df_between, df_within, lower.tail = FALSE)
    
    tibble::tibble(
      variable = var_name,
      F = F_stat,
      df_between = df_between,
      df_within = df_within,
      p_value = p_value
    )
    
  }
  
  run_anova(all_table_2_5group_cont, "los")  
  run_anova(all_table_2_5group_cont, "days_off")
  
  #### Pairwise comparisons
  
  # Contunous
  # pull n / mean / sd for one variable & one group column
  get_cont <- function(df, var, group_col) {
    rows <- df[df$variable == var, ]
    list(
      n    = as.numeric(rows[rows$measure == "n",    group_col]),
      mean = as.numeric(rows[rows$measure == "mean", group_col]),
      sd   = as.numeric(rows[rows$measure == "sd",   group_col])
    )
  }
  
  # Welch t-test + Cohen's d, all from summary stats
  welch_from_summary <- function(n1, m1, s1, n2, m2, s2) {
    se <- sqrt(s1^2/n1 + s2^2/n2)
    t  <- (m1 - m2) / se
    df <- (s1^2/n1 + s2^2/n2)^2 /
      ((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))
    p  <- 2 * pt(-abs(t), df)
    sp <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))  # pooled SD
    data.frame(mean_pos = m1, mean_neg = m2, diff = m1 - m2,
               t = t, df = df, p_value = p, cohens_d = (m1 - m2)/sp)
  }
  
  compare_cont_stats <- function(df, var, gpos, gneg) {
    a <- get_cont(df, var, gpos); b <- get_cont(df, var, gneg)
    cbind(variable = var, comparison = paste(gpos, "vs", gneg),
          welch_from_summary(a$n, a$mean, a$sd, b$n, b$mean, b$sd))
  }
  
  cont_results_stats <- bind_rows(
    compare_cont_stats(all_table_2_5group_cont, "los",      "ICU (+)",  "ICU (-)"),
    compare_cont_stats(all_table_2_5group_cont, "los",      "Ward (+)", "Ward (-)"),
    compare_cont_stats(all_table_2_5group_cont, "days_off", "ICU (+)",  "ICU (-)"),
    compare_cont_stats(all_table_2_5group_cont, "days_off", "Ward (+)", "Ward (-)")
  )
  
  # Categorical
  compare_cat_stats <- function(df, var, gpos, gneg, test = c("chisq", "fisher")) {
    test <- match.arg(test)
    rows <- df[df$variable == var, ]
    rows$measure <- as.character(rows$measure)   # handles 0/1 stored as num or chr
    
    pos0 <- as.numeric(rows[rows$measure == "0", gpos]); pos1 <- as.numeric(rows[rows$measure == "1", gpos])
    neg0 <- as.numeric(rows[rows$measure == "0", gneg]); neg1 <- as.numeric(rows[rows$measure == "1", gneg])
    
    m <- matrix(c(pos0, pos1, neg0, neg1), nrow = 2,
                dimnames = list(outcome = c("0", "1"), group = c(gpos, gneg)))
    
    ht   <- if (test == "chisq") chisq.test(m, correct = FALSE) else fisher.test(m)
    stat <- if (test == "chisq") unname(ht$statistic) else NA_real_
    
    data.frame(
      variable   = var,
      comparison = paste(gpos, "vs", gneg),
      prop_pos   = pos1 / (pos0 + pos1),      # event rate in + group
      prop_neg   = neg1 / (neg0 + neg1),      # event rate in - group
      odds_ratio = (pos1/pos0) / (neg1/neg0), # odds of event, + vs -
      statistic  = stat,
      p_value    = ht$p.value,
      test       = test
    )
  }
  
  cat_results_stats <- bind_rows(
    compare_cat_stats(all_table_2_5group_cat, "dh",            "ICU (+)",  "ICU (-)"),
    compare_cat_stats(all_table_2_5group_cat, "dh",            "Ward (+)", "Ward (-)"),
    compare_cat_stats(all_table_2_5group_cat, "organ_failure", "ICU (+)",  "ICU (-)"),
    compare_cat_stats(all_table_2_5group_cat, "organ_failure", "Ward (+)", "Ward (-)"),
    compare_cat_stats(all_table_2_5group_cat, "was_escalated", "Ward (+)", "Ward (-)")  # ward only
  )
  
  
  # Clean space
  rm(raw_table_2_3group_cat, raw_table_2_3group_cont, raw_table_2_5group_cat, raw_table_2_5group_cont,
     all_table_2_3group_cat, all_table_2_3group_cat_display, all_table_2_3group_cont, all_table_2_3group_cont_display,
     all_table_2_5group_cat, all_table_2_5group_cat_display, all_table_2_5group_cont, all_table_2_5group_cont_display)
  
} # Load and output table 2

{ # Figure 1a and b
  fig_1a_data <- load_site_csv(site=sites[1], filename=paste0("fig_1a_data")) |>
    mutate(site=sites[1]) |>
    # Old version of imc capable remove
    select(-imc_capable)
  
  hosp_data <- load_site_csv(site=sites[1], filename=paste0("hospital_data"))
  
  for(site in sites[-1]){
    fig_1a_data <- fig_1a_data |>
      add_row(load_site_csv(site=site, filename=paste0("fig_1a_data")) |> 
                mutate(hospital=as.character(hospital),
                       site=site)|>
                # Old version of imc capable remove
                select(-imc_capable))
    
    hosp_data <- hosp_data |>
      add_row(load_site_csv(site=site, filename=paste0("hospital_data"))|>
                mutate(first_hospital_id=as.character(first_hospital_id)))
  }
  
  fig_1a_data <- fig_1a_data |>
    left_join(hosp_data|>select(hospital=first_hospital_id, imc_capable), 
              by="hospital")
  
  n_hosp <- nrow(fig_1a_data)
  n_site <- length(unique(fig_1a_data |> pull(site)))
  
  fig_1a_data <- fig_1a_data |>
    mutate(x_axis_label = 
             ifelse(site %in% c("NU", "UCSF"), 
                    # In NU or UCSF
                    ifelse(imc_capable==1,
                           # IMC capable
                           "*",
                           # IMC incapable
                           "†"), 
                    # Not in NU or UCSF
                    ""))
  
  hosp_info_tab_long <- fig_1a_data |>
    select(-site) |>
    arrange(-pct_ICU, desc(x_axis_label), n_total) |>
    select(hospital, x_axis_label, ICU, IMC, Ward, n_total) |>
    pivot_longer(
      cols = c(ICU, IMC, Ward),
      names_to = "unit",
      values_to = "n"
    ) |>
    mutate(unit = factor(unit, levels = c("ICU", "IMC", "Ward")),
           pct = n/n_total
    ) 
  
  
  
  make_fig_1_plot <- function(plot_data, y_val="pct", fill="unit"){
    if(y_val=="pct"){
      plot_data <- plot_data |>
        mutate(y_lab_max = 1.03)
      y_lab_text <- "Percent of Admissions"
    }else{      
      plot_data <- plot_data |>
        mutate(y_lab_max = n_total*1.03)
      if(fill=="unit"){
        y_lab_text <- "Number of Admissions"
      }
      else{
        y_lab_text <- "n"
      }
    }
    
    hosp_info_plot <- ggplot(plot_data, aes(x = factor(hospital, levels = unique(hospital)),
                                                     y = .data[[y_val]],
                                                     fill = .data[[fill]])) +
      geom_bar(stat = "identity", width = 0.9) +
      labs(
        x = "Hospital",
        y = y_lab_text,
        fill = "Unit"
      ) +
      theme_classic() +
      theme(
        axis.title.x=element_text(size = 16),
        axis.title.y=element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),   # hides hospital names
        axis.ticks.x = element_blank()
      ) 
    
    if(fill=="unit"){
      hosp_info_plot <- hosp_info_plot +
        scale_fill_manual(values = TRIAGE_COLORS,
                          name=expression(bold("Admission Unit:"))) +
        theme(
          legend.position = "bottom",
          legend.direction="horizontal",
          legend.title = element_text(face = "bold")
        )+
        geom_text(aes(x=hospital, y=y_lab_max, label=x_axis_label))
    }else{
      hosp_info_plot <- hosp_info_plot +
        geom_col(fill = "grey4")+
        theme(
          legend.position = "none"
        )
    }
    
    if(y_val=="pct"){
      hosp_info_plot <- hosp_info_plot +
        scale_y_continuous(labels = scales::percent_format()) 
    }
    
    return(hosp_info_plot)
  }
  
  fig_1a <- make_fig_1_plot(hosp_info_tab_long)
  fig_1a
  
  hosp_hist_order <- unique(hosp_info_tab_long|>
    pull(hospital))
  
  fig_1a_extra <- make_fig_1_plot(hosp_info_tab_long|>
                                    group_by(hospital)|>
                                    summarize(
                                      x_axis_label=first(x_axis_label),
                                      n_total=first(n_total),
                                      All="All",
                                      n=sum(n),
                                      pct=1,
                                      .groups = "drop"
                                      ) |>
                                    arrange(match(hospital, hosp_hist_order)),
                                  y_val="n",
                                  fill="All"
                                  )
  #fig_1a_extra
  fig_1a/fig_1a_extra+plot_layout(heights=c(3.5,2.5))
  
  fig_1b <- make_fig_1_plot(hosp_info_tab_long, y_val="n")
  fig_1b
  
  ### Save as jpg to view ###
  ggsave(paste0(output_dir, "fig_1a.jpg"), 
         plot=fig_1a, width=8, height=5, dpi=600)
  ggsave(paste0(output_dir, "fig_1a_with_counts.jpg"), 
         plot=(fig_1a/fig_1a_extra+plot_layout(heights=c(3.5,2.5))), width=8, height=7, dpi=600)
  ggsave(paste0(output_dir, "fig_1b.jpg"), 
         plot=fig_1b, width=8, height=5, dpi=600)
  
} # Figure 1a and b

{ # Figure 2
  fig_2a_data <- load_site_csv(site=sites[1], filename=paste0("fig_2a_data")) |>
    mutate(site=sites[1]) |>
    select(site, time_bin, triage_location_formatted, n_patients)
  fig_2b_data <- load_site_csv(site=sites[1], filename=paste0("fig_2b_data")) |>
    mutate(site=sites[1]) |>
    select(site, time_bin, last_niv_device, n_patients)
  fig_2c_data <- load_site_csv(site=sites[1], filename=paste0("fig_2c_data")) |>
    mutate(site=sites[1]) |>
    select(site, time_bin, physiologic_type, n_patients)
  
  for(site in sites[-1]){
    fig_2a_data <- fig_2a_data |>
      add_row(load_site_csv(site=site, filename=paste0("fig_2a_data")) |> 
                mutate(site=site,
                       n_patients=as.character(n_patients))|>
                select(site, time_bin, triage_location_formatted, n_patients))
    fig_2b_data <- fig_2b_data |>
      add_row(load_site_csv(site=site, filename=paste0("fig_2b_data")) |> 
                mutate(site=site,
                       n_patients=as.character(n_patients))|>
                select(site, time_bin, last_niv_device, n_patients))
    fig_2c_data <- fig_2c_data |>
      add_row(load_site_csv(site=site, filename=paste0("fig_2c_data")) |> 
                mutate(site=site,
                       n_patients=as.character(n_patients))|>
                select(site, time_bin, physiologic_type, n_patients))
  }
  
  time_data <- fig_2a_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(site, time_bin) |>
    summarise(
      total_time_bin = sum(n_patients),
      .groups = "drop"
      ) |>
    ungroup()|>
    arrange(site, time_bin) |>
    group_by(site) |>
    mutate(
      has_time_bin = as.integer(total_time_bin>0),
      in_registry = as.integer(
        cumsum(has_time_bin) > 0 &
          rev(cumsum(rev(has_time_bin))) > 0
      )
    ) |>
    ungroup()
  
  time_data_sum <- time_data |>
    filter(in_registry==1)|>
    group_by(time_bin) |>
    summarize(n_hosp = n()) 
  
  fig_2d <- ggplot(time_data_sum, aes(
    x = time_bin,
    y = n_hosp)) +
    geom_col(width = 25) + # adjust width as needed for monthly bins
    labs(
      title = "CLIF Sites Contributing Data",
      x = "Date",
      y = "Number of CLIF Sites",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    scale_y_continuous(
      breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      limits = c(0, 11)
    ) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  fig_2d
  ggsave(
    filename = paste0(output_dir,"fig_2d.jpg"),
    plot = fig_2d,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  fig_2a_data_plot <- fig_2a_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, triage_location_formatted) |>
    summarise(
      n_patients_total=sum(n_patients)
    ) |>
    ungroup()|>
    group_by(time_bin) |>
    mutate(n_time_bin = sum(n_patients_total)) |>
    ungroup()|>
    mutate(pct = 100*n_patients_total/n_time_bin)
  
  
  fig_2b_data_plot <- fig_2b_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, last_niv_device) |>
    summarise(
      n_patients_total=sum(n_patients)
    )|>
    ungroup()|>
    group_by(time_bin) |>
    mutate(n_time_bin = sum(n_patients_total)) |>
    ungroup()|>
    mutate(pct = 100*n_patients_total/n_time_bin)
  
  fig_2c_data_plot <- fig_2c_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, physiologic_type) |>
    summarise(
      n_patients_total=sum(n_patients)
    )|>
    ungroup()|>
    group_by(time_bin) |>
    mutate(n_time_bin = sum(n_patients_total)) |>
    ungroup()|>
    mutate(pct = 100*n_patients_total/n_time_bin)
  
  
  
  fig_2_plot_generation <- function(plot_data, y_column, fill_column, 
                                    fill_colors, plot_title, y_axis_title, panel){
    fig_2_plot <- ggplot(plot_data, aes(
      x = time_bin,
      y = !!sym(y_column),
      fill = !!sym(fill_column)
    )) + 
      geom_col(width = 25) + # adjust width as needed for monthly bins
      scale_fill_manual(values=fill_colors) + 
      labs(
        title = plot_title,
        x = "Date",
        y = y_axis_title,
        fill = NULL
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )
    
    ggsave(
      filename = paste0(output_dir,"fig_2", panel,".jpg"),
      plot = fig_2_plot,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    return(fig_2_plot)
  }
  
  
  fig_2_plot_generation(fig_2a_data_plot, "n_patients_total", "triage_location_formatted",
                        TRIAGE_COLORS, "Initial Admission Location", "Number of Encounters", "a")
  fig_2_plot_generation(fig_2b_data_plot, "n_patients_total", "last_niv_device",
                        c("orange", "cadetblue"), "NIV Type", "Number of Encounters", "b")
  fig_2_plot_generation(fig_2c_data_plot, "n_patients_total", "physiologic_type",
                        c("lightblue", "plum1", "lavender", "khaki", "grey"),
                        "Physiologic Type", "Number of Encounters", "c")
  
  fig_2_plot_generation(fig_2a_data_plot, "pct", "triage_location_formatted",
                        TRIAGE_COLORS, "Initial Admission Location", "Percent of Encounters", "ai")
  fig_2_plot_generation(fig_2b_data_plot, "pct", "last_niv_device",
                        c("orange", "cadetblue"), "Initial Admission Location", "Percent of Encounters", "bi")
  fig_2_plot_generation(fig_2c_data_plot, "pct", "physiologic_type",
                        c("lightblue", "plum1", "lavender", "khaki", "grey"),
                          "Initial Admission Location", "Percent of Encounters", "ci")
  
  
} # Figure 2

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

