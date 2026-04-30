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
  
  
  rm(raw_table_1_3group_cat, raw_table_1_3group_cont, raw_table_1_5group_cat, raw_table_1_5group_cont,
     all_table_1_3group_cat, all_table_1_3group_cat_display, all_table_1_3group_cont, all_table_1_3group_cont_display,
     all_table_1_5group_cat, all_table_1_5group_cat_display, all_table_1_5group_cont, all_table_1_5group_cont_display)
} # Load and output table 1

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
  
  # Clean space
  rm(raw_table_2_3group_cat, raw_table_2_3group_cont, raw_table_2_5group_cat, raw_table_2_5group_cont,
     all_table_2_3group_cat, all_table_2_3group_cat_display, all_table_2_3group_cont, all_table_2_3group_cont_display,
     all_table_2_5group_cat, all_table_2_5group_cat_display, all_table_2_5group_cont, all_table_2_5group_cont_display)
  
} # Load and output table 2

{ # Figure 1a
  fig_1a_data <- load_site_csv(site=sites[1], filename=paste0("fig_1a_data"))
  
  for(site in sites[-1]){
    fig_1a_data <- fig_1a_data |>
      add_row(load_site_csv(site=site, filename=paste0("fig_1a_data")) |> mutate(hospital=as.character(hospital)))
  }
  
  hosp_info_tab_long <- fig_1a_data |>
    arrange(-pct_ICU) |>
    select(hospital, ICU, IMC, Ward, n_total) |>
    pivot_longer(
      cols = c(ICU, IMC, Ward),
      names_to = "unit",
      values_to = "n"
    ) |>
    mutate(unit = factor(unit, levels = c("ICU", "IMC", "Ward")),
           pct = n/n_total
    ) 
  
  # Actual plot
  hosp_info_plot <- ggplot(hosp_info_tab_long, aes(x = factor(hospital, levels = unique(hospital)),
                                                   y = pct,
                                                   fill = unit)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_fill_manual(values = TRIAGE_COLORS,
                      name=expression(bold("Admission Unit:"))) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = "Hospital",
      y = "Percent of Admissions",
      fill = "Unit"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.direction="horizontal",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_blank(),   # hides hospital names
      axis.ticks.x = element_blank()
    )
  
  hosp_info_plot
  
  ### Save as jpg to view ###
  ggsave(paste0(output_dir, "fig_1a.jpg"), 
         plot=hosp_info_plot, width=6, height=5, dpi=300)
  
} # Figure 1a

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
  
  fig_2a_data_plot <- fig_2a_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, triage_location_formatted) |>
    summarise(
      n_patients_total=sum(n_patients)
    )
  fig_2b_data_plot <- fig_2b_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, last_niv_device) |>
    summarise(
      n_patients_total=sum(n_patients)
    )
  fig_2c_data_plot <- fig_2c_data |>
    mutate(n_patients=case_when(
      n_patients=="< 5" ~ 0,
      TRUE ~ as.integer(n_patients)
    )) |>
    group_by(time_bin, physiologic_type) |>
    summarise(
      n_patients_total=sum(n_patients)
    )
  
  
  fig_2a_plot <- ggplot(fig_2a_data_plot, aes(
    x = time_bin,
    y = n_patients_total,
    fill = triage_location_formatted
  )) +
    geom_col(width = 25) + # adjust width as needed for monthly bins
    scale_fill_manual(values = TRIAGE_COLORS) +
    labs(
      title = "Initial Admission Location",
      x = "Date",
      y = "Number of Encounters",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  ggsave(
    filename = paste0(output_dir,"fig_2a.jpg"),
    plot = fig_2a_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  fig_2b_plot <- ggplot(fig_2b_data_plot, aes(
    x = time_bin,
    y = n_patients_total,
    fill = last_niv_device
  )) +
    geom_col(width = 25) + # adjust width as needed for monthly bins
    scale_fill_manual(values = c("orange", "cadetblue")) +
    labs(
      title = "NIV Type",
      x = "Date",
      y = "Number of Encounters",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  ggsave(
    filename = paste0(output_dir,"fig_2b.jpg"),
    plot = fig_2b_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  fig_2c_plot <- ggplot(fig_2c_data_plot, aes(
    x = time_bin,
    y = n_patients_total,
    fill = physiologic_type
  )) +
    geom_col(width = 25) + # adjust width as needed for monthly bins
    scale_fill_manual(values =  c("lightblue", "plum1", "lavender", "khaki", "grey")) +
    labs(
      title = "Physiologic Type",
      x = "Date",
      y = "Number of Encounters",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  ggsave(
    filename = paste0(output_dir,"fig_2c.jpg"),
    plot = fig_2c_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  
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

