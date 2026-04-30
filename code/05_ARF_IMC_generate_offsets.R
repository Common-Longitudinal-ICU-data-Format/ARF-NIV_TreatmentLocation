# Sarah Goldfarb
# 03/04/2026

units <- c("icu", "ward", "stepdown")

{ # Setup
  { # Load needed packages
    packages <- c(
      "tidyverse",
      "yaml",
      "rprojroot",
      "stringr",
      "arrow"
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
    file_type <- config$file_type
    
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
  
  { # Reading in and reformatting data
    cat("Reading in and reformatting data\n")
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
    
    meta_hosp <- read_csv(paste0(project_location, 
                                 "/global_model_outputs/global_coeff_by_hosp.csv"),
                          show_col_types = FALSE)
    # meta_imc <- read_csv(paste0(project_location, 
    #                              "/global_model_outputs/global_coeff_by_imc_capable.csv"),
    #                      show_col_types = FALSE)
    
    meta_imc_vs_icu <- read_csv(paste0(project_location, 
                                       "/global_model_outputs/global_coeff_imc_icu_together.csv"),
                                show_col_types = FALSE)
      
    cov_names <- read_csv(paste0(project_location, 
                                "/global_model_outputs/cov_names.csv"),
                         show_col_types = FALSE)|>pull(term)
    
  } # Reading in and reformatting data
  
  { # Reading in CLIF tables
    { # Load the Required CLIF Tables
      cat("Load the Required CLIF Tables\n")
      #List of table names from CLIF 2.0
      tables <- c("patient", "hospitalization", "vitals", "labs", 
                  "medication_admin_continuous", "adt", 
                  "patient_assessments", "respiratory_support", "position", 
                  "dialysis", "intake_output", "ecmo_mcs", "procedures", 
                  "admission_diagnosis", "provider", "sensitivity", 
                  "medication_orders", "medication_admin_intermittent", 
                  "therapy_details", "microbiology_culture", "sensitivity", "microbiology_nonculture", "hospital_diagnosis")
      
      # Tables that should be set to TRUE for this project
      true_tables <- c("adt")
      
      # Create a named vector and set the boolean values
      table_flags <- setNames(tables %in% true_tables, tables)
      
      
      # List all CLIF files in the directory
      clif_table_filenames <- list.files(path = tables_location, 
                                         pattern = paste0("^clif_.*\\.", file_type, "$"), 
                                         full.names = TRUE)
      
      # Extract the base names of the files (without extension)
      clif_table_basenames <- basename(clif_table_filenames) |>
        str_remove(paste0("\\.", file_type, "$"))
      
      # Create a lookup table for required files based on table_flags
      required_files <- paste0("clif_", names(table_flags)[table_flags])
      
      # Check if all required files are present
      missing_tables <- setdiff(required_files, clif_table_basenames)
      if (length(missing_tables) > 0) {
        stop(paste("Error: Missing required tables:", paste(missing_tables, collapse = ", ")))
      }
      
      # Filter only the filenames that are required
      required_filenames <- clif_table_filenames[clif_table_basenames %in% required_files]
      
      # Define the cast function to convert large_string to string
      cast_large_utf8_to_utf8 <- function(x) {
        # Only applies to Arrow objects that have a schema()
        # (open_dataset() returns an Arrow Dataset / query)
        sch <- arrow::schema(x)
        
        large_cols <- vapply(
          sch$fields,
          function(f) f$type$ToString() %in% c("large_string", "large_utf8"),
          logical(1)
        )
        
        cols_to_cast <- sch$names[large_cols]
        if (length(cols_to_cast) == 0) return(x)
        
        x %>% mutate(across(all_of(cols_to_cast), ~ arrow::cast(.x, arrow::utf8())))
      }
      
      # Read the required files into a list of data frames
      if (file_type == "parquet") {
        data_list <- lapply(required_filenames, open_dataset)
        # Apply cast function to normalize Arrow string types right after import
        data_list <- lapply(data_list, cast_large_utf8_to_utf8)
      } else if (file_type == "csv") {
        data_list <- lapply(required_filenames, read_csv)
      } else if (file_type == "fst") {
        data_list <- lapply(required_filenames, read.fst)
      } else {
        stop("Unsupported file format")
      }
      
      # Assign the data frames to variables based on their file names
      for (i in seq_along(required_filenames)) {
        # Extract the base name of the file (without extension)
        object_name <- str_remove(basename(required_filenames[i]), paste0("\\.", file_type, "$"))
        # Make the object name valid for R (replace invalid characters with underscores)
        object_name <- make.names(object_name)
        # Assign the tibble to a variable with the name of the file
        assign(object_name, data_list[[i]])
      }
    } # Load the Required CLIF Tables
    
    { # Merge with hospital block
    cat("Merging adt with hospital block ID\n")
    
    # Hospital block to hospitalization id key
    hospital_block_key <- read_csv(paste0(project_location, "/private_tables/hospital_block_key.csv"), show_col_types=FALSE)
    
    hospital_block_key_obj <- 
      arrow::arrow_table(
        hospital_block_key |>
          select(patient_id,
                 hospitalization_id,
                 hospital_block_id,
                 block_start_admit,
                 block_end_discharge,
                 discharge_location) |>
          # ensure same datatype
          mutate(patient_id = as.character(patient_id),
                 hospitalization_id = as.character(hospitalization_id)))
    
    clif_adt <- clif_adt |>
      select(hospitalization_id, hospital_id, hospital_type, in_dttm, out_dttm, location_name, location_category, location_type) |>
      # ensure same datatype
      mutate(hospitalization_id = as.character(hospitalization_id)) |>
      # Ensure all time data structure are the same between tables 
      mutate(across(ends_with("dttm"), ~ cast(.x, timestamp("us", timezone=site_time_zone)))) |>
      inner_join(hospital_block_key_obj, by = c("hospitalization_id")) |>
      compute()
    } # Merge with hospital block
    
  } # Reading in CLIF tables
  
} # Setup

{ # Identify where the stepup occured
  cat("Identifying where the stepup occured\n")
  escalated_cases <- final_cohort |>
    filter(was_escalated==1) |>
    select(hospital_block_id, start_ed, stop_ed, triage_location, first_hospital_id, imc_capable)
  
  
  cat("Merging with adt\n")
  all_escalations <- arrow::arrow_table(escalated_cases) |>
    left_join(clif_adt |>
                select(hospital_block_id, in_dttm, location_category),
              by="hospital_block_id") |>
    collect() |>
    filter(in_dttm > stop_ed) |>
    mutate(
        triage_rank = case_when(
          tolower(triage_location) == "ward" ~ 1,
          tolower(triage_location) == "stepdown" ~ 2,
          tolower(triage_location) == "icu" ~ 3,
          TRUE ~ 0
        ),
        adt_rank = case_when(
          tolower(location_category) == "ward" ~ 1,
          tolower(location_category) == "stepdown" ~ 2,
          tolower(location_category) == "icu" ~ 3,
          TRUE ~ 0
        )
    ) |>
    filter(adt_rank > triage_rank)
  
  highest_lvl_care <- function(df, hours_in=NA){
    if(!is.na(hours_in)){
      df <- df |>
        filter(in_dttm <= stop_ed + hours(hours_in))
    }
    
    df <- df |>
      group_by(hospital_block_id)|>
      arrange(desc(adt_rank), in_dttm) |> # max rank first, then earliest time
      slice(1) |> # take the best row per group
      ungroup() |>
      select(hospital_block_id, first_hospital_id, imc_capable, triage_location, escalation_location=location_category)
    
    return(df)
  }
  
  within_hospitalization <- highest_lvl_care(all_escalations) |>
    left_join(highest_lvl_care(all_escalations, 24)|>
                select(hospital_block_id, escalation_location_24=escalation_location), by="hospital_block_id")|>
    left_join(highest_lvl_care(all_escalations, 48)|>
                select(hospital_block_id, escalation_location_48=escalation_location), by="hospital_block_id") |>
    mutate(triage_location_formatted =
             case_when(
               tolower(triage_location) == "stepdown" ~ "IMC",
               imc_capable == 1 & tolower(triage_location) == "ward" ~ "Ward (+)",
               imc_capable == 0 & tolower(triage_location) == "ward" ~ "Ward (-)",
               TRUE ~ "ERROR"
             ))
  
  
  cat("Outputting where escalation in level of care occured\n")
  within_hospitalization_output <- within_hospitalization |>
    select(triage_location_formatted,
           escalation_location,
           escalation_location_24,
           escalation_location_48) |>
    pivot_longer(
      cols = starts_with("escalation_location"),
      names_to = "timepoint",
      values_to = "escalation"
    ) |>
    count(triage_location_formatted, timepoint, escalation, name = "n") |>
    arrange(triage_location_formatted, timepoint, desc(n)) |>
    filter(!is.na(escalation))
  
  write_csv(within_hospitalization_output, file.path(project_location, paste0(site, "_project_output/", site, "_escalation_locations.csv")))
  
} # Identify where the stepup occured

{ # Output timeline for each hospital
  cat("Outputting first and last month of admissions for each hopsital\n")
  
  hospitalization_date_range <- final_cohort |>
    arrange(first_hospital_id, start_ed) |>
    group_by(first_hospital_id) |>
    summarize(
      year_start = first(year(start_ed)),
      month_start = first(month(start_ed)),
      year_end = last(year(start_ed)),
      month_end = last(month(start_ed))
    ) |>
    ungroup()
  
  write_csv(hospitalization_date_range, file.path(project_location, paste0(site, "_project_output/", site, "_hospitalization_date_range.csv")))
  
} # Output timeline for each hospital

{ # Compare row-level data to summary coeff
  
  # Create function that reshapes patient row-level data to match covariate structure
  # E.g., rather than ph_factor as 0 1 2 etc, will have ph_factorNot Measured, ph_factor>7.35, ph_factor>7.30... as 0s and 1s
  reshape_patient_covariate_cols <- function(cohort_data){
    
    # Identify which covariate columns are factors
    factor_cols <- names(cohort_data |> select(covariates))[sapply(cohort_data |> select(covariates), is.factor)]
    
    # Reshape matrix to be dummy variables for factors
    dummy_matrix <- model.matrix(
      ~ . - 1,
      data = cohort_data[factor_cols]
    )[,-1] |> as.data.frame()
    
    # Identify which columns need to be converted back to factors
    factorized_covs <- cov_names[grepl(paste(factor_cols, collapse="|"), cov_names)]
    
    # Reshape overall patient row level data with factorized dummy variables
    return(cohort_data |>
      select(-all_of(factor_cols)) |>
      bind_cols(dummy_matrix) |>
      # re-order them to match
      select(c("triage_location", 
               "first_hospital_id", 
               "imc_capable",
               cov_names, 
               outcomes_binary)) |>
      # Set as factors rather than numeric
      mutate(across(all_of(factorized_covs), ~factor(.)))
      )
  }
  
  cat("Saving row-level covariate values\n")
  # Save row-level covariate values for next run privately (not to share with CLIF)
  write_csv(reshape_patient_covariate_cols(final_cohort),
            paste0(project_location, "/private_tables/model_data.csv"))
  
  { # Generate hospital- or imccapable- level gammas
    # Meta coeff is: meta_hosp or meta_imc
    # Strat var must be: first_hospital_id or "imc_capable"
    calculate_gamma <- function(cohort_data, hosp_data, meta_coeff, strat_var){
      
      # Define list of imc capability means
      imc_cap_levels <- c(0, 1)
      imc_cap_level_names <- c("imc_incapable", "imc_capable")
      
      # Initialize gamma lists and errors
      gamma_list_all <- list()
      gamma_error_list_all <- list()
      
      # Select only relevant columns
      model_data_all <- cohort_data |>
        select(c(triage_location, first_hospital_id, imc_capable, covariates, outcomes_binary))
      
      # Reformat data 
      # E.g., rather than ph_factor as 0 1 2 etc, will have ph_factorNot Measured, ph_factor>7.35, ph_factor>7.30... as 0s and 1s
      model_data_all <- reshape_patient_covariate_cols(model_data_all)
      
      for(outcome_i in outcomes_binary){
        for(unit_i in units){
          # Select only the patients admitted to the desired unit at the specified hospital/imc capable
          model_data_unit <- model_data_all |>
            filter(triage_location == unit_i)
          
          # Initialize the gamma and gamma errors as empty
          gamma_list = gamma_error_list = c()
          
          if(!strat_var %in% c("first_hospital_id", "imc_capable")){
            stop("Stratifying variable must be hospital (first_hospital_id) or imc capability (imc_capable)")
          }
          
          iteration_var <- c(1:ifelse(strat_var=="first_hospital_id", nrow(hosp_data), 2))
          
          # Iterate over each hospital
          for(i in iteration_var){
            
            # Define the name of the df, therefore where indexed in the output list, 
            # depending on if stratified by hospital or IMC capability
            index_i <- ifelse(strat_var=="first_hospital_id",
                              hosp_data$first_hospital_id[i],
                              imc_cap_level_names[i])
            
            # Select data from only this hospital or IMC capability
            if(strat_var=="first_hospital_id"){
              model_data <- model_data_unit |> 
                filter(first_hospital_id == hosp_data$first_hospital_id[i]) |>
                # Only want the covariate columns (converted to factored versions)
                select(c(cov_names, outcome_i))
            }
            else{
              model_data <- model_data_unit |> 
                filter(imc_capable == imc_cap_levels[i]) |>
                # Only want the covariate columns (converted to factored versions)
                select(c(cov_names, outcome_i))
            }
            
            # Only continue if there is actually row-level data for this model
            if(nrow(model_data) > 0){
              
              # Global fixed effects generated for this outcome within a given unit
              beta_meta_fix <- meta_coeff[[paste0(outcome_i, ".", unit_i)]]
              
              # Use beta as plug-in estimator and re-fit model
              # each entry of os is the patient-specific linear predictor, 
              # i.e. the log odds of the outcome for that patient
              os = as.matrix(model_data |>
                               # Select only covairate cols
                               select(cov_names) |>
                               # Convert to character (must do before numeric)
                               mutate(across(everything(), as.character)) |>
                               # Convert to numeric
                               mutate(across(everything(), as.numeric))
              ) %*% 
                as.matrix(beta_meta_fix)
              
              model_data <- transform(model_data,os=os)
              
              fit_sub = glm(model_data[[outcome_i]] ~ 1 + offset(os),
                            data = model_data, family = "binomial")
              
              fit_summary = summary(fit_sub)
              
              # save the intercept
              gamma_list[index_i] = fit_summary$coefficients[1]
              gamma_error_list[index_i] = fit_summary$coefficients[2]
            }
            else{
              gamma_list[index_i] = NA
              gamma_error_list[index_i] = NA
            }
          }
          
          gamma_list_all[[outcome_i]][[unit_i]] <- gamma_list
          gamma_error_list_all[[outcome_i]][[unit_i]] <- gamma_error_list
          
        }
      }
      
      return(list(gamma=gamma_list_all, error=gamma_error_list_all))
    }
    
    
  cat("Generating gammas by hospital\n")
    # Run
    all_gammas_by_hospital <- calculate_gamma(final_cohort, 
                                              hospital_data, 
                                              meta_hosp, 
                                              "first_hospital_id")
    
    # cat("Generating gammas by IMC capability\n")
    # all_gammas_by_imc_cap <- calculate_gamma(final_cohort, 
    #                                           hospital_data, 
    #                                           meta_imc, 
    #                                           "imc_capable")
    
    
    cat("Saving gammas to file\n")
    # Save outcomes
    # Note: hospital order is defined by hospital_data
    write_csv(data.frame(all_gammas_by_hospital$gamma) |>
                rownames_to_column("first_hospital_id"),
              paste0(model_out_dir, site, "_gamma_by_hosp.csv"))
    write_csv(data.frame(all_gammas_by_hospital$error) |>
                rownames_to_column("first_hospital_id"),
              paste0(model_out_dir, site, "_gamma_error_by_hosp.csv"))
    
    # write_csv(data.frame(all_gammas_by_imc_cap$gamma) |>
    #             rownames_to_column("first_hospital_id"),
    #           paste0(model_out_dir, site, "_gamma_by_imc_cap.csv"))
    # write_csv(data.frame(all_gammas_by_imc_cap$error) |>
    #             rownames_to_column("first_hospital_id"),
    #           paste0(model_out_dir, site, "_gamma_error_by_imc_cap.csv"))
    
  } # Generate hospital- or imccapable- level gammas

  { # Generate gammas for imc vs icu
    
    calculate_all_gammas_imc_v_icu <- function(cohort_data, hosp_data, meta_coeff){
      
      # Initialize gamma lists and errors
      gamma_list_all <- list()
      gamma_error_list_all <- list()
      
      # Select only relevant columns
      model_data_all <- cohort_data |>
        filter(triage_location %in% c("icu", "stepdown")) |>
        select(c(triage_location, first_hospital_id, imc_capable, covariates, outcomes_binary))
      
      # Reformat data 
      # E.g., rather than ph_factor as 0 1 2 etc, will have ph_factorNot Measured, ph_factor>7.35, ph_factor>7.30... as 0s and 1s
      model_data_all <- reshape_patient_covariate_cols(model_data_all)
      
      for(outcome_i in outcomes_binary){
        
        for(unit_i in units){
          # Select only the patients admitted to the desired unit at the specified hospital/imc capable
          model_data_unit <- model_data_all |>
            filter(triage_location == unit_i)
          
          # Initialize the gamma and gamma errors as empty
          gamma_list = gamma_error_list = c()
          
          # Iterate over each hospital
          for(i in c(1:nrow(hosp_data))){
            
            # Only do analysis if this hospital is IMC capable
            if(hosp_data$imc_capable[i]== 1){
              # Define the name of the df, therefore where indexed in the output list, 
              # depending on if stratified by hospital
              index_i <- hosp_data$first_hospital_id[i]
              
              # Select data from only this hospital
              model_data <- model_data_unit |> 
                filter(first_hospital_id == hosp_data$first_hospital_id[i]) |>
                # Only want the covariate columns (converted to factored versions)
                select(c(cov_names, outcome_i))
              
              if(nrow(model_data) > 0){
                
                # Global fixed effects generated for this outcome within a given unit
                beta_meta_fix <- meta_coeff[[paste0(outcome_i, ".", unit_i)]]
                
                # Use beta as plug-in estimator and re-fit model
                # each entry of os is the patient-specific linear predictor, 
                # i.e. the log odds of the outcome for that patient
                os = as.matrix(model_data |>
                                 # Select only covairate cols
                                 select(cov_names) |>
                                 # Convert to character (must do before numeric)
                                 mutate(across(everything(), as.character)) |>
                                 # Convert to numeric
                                 mutate(across(everything(), as.numeric))
                ) %*% 
                  as.matrix(beta_meta_fix)
                
                model_data <- transform(model_data,os=os)
                
                fit_sub = glm(model_data[[outcome_i]] ~ 1 + offset(os),
                              data = model_data, family = "binomial")
                
                fit_summary = summary(fit_sub)
                
                # save the intercept
                gamma_list[index_i] = fit_summary$coefficients[1]
                gamma_error_list[index_i] = fit_summary$coefficients[2]
              }
              
            }
            else{
              stop("IMC incapable hospital sent in accidentally")
            }
            
          }
          
          gamma_list_all[[outcome_i]][[unit_i]] <- gamma_list
          gamma_error_list_all[[outcome_i]][[unit_i]] <- gamma_error_list
          
        }
      }
      
      return(list(gamma=gamma_list_all, error=gamma_error_list_all))
    }
    
    imc_capable_hospital_data <- hospital_data|>filter(imc_capable==1)
    
    # Run
    if(nrow(imc_capable_hospital_data) > 0){

      cat("Repeating, but generating gammas for IMC vs ICU\n")

      all_gammas_imc_v_icu <- calculate_all_gammas_imc_v_icu(final_cohort, 
                                              imc_capable_hospital_data, 
                                              meta_hosp)
      
      cat("Saving results\n")
      # Note: hospital order is defined by hospital_data
      write_csv(data.frame(all_gammas_imc_v_icu$gamma) |>
                  rownames_to_column("first_hospital_id"),
                paste0(model_out_dir, site, "_imc_vs_icu_gamma.csv"))
      write_csv(data.frame(all_gammas_imc_v_icu$error) |>
                  rownames_to_column("first_hospital_id"),
                paste0(model_out_dir, site, "_imc_vs_icu_gamma_error.csv"))

      cat("Run successful!\n")
    }
    else{
      cat("No hospitals here are IMC capable, skipping secondary analysis\n")
      cat("Run successful!\n")
    }
    
  } # Generate gammas for imc vs icu
  
} # Compare row-level data to summary coeff