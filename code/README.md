 ## Code directory

### General Workflow

0. Upload outlier cutoffs & edit/upload the config file
   - Before running the code for this project, please upload the file [`project_outlier_thresholds.csv`](../outlier-thresholds/project_outlier_thresholds.csv) into your local working directory folder
   - As well, please edit and upload the configuration file [`config.yaml`](../config/config.yaml) inside a "config" folder in your local working directory folder
   - Your working directory should appear as follows:
   
    [NAME OF YOUR LOCAL WORKING DIRECTORY FOLDER]/
    
    ├── config/
    
    │   └── config.yaml
    
    ├── 00_local_CLIF_ARF_cohort_identification.Rmd
    
    ├── 01_local_CLIF_ARF_treatment_location_stratification.Rmd
    
    ├── ... [all other Rmd files listed here]
    
    ├── project_outlier_thresholds.csv
    
    └── ...

1. Run the `00_local_CLIF_ARF_cohort_identification.Rmd` script

   **This script should:**
   - Define study parameters and inclusion/exclusion criteria
   - Create hospital blocks
   - Identify valid ED encounters to include
   
   **Before running, users at each location must:**
   - Update `tables_location` as the path to their local CLIF tables
   - Update `project_location` as the path to where these R files reside
   - Update `site_time_zone` to the appropriate time zone
   - Update `site` to the appropriate site name

   **Script:** [`code/00_local_CLIF_ARF_cohort_identification.Rmd`](00_local_CLIF_ARF_cohort_identification.Rmd)
   
   **Input:**
   - CLIF tables `hospitalization`, `adt`, `respiratory_support`, `code_status`, `medication_admin_continuous`
   
   **Expected outputs:**
   - `cohort_size_[SITENAME].txt`: Log of the cohort size as inclusion/exclusion criteria are applied
   - `final_cohort.csv`: Basic table of included patients
   - `hospital_block_key.csv`: Mapping table that links individual hospitalizations to grouped hospital episodes

2. Run the `01_local_CLIF_ARF_treatment_location_stratification.Rmd` script

   **This script should:**
   - Classify physiologic phenotypes of ARF
   - Calculate non-respiratory SOFA scores
   - Track patient outcomes
   - Apply outcome penalization for analysis
   
   **Before running, users at each location must:**
   - Update `site_time_zone` to the appropriate time zone
   - Update `tables_location` as the path to their local CLIF tables
   - Update `project_location` as the path to where these R files reside
   - Update `site` to the appropriate site name
   
   **Script:** [`code/01_local_CLIF_ARF_treatment_location_stratification.Rmd`](01_local_CLIF_ARF_treatment_location_stratification.Rmd)
   
   **Input:** 
   - CLIF tables `patient`, `hospitalization`, `respiratory_support`, `vitals`, `labs`, `medication_admin_continuous`, `patient_assessments`, `adt`
   - `final_cohort.csv`
   - `hospital_block_key.csv`

   **Expected Output:**
   - `final_cohort_with_stratification.csv`: Table of included patients with greater detail (e.g., patient demographics, SOFA, respiratory phenotype)

3. Run the `02_local_CLIF_ARF_treatment_location_descriptive.Rmd` script

   **This script should:**
   - Summarize demographic characteristics between groups
   - Describe and compare unadjusted outcomes data between groups
   
   **Before running, users at each location must:**
   - Update `project_location` as the path to where these R files reside
   - Update `site` to the appropriate site name
   
   **Script:** [`code/02_local_CLIF_ARF_treatment_location_descriptives.Rmd`](02_local_CLIF_ARF_treatment_location_descriptive.Rmd)
   
   **Input:**
   - `final_cohort_with_stratification.csv`: 
   
   **Output:**
   - `tab1_continuous_[SITENAME].csv`: Summary characteristics of included cohort, continuous variables only.
   - `tab1_categorical_[SITENAME].csv`: Summary characteristics of included cohort, categorical variables only.
   - `tab1_continuous_comparisons_[SITENAME].csv`: Pairwise comparisons with post-hoc correction of continuous variables.
   - `tab1_categorical_comparisons_[SITENAME].csv`: Pairwise comparisons with post-hoc correction of categorical variables.
   
4. TODO

**Note that all summarized output that must be shared will be in the `[SITE_NAME_project_output]` directory; private, patient-level data will stay in the `private_tables` folder and should not be shared**