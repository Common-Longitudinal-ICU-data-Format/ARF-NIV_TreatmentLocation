 ## Code directory

### General Workflow

0. Upload outlier cutoffs & edit/upload the config file
   - Before running the code for this project, please upload the file [`project_outlier_thresholds.csv`](../outlier-thresholds/project_outlier_thresholds.csv) into your local project directory folder
   - As well, please edit and upload the configuration file [`config.yaml`](../config/config.yaml) inside a "config" folder in your local project directory folder
   - Your project directory should appear as follows:
   
      <pre>
      [NAME OF YOUR LOCAL PROJECT DIRECTORY FOLDER]/
      ├── config/
      │   └── config.yaml
      ├── 00_local_CLIF_ARF_cohort_identification.Rmd
      ├── 01_local_CLIF_ARF_treatment_location_stratification.Rmd
      ├── ... [all other Rmd files listed here]
      ├── project_outlier_thresholds.csv
      └── ...
      </pre>

1. Run the `00_local_CLIF_ARF_cohort_identification.Rmd` script

   **This script should:**
   - Define study parameters and inclusion/exclusion criteria
   - Create hospital blocks
   - Identify valid ED encounters to include

   **Script:** [`code/00_local_CLIF_ARF_cohort_identification.Rmd`](00_local_CLIF_ARF_cohort_identification.Rmd)
   
   **Input:**
   - CLIF tables `hospitalization`, `adt`, `respiratory_support`, `code_status`
   
   **Expected outputs:**
   - `broad_cohort_size_[SITENAME].txt`: Log of the cohort size as inclusion/exclusion criteria are applied
   - `broad_cohort.csv`: Basic table of included patients
   - `hospital_block_key.csv`: Mapping table that links individual hospitalizations to grouped hospital episodes

2. Run the `01_local_CLIF_ARF_treatment_location_descriptive_tables.Rmd` script

   **This script should:**
   - Classify physiologic phenotypes of ARF
   - Calculate non-respiratory SOFA scores
   - Track patient outcomes
   - Apply outcome penalization for analysis
   - Identify the analytic cohort
   
   **Script:** [`code/01_local_CLIF_ARF_treatment_location_cohort_selection.Rmd`](01_local_CLIF_ARF_treatment_location_cohort_selection.Rmd)
   
   **Input:** 
   - CLIF tables `patient`, `hospitalization`, `respiratory_support`, `vitals`, `labs`, `medication_admin_continuous`, `patient_assessments`, `adt`
   - `broad_cohort.csv`
   - `hospital_block_key.csv`

   **Expected Output:**
   - `analytic_cohort_size_[SITENAME].txt`: Log of the cohort size as final exclusion criteria are applied
   - `broad_cohort_with_stratification.csv`: Table of all patients with greater detail (e.g., patient demographics, SOFA, respiratory phenotype)
   - `analytic_cohort_with_stratification.csv`: Table of analytic cohort with greater detail (e.g., patient demographics, SOFA, respiratory phenotype)

3. Run the `02_local_CLIF_ARF_treatment_location_descriptive_tables.Rmd` script

   **This script should:**
   - Summarize demographic characteristics between groups
   
   **Script:** [`code/02_local_CLIF_ARF_treatment_location_descriptive_tables.Rmd`](02_local_CLIF_ARF_treatment_location_descriptive_tables.Rmd)
   
   **Input:**
   - `broad_cohort_with_stratification.csv` 
   - `analytic_cohort_with_stratification.csv`
   
   **Output:**
   - Various csv and html files (all including "tab1" in the name) with summarized demographic data
   
4. Run the `03_local_CLIF_ARF_treatment_location_outcomes.Rmd` script

   **This script should:**
   - Summarize outcomes between groups
   
   **Script:** [`code/03_local_CLIF_ARF_treatment_location_outcomes.Rmd`](03_local_CLIF_ARF_treatment_location_outcomes.Rmd)
   
   **Input:**
   - `analytic_cohort_with_stratification.csv`
   
   **Output:**
   - Various csv and html files with summarized outcome data

5. TODO

**Note that all summarized output that must be shared will be in the `[SITE_NAME_project_output]` directory; private, patient-level data will stay in the `private_tables` folder and should not be shared**