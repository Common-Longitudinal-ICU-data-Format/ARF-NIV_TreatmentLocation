# Emergency Department Triage Pathways in Acute Respiratory Failure: A comprehensive analysis of utilization and outcomes.

## CLIF 2.1

## Objective

Acute respiratory failure (ARF) is a common and serious condition that places significant burdens on healthcare systems in the United States. ARF requiring respiratory support (invasive or noninvasive) has traditionally been managed in an intensive care unit (ICU). However, there has been growing utilization of non-ICU settings for the management of moderately severe ARF, defined here as those requiring non-invasive ventilation (NIV). 

Nevertheless, contemporary outcomes of moderately severe ARF patients admitted to varying levels of care (e.g., ward versus IMC versus ICU) remain poorly described.

**Our aim is to characterize case-mix, outcomes, and utilization of triage settings (e.g., ICU versus IMC versus wards) for patients with moderately severe ARF.**

## Required CLIF tables and fields

Please refer to the online [CLIF data dictionary](https://clif-consortium.github.io/website/data-dictionary.html), [ETL tools](https://github.com/clif-consortium/CLIF/tree/main/etl-to-clif-resources), and [specific table contacts](https://github.com/clif-consortium/CLIF?tab=readme-ov-file#relational-clif) for more information on constructing the required tables and fields. 

The following tables are required:
1. **hospitalization**: `patient_id`, `hospitalization_id`, `admission_dttm`, `discharge_dttm`, `age_at_admission`, `discharge_category`
2. **adt**: `hospitalization_id`, `hospital_id`, `hospital_type`, `in_dttm`. `out_dttm` `location_category`, `location_type`
3. **respiratory_support**: `hospitalization_id`, `recorded_dttm`, `device_category`, `tracheostomy`, `fio2_set`
4. **patient**: `patient_id`, `race_category`, `ethnicity_category`, `sex_category`
5. **vitals**: `hospitalization_id`, `recorded_dttm`, `vital_category`, `vital_value`
   - `vital_category` = 'map', 'spo2', 'weight_kg'
6. **labs**: `hospitalization_id`, `lab_collect_dttm`, `lab_category`, `lab_value`
   - `lab_category` = 'so2_arterial', 'so2_mixed_venous', 'so2_central_venous', 'ph_arterial', 'ph_venous', 'pco2_arterial', 'pco2_venous', 'po2_arterial', 'platelet_count', 'bilirubin_total', 'creatinine'
7. **medication_admin_continuous**: `hospitalization_id`, `admin_dttm`, `med_name`, `med_category`, `med_group`, `med_dose`, `med_dose_unit`, `mar_action_name`
   - `med_category` = 'norepinephrine', 'epinephrine', 'phenylephrine', 'angiotensin', 'vasopressin', 'dopamine', 'dobutamine'
   - `med_group` = 'vasoactives'
8. **patient_assessments**: `hospitalization_id`, `recorded_dttm`, `numerical_value`, `assessment_category` = 'gcs_total'
9. **code_status**: `patient_id`, `start_dttm`, `code_status_category`
10. We would like to include **patient_diagnosis**: `patient_id`, `hospitalization_id`, `diagnosis_code`, `diagnosis_code_format`, `start_dttm`, `end_dttm`, although we do not use it in our code as of now

## Cohort identification
**Inclusion Criteria:**
- Age >18 years
- Presented first to the ED at a CLIF hospital from 2018-01-01 to 2024-12-31, inclusive
- NIV (defined as: continuous positive airway pressure (CPAP), non-invasive positive pressure ventilation (NIPPV), and/or high flow nasal oxygen (HFNO)) started in the ED
- Within the first 6 hours after NIV initiation, patients must stay on NIV with (1) no more than 2 cumulative hours off NIV (e.g., room air or nasal cannula) and (2) no intubation or tracheostomy

**Exclusion Criteria:**
- Intubation or tracheostomy while in the ED
- Continuous vasopressors received while in the ED
- Code status of "allow natural death" at time of leaving the ED (e.g., admission)
- Patient still admitted by end-point of study

## Expected Results

`.txt`: A text tile that reports the log of cohort size (will be used for CONSORT flow diagram). 

`tab1_continuous`: TODO

`tab1_categorical`: TODO

`tab1_continuous_comparisons`: TODO

`tab1_categorical_comparisons`: TODO

**Final project results should be saved in the [`/project_tables/`] directory.**

## Detailed Instructions for running the project

## 1. Update `config/config.json`
Follow instructions in the [config/README.md](config/README.md) file for detailed configuration steps.

**Note: if using the `01_run_cohort_id_app.R` file, this step is not necessary as the app will create the config file for the user**

## 2. Set up the project environment

*Describe the steps to setup the project environment.*

Example for R:
Run `00_renv_restore.R` in the [code](code/templates/R) to set up the project environment

```

## 3. Run code

Detailed instructions on the code workflow are provided in the [code directory](code/README.md)

Please deposit your entire result_[SITE_NAME] at ____TODO____
---


