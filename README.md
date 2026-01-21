# Characteristics and outcomes of ED patients requiring non-invasive respiratory support admitted to ward, intermediate, and intensive care settings

## CLIF 2.1

## Objective

Acute respiratory failure (ARF) is a common and increasingly prevalent cause of hospitalization and death in the United States, accompanied by rising use of high-flow nasal oxygen (HFNO) and non-invasive ventilation (NIV). Although such patients have traditionally been managed in intensive care units (ICUs), many can now be safely treated in intermediate care (IMC) or specialized ward settings, depending on illness severity and available monitoring and staffing. IMC units were developed to optimize ICU utilization and may influence both triage patterns and outcomes for patients receiving HFNO or NIV, yet their role in this population remains incompletely defined. 

**In this study, we examine variation in clinical outcomes among patients with ARF initiated on HFNO and/or NIV in the emergency department across hospitals in the CLIF consortium, testing whether the availability of IMC capability is associated with differences in mortality and hospital length of stay across ICU, IMC, and ward admissions.**

## Required CLIF tables and libraries

Please refer to the online [CLIF data dictionary](https://clif-icu.com/data-dictionary), and [ETL tools](https://clif-icu.com/etl-guide), for more information on constructing the required tables and fields.

The following tables are required:
1. **`hospitalization`**
2. **`adt`**
3. **`respiratory_support`**
4. **`medication_admin_continuous`**
5. **`code_status`**
6. **`patient`**
7. **`vitals`**
8. **`labs`**
9. **`patient_assessments`**
10. **`hospital_diagnosis`**
11. **`crrt_therapy`**

To run this code, you must also be able to run R and Python and have clify installed.

## Instructions

1. Rename the `config/config_template.yaml` to `config/config.yaml` with site specific details 
2. Run `code/01_ARF_IMC_cohort.Rmd`
3. To run `code/01_ARF_IMC_sepsis_indicators.py`:
    3.1 Install uv (if not already installed): `curl -LsSf https://astral.sh/uv/install.sh | sh`
    3.2 Sync the Python environment and install dependencies: `uv sync`
    3.3 Run `uv run code/01_ARF_IMC_sepsis_indicators.py`
4. 

## Expected Results
In running the code, each site will produce two folders: `private_tables` and `[sitename]_project_output`. `private_tables` is NOT to be shared, as it includes patient row-level data for local analysis. When completed your analysis, please upload `[sitename]_project_output` to the shared box: [`https://app.box.com/folder/342878616000`](https://app.box.com/folder/342878616000).

