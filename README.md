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

**Stage 1:**

1. Rename `config/config_template.yaml` to `config/config.yaml` with site-specific details

2. Install `uv` (if not already installed) and sync:
   
   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   
   uv sync
   ```
   
3. Run pipeline for the first time:
   
   For MacOS/Linux: 
   ```bash
   chmod +x run_pipeline.sh
   ./run_pipeline.sh
   ```
   
   For Windows: 
   ```bash
   run_pipeline.bat
   ```
   
   At this stage, select "1" when prompted for round number

4. Upload results, `[sitename]_project_output`, to the shared box: [`https://app.box.com/folder/342878616000`](https://app.box.com/folder/342878616000)

5. NOTE: Do NOT share `private_tables`, as this includes patient row-level data; but please make sure to keep the `private_tables` folder in your secure location, as the row-level data will be used in later stages' analyses

6. Wait for Hopkins to pool the local coefficients to generate global coefficients

**Stage 2**

1. Pull from the GitHub repo to update global coefficients

2. Call run_pipeline as described above, but selecting "2" when prompted for round number

3. Upload results, `[sitename]_project_output`, to the shared box: [`https://app.box.com/folder/342878616000`](https://app.box.com/folder/342878616000)

4. NOTE: Do NOT share `private_tables`, as this includes patient row-level data; but please make sure to keep the `private_tables` folder in your secure location, as the row-level data will be used in later stages' analyses

5. Wait for Hopkins to pool the local intercepts to generate global intercepts

**Stage 3**

1. Pull from the GitHub repo to update global intercepts

2. Call run_pipeline as described above, but selecting "3" when prompted for round number

3. Upload results, `[sitename]_project_output`, to the shared box: [`https://app.box.com/folder/342878616000`](https://app.box.com/folder/342878616000)