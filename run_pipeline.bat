@echo off
chcp 65001 >nul 2>&1
REM ════════════════════════════════════════════════════════════════════════════
REM  run_pipeline.bat — Execute the full CLIF treatment location analysis pipeline
REM ════════════════════════════════════════════════════════════════════════════
setlocal EnableDelayedExpansion

REM ── paths ────────────────────────────────────────────────────────────────
set "PROJECT_ROOT=%~dp0"
REM Remove trailing backslash
if "!PROJECT_ROOT:~-1!"=="\" set "PROJECT_ROOT=!PROJECT_ROOT:~0,-1!"

REM Get timestamp using PowerShell (wmic is deprecated on newer Windows)
for /f %%I in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmmss"') do set "TIMESTAMP=%%I"

set "LOG_DIR=!PROJECT_ROOT!\logs"
if not exist "!LOG_DIR!" mkdir "!LOG_DIR!"
set "LOG_FILE=!LOG_DIR!\pipeline_!TIMESTAMP!.log"

REM ── logging ──────────────────────────────────────────────────────────────
echo CLIF Treatment Location ARF Pipeline
echo CLIF Treatment Location ARF Pipeline >> "!LOG_FILE!"
echo Started: %date% %time%
echo Started: %date% %time% >> "!LOG_FILE!"
echo Log: !LOG_FILE!
echo Log: !LOG_FILE! >> "!LOG_FILE!"
echo.

REM ── environment (uv) ─────────────────────────────────────────────────────
where uv >nul 2>&1
if errorlevel 1 (
    echo ERROR: uv not found. Install it: https://docs.astral.sh/uv/getting-started/installation/
    echo ERROR: uv not found. >> "!LOG_FILE!"
    exit /b 1
)

echo Syncing dependencies with uv...
echo Syncing dependencies with uv... >> "!LOG_FILE!"
uv sync --project "!PROJECT_ROOT!" 2>&1
echo Environment ready
echo Environment ready >> "!LOG_FILE!"
echo.

REM ── locate R ─────────────────────────────────────────────────────────────
where Rscript >nul 2>&1
if errorlevel 1 (
    REM Try common Windows R install locations
    set "R_FOUND="
    for /f "delims=" %%D in ('dir /b /ad /o-n "C:\Program Files\R\R-*" 2^>nul') do (
        if not defined R_FOUND (
            if exist "C:\Program Files\R\%%D\bin\Rscript.exe" (
                set "PATH=C:\Program Files\R\%%D\bin;!PATH!"
                set "R_FOUND=1"
                echo Found R at: C:\Program Files\R\%%D\bin
                echo Found R at: C:\Program Files\R\%%D\bin >> "!LOG_FILE!"
            )
        )
    )
    if not defined R_FOUND (
        for /f "delims=" %%D in ('dir /b /ad /o-n "C:\Program Files (x86)\R\R-*" 2^>nul') do (
            if not defined R_FOUND (
                if exist "C:\Program Files (x86)\R\%%D\bin\Rscript.exe" (
                    set "PATH=C:\Program Files (x86)\R\%%D\bin;!PATH!"
                    set "R_FOUND=1"
                    echo Found R at: C:\Program Files (x86^)\R\%%D\bin
                    echo Found R at: C:\Program Files (x86^)\R\%%D\bin >> "!LOG_FILE!"
                )
            )
        )
    )
    if not defined R_FOUND (
        echo WARNING: Rscript not found on PATH or in standard install locations.
        echo WARNING: Rscript not found >> "!LOG_FILE!"
        echo Please add R to your PATH or install R from https://cran.r-project.org/
    )
)

REM ── helpers ──────────────────────────────────────────────────────────────
set "PYTHONUNBUFFERED=1"
set "PYTHONIOENCODING=utf-8"
set "MPLBACKEND=Agg"
set "PYTHONPATH=!PROJECT_ROOT!\code;!PYTHONPATH!"

set STEP=0
set TOTAL=4
set "FAILED_STEPS="

REM ── pipeline (cwd = code\ so relative paths work) ──────────────────────
cd /d "!PROJECT_ROOT!\code"

REM ── request which round ─────────────────────────────────────────────────
set /p "run_num=Which round are you running (i.e., 1 = cohort selection + initial analysis, 2 = using global coefficients, 3 = using global intercepts): "

if "!run_num!"=="1" goto :round1
if "!run_num!"=="2" goto :round2
if "!run_num!"=="3" goto :round3
if "!run_num!"=="4" goto :round4
echo Invalid choice. Exiting.
exit /b 1

REM ════════════════════════════════════════════════════════════════════════
:round1
where Rscript >nul 2>&1
if errorlevel 1 (
    echo WARNING: Rscript not found -- skipping R steps.
    echo WARNING: Rscript not found >> "!LOG_FILE!"
    echo Run manually: cd code ^&^& Rscript 00_ARF_IMC_cohort.R
    set "FAILED_STEPS=!FAILED_STEPS! 00_ARF_IMC_cohort"
    goto :round1_python
)
set /a STEP+=1
echo [!STEP!/!TOTAL!] 00_ARF_IMC_cohort
echo [!STEP!/!TOTAL!] 00_ARF_IMC_cohort >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 00_ARF_IMC_cohort.R > "!LOG_DIR!\00_ARF_IMC_cohort_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\00_ARF_IMC_cohort_!TIMESTAMP!.log"
type "!LOG_DIR!\00_ARF_IMC_cohort_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "00_ARF_IMC_cohort"

:round1_python
set /a STEP+=1
echo [!STEP!/!TOTAL!] 01_ARF_IMC_sepsis_indicators
echo [!STEP!/!TOTAL!] 01_ARF_IMC_sepsis_indicators >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
uv run --project "!PROJECT_ROOT!" python 01_ARF_IMC_sepsis_indicators.py > "!LOG_DIR!\01_ARF_IMC_sepsis_indicators_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\01_ARF_IMC_sepsis_indicators_!TIMESTAMP!.log"
type "!LOG_DIR!\01_ARF_IMC_sepsis_indicators_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "01_ARF_IMC_sepsis_indicators"

where Rscript >nul 2>&1
if errorlevel 1 (
    echo WARNING: Rscript not found -- skipping remaining R steps.
    echo WARNING: Rscript not found >> "!LOG_FILE!"
    set "FAILED_STEPS=!FAILED_STEPS! 02_ARF_IMC_stratification 03_ARF_IMC_initial_analysis 04_ARF_IMC_generate_local_coeff"
    goto :summary
)

set /a STEP+=1
echo [!STEP!/!TOTAL!] 02_ARF_IMC_stratification
echo [!STEP!/!TOTAL!] 02_ARF_IMC_stratification >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 02_ARF_IMC_stratification.R > "!LOG_DIR!\02_ARF_IMC_stratification_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\02_ARF_IMC_stratification_!TIMESTAMP!.log"
type "!LOG_DIR!\02_ARF_IMC_stratification_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "02_ARF_IMC_stratification"

set /a STEP+=1
echo [!STEP!/!TOTAL!] 03_ARF_IMC_initial_analysis
echo [!STEP!/!TOTAL!] 03_ARF_IMC_initial_analysis >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 03_ARF_IMC_initial_analysis.R > "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log"
type "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "03_ARF_IMC_initial_analysis"

set /a STEP+=1
echo [!STEP!/!TOTAL!] 04_ARF_IMC_generate_local_coeff
echo [!STEP!/!TOTAL!] 04_ARF_IMC_generate_local_coeff >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 04_ARF_IMC_generate_local_coeff.R > "!LOG_DIR!\04_ARF_IMC_generate_local_coeff_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\04_ARF_IMC_generate_local_coeff_!TIMESTAMP!.log"
type "!LOG_DIR!\04_ARF_IMC_generate_local_coeff_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "04_ARF_IMC_generate_local_coeff"
goto :summary

REM ════════════════════════════════════════════════════════════════════════
:round2
where Rscript >nul 2>&1
if errorlevel 1 (
    echo WARNING: Rscript not found.
    echo Run manually: cd code ^&^& Rscript 05_ARF_IMC_generate_offsets.R
    set "FAILED_STEPS=!FAILED_STEPS! 05_ARF_IMC_generate_offsets"
    goto :summary
)
set /a STEP+=1
echo [!STEP!/!TOTAL!] 05_ARF_IMC_generate_offsets
echo [!STEP!/!TOTAL!] 05_ARF_IMC_generate_offsets >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 05_ARF_IMC_generate_offsets.R > "!LOG_DIR!\05_ARF_IMC_generate_offsets_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\05_ARF_IMC_generate_offsets_!TIMESTAMP!.log"
type "!LOG_DIR!\05_ARF_IMC_generate_offsets_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "05_ARF_IMC_generate_offsets"
goto :summary

REM ════════════════════════════════════════════════════════════════════════
:round3
where Rscript >nul 2>&1
if errorlevel 1 (
    echo WARNING: Rscript not found.
    echo Run manually: cd code ^&^& Rscript 06_ARF_IMC_generate_counterfactual.R
    set "FAILED_STEPS=!FAILED_STEPS! 06_ARF_IMC_generate_counterfactual"
    goto :summary
)
set /a STEP+=1
echo [!STEP!/!TOTAL!] 06_ARF_IMC_generate_counterfactual
echo [!STEP!/!TOTAL!] 06_ARF_IMC_generate_counterfactual >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 06_ARF_IMC_generate_counterfactual.R > "!LOG_DIR!\06_ARF_IMC_generate_counterfactual_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\06_ARF_IMC_generate_counterfactual_!TIMESTAMP!.log"
type "!LOG_DIR!\06_ARF_IMC_generate_counterfactual_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "06_ARF_IMC_generate_counterfactual"
goto :summary

REM ════════════════════════════════════════════════════════════════════════
:round4
set /a STEP+=1
echo [!STEP!/!TOTAL!] 03_ARF_IMC_initial_analysis
echo [!STEP!/!TOTAL!] 03_ARF_IMC_initial_analysis >> "!LOG_FILE!"
set "START_TIME=%time%"
call :get_seconds "!START_TIME!" START_S
Rscript --vanilla 03_ARF_IMC_initial_analysis.R > "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log" 2>&1
set "EXIT_CODE=!errorlevel!"
type "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log"
type "!LOG_DIR!\03_ARF_IMC_initial_analysis_!TIMESTAMP!.log" >> "!LOG_FILE!"
call :report_step "03_ARF_IMC_initial_analysis"
goto :summary

REM ════════════════════════════════════════════════════════════════════════
:summary
echo.
if defined FAILED_STEPS (
    echo FAILED STEPS:!FAILED_STEPS!
    echo FAILED STEPS:!FAILED_STEPS! >> "!LOG_FILE!"
) else (
    echo All steps completed successfully.
    echo All steps completed successfully. >> "!LOG_FILE!"
)
echo Finished: %date% %time%
echo Finished: %date% %time% >> "!LOG_FILE!"
exit /b 0

REM ════════════════════════════════════════════════════════════════════════
REM  report_step — report elapsed time and track failures
REM ════════════════════════════════════════════════════════════════════════
:report_step
set "END_TIME=%time%"
call :get_seconds "!END_TIME!" END_S
set /a "ELAPSED=END_S-START_S"
if !ELAPSED! lss 0 set /a "ELAPSED=ELAPSED+86400"

if !EXIT_CODE! equ 0 (
    echo   Done in !ELAPSED!s
    echo   Done in !ELAPSED!s >> "!LOG_FILE!"
) else (
    echo   FAILED after !ELAPSED!s
    echo   FAILED after !ELAPSED!s >> "!LOG_FILE!"
    set "FAILED_STEPS=!FAILED_STEPS! %~1"
)
echo.
exit /b 0

REM ── convert time string to seconds ──────────────────────────────────────
:get_seconds
set "T=%~1"
for /f "tokens=1-4 delims=:." %%a in ("!T!") do (
    set /a "%~2=(%%a)*3600 + (1%%b-100)*60 + (1%%c-100)"
)
exit /b 0
