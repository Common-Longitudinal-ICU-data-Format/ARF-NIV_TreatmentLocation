#!/usr/bin/env bash
# ════════════════════════════════════════════════════════════════════════════════
#  run_pipeline.sh — Execute the full CLIF treatment location analysis pipeline
# ════════════════════════════════════════════════════════════════════════════════
set -euo pipefail

# ── colours ──────────────────────────────────────────────────────────────────
GREEN=$'\033[32m'; RED=$'\033[31m'; CYAN=$'\033[36m'; YELLOW=$'\033[33m'
BOLD=$'\033[1m'; RESET=$'\033[0m'

# ── paths ────────────────────────────────────────────────────────────────────
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_DIR="${PROJECT_ROOT}/logs"
LOG_FILE="${LOG_DIR}/pipeline_${TIMESTAMP}.log"
mkdir -p "$LOG_DIR"

# ── logging ──────────────────────────────────────────────────────────────────
log() { echo -e "$1" | tee -a "$LOG_FILE"; }

log "${CYAN}${BOLD}CLIF Treatment Location ARF Pipeline${RESET}"
log "Started: $(date)"
log "Log: ${LOG_FILE}"
log ""

# ── environment (uv) ─────────────────────────────────────────────────────────
if ! command -v uv >/dev/null 2>&1; then
  log "${RED}uv not found. Install it: https://docs.astral.sh/uv/getting-started/installation/${RESET}"
  exit 1
fi

log "Syncing dependencies with uv..."
uv sync --project "${PROJECT_ROOT}" 2>&1 | tee -a "$LOG_FILE"
log "${GREEN}Environment ready${RESET}"
log ""

# ── loading R module ─────────────────────────────────────────────────────────
module load R


# ── helpers ──────────────────────────────────────────────────────────────────
export PYTHONUNBUFFERED=1
export MPLBACKEND=Agg
export PYTHONPATH="${PROJECT_ROOT}/code:${PYTHONPATH:-}"

FAILED_STEPS=()
STEP=0
TOTAL=4  # 2 Python + 2 R

# This function runs a given Python or R file
run_step() {
  local name="$1"; shift
  STEP=$((STEP + 1))
  log "${CYAN}${BOLD}[${STEP}/${TOTAL}] ${name}${RESET}"
  local start_s=$SECONDS
  local step_log="${LOG_DIR}/${name// /_}_${TIMESTAMP}.log"

  if "$@" 2>&1 | tee "$step_log" | tee -a "$LOG_FILE"; then
    local elapsed=$(( SECONDS - start_s ))
    log "${GREEN}  Done in ${elapsed}s${RESET}"
  else
    local elapsed=$(( SECONDS - start_s ))
    log "${RED}  FAILED after ${elapsed}s${RESET}"
    FAILED_STEPS+=("$name")
  fi
  log ""
}

# ── pipeline (cwd = code/ so relative paths work) ───────────────────────────
cd "${PROJECT_ROOT}/code"

# Request which run is happening
read -p "${RED}Which round are you running (i.e., 1 = cohort selection + initial analysis, 2 = using global coefficients, 3 = using global intercepts):${RESET} " run_num

# Run only the files relevant for this run
case $run_num in
    1) 
        if command -v Rscript >/dev/null 2>&1; then
            run_step "00_ARF_IMC_cohort"       Rscript --vanilla 00_ARF_IMC_cohort.R
        else
          log "${YELLOW}Rscript not found — skipping R analysis.${RESET}"
          log "${YELLOW}Run manually: cd code && Rscript 00_ARF_IMC_cohort.R${RESET}"
          FAILED_STEPS+=("00_ARF_IMC_cohort (Rscript not found)")
        fi

        run_step "01_ARF_IMC_sepsis_indicators"       uv run --project "${PROJECT_ROOT}" python 01_ARF_IMC_sepsis_indicators.py
        
        
        if command -v Rscript >/dev/null 2>&1; then
            run_step "02_ARF_IMC_stratification"       Rscript --vanilla 02_ARF_IMC_stratification.R
            run_step "03_ARF_IMC_initial_analysis"       Rscript --vanilla 03_ARF_IMC_initial_analysis.R
            run_step "04_ARF_IMC_generate_local_coeff"       Rscript --vanilla 04_ARF_IMC_generate_local_coeff.R
        else
          log "${YELLOW}Rscript not found — skipping R analysis.${RESET}"
          log "${YELLOW}Run manually: cd code && Rscript 02_ARF_IMC_stratification.R && Rscript 03_ARF_IMC_initial_analysis.R && Rscript 04_ARF_IMC_generate_local_coeff.R${RESET}"
          FAILED_STEPS+=("02_ARF_IMC_stratification (Rscript not found)")
          FAILED_STEPS+=("03_ARF_IMC_initial_analysis (Rscript not found)")
          FAILED_STEPS+=("04_ARF_IMC_generate_local_coeff (Rscript not found)")
        fi
        
        ;;
    2)
        if command -v Rscript >/dev/null 2>&1; then
            run_step "05_ARF_IMC_generate_offsets"       Rscript --vanilla 05_ARF_IMC_generate_offsets.R
        else
          log "${YELLOW}Rscript not found — skipping R analysis.${RESET}"
          log "${YELLOW}Run manually: cd code && Rscript 05_ARF_IMC_generate_offsets.R${RESET}"
          FAILED_STEPS+=("05_ARF_IMC_generate_offsets (Rscript not found)")
        fi
        ;;
    3)
        if command -v Rscript >/dev/null 2>&1; then
            run_step "06_ARF_IMC_generate_counterfactual"       Rscript --vanilla 06_ARF_IMC_generate_counterfactual.R
        else
          log "${YELLOW}Rscript not found — skipping R analysis.${RESET}"
          log "${YELLOW}Run manually: cd code && Rscript 06_ARF_IMC_generate_counterfactual.R${RESET}"
          FAILED_STEPS+=("06_ARF_IMC_generate_counterfactual (Rscript not found)")
        fi
        ;;
    4) 
        run_step "03_ARF_IMC_initial_analysis"       Rscript --vanilla 03_ARF_IMC_initial_analysis.R
        ;;
    *)
        echo "Invalid choice. Exiting."
        exit 1
        ;;
esac