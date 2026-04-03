library(rix)

rix(
  r_ver = "latest-upstream",
  r_pkgs = c(
    "duckdb", 
    "lubridate", 
    "tidyverse", 
    "dplyr",
    "table1", 
    "broom", 
    "arrow", 
    "rvest", 
    "readr", 
    "fst", 
    "data.table", 
    "collapse", 
    "tictoc",
    "yaml",
    "rprojroot",
    "zoo",
    "comorbidity",
    "gtsummary@2.0.4",
    "gt",
    "icd.data",
    "rlang",
    "stringr",
    "lmtest",
    "sandwich",
    "marginaleffects@0.25.1",
    "ordinal",
    "logistf"
  ),
  ide = "none",
  project_path = ".",
  overwrite = TRUE,
  shell_hook = "R"
)