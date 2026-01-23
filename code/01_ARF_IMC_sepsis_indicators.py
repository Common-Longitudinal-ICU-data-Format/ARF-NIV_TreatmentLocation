from pathlib import Path
import yaml
import pandas as pd
from clifpy.utils import compute_ase
# -----------------------------
# Find project root (directory containing "config/")
# -----------------------------
def find_project_root(start_path=None, target_dir="config"):
    start_path = Path(start_path or Path.cwd()).resolve()
    for parent in [start_path, *start_path.parents]:
        if (parent / target_dir).is_dir():
            return parent
    raise FileNotFoundError(f'Could not find "{target_dir}" directory in parents')

project_root = find_project_root()

# -----------------------------
# Read YAML config
# -----------------------------
with open(project_root / "config" / "config.yaml", "r") as f:
    config = yaml.safe_load(f)

tables_location  = config["tables_location"]
project_location = config["project_location"]
site             = config["institution"]
time_zone        = config["time_zone"]
filetype         = config["file_type"]


# -----------------------------
# Load hospital block key and extract hospitalization IDs
# -----------------------------
hospital_block_key = pd.read_csv(
    Path(project_location) / "private_tables" / "hospital_block_key.csv"
)

hospitalizations = (
    hospital_block_key["hospitalization_id"]
    .astype("str")
    .drop_duplicates()
    .tolist()
)

# Compute ASE for specific hospitalizations
ase_results = compute_ase(
    hospitalization_ids= hospitalizations,
    data_directory= tables_location,
    filetype=filetype,
    timezone=time_zone,
    apply_rit=True,           # Apply 14-day Repeat Infection Timeframe
    include_lactate=False,    # Include lactate criterion
    verbose=True              # Show detailed progress
)

ase_results.to_csv(
    Path(project_location) / "private_tables" / "sepsis_indicators.csv",
    index=False
)