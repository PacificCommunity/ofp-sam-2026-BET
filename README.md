# BET 2026 Assessment Project

This repository contains the code and configuration for the 2026 Bigeye Tuna (BET) assessment.

## Directory Structure

* **Root**: Main entry points and configuration.
    * `config.R`: Central configuration for models, paths, and settings.
    * `launch.R`: Main CLI tool for submitting jobs to Condor or running locally.
    * `run_*_condor.R`: Interactive scripts for running jobs from RStudio.
    * `makefile`: Shortcuts for common tasks.
* **scripts/**: Worker scripts executed by Condor (backend logic).
    * `run_model.R`: Runs the MFCL model.
    * `run_prof.R`: Runs profile likelihood.
    * `run_hessian.R`: Runs Hessian calculations.
    * `run_jitter.R`: Runs jitter analysis.
    * `fetch_results.R`: Downloads results from Condor.
* **mfcl/**: MFCL executables and input files.
* **model/**: Model output directory (local).
* **tools/**: Helper functions and utilities.
* **archive/**: Deprecated or unused scripts.

## Workflow

### 1. Interactive Mode (RStudio)

Use the `run_*_condor.R` scripts in the root directory. Open them in RStudio and run line-by-line or source them.

* `run_model_condor.R`: Submit model runs.
* `run_prof_condor.R`: Submit profile runs.
* `run_hessian_condor.R`: Submit Hessian jobs.
* `run_jitter_condor.R`: Submit jitter jobs.

### 2. Command Line (CLI)

Use `launch.R` for submitting jobs or running locally.

```bash
# Submit model to Condor
Rscript launch.R model

# Run locally (for testing)
Rscript launch.R model --local
```

### 3. Using Make

Shortcuts are available in the `makefile`:

```bash
# Run local test
make local-model

# Fetch results from Condor
make fetch remote_dir=develop/Feb_03_2026_model
```

## Configuration

Edit `config.R` to change model settings, paths, and Condor configuration.

