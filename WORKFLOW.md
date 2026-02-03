# MFCL Workflow - Quick Start Guide

## Model Configuration

Edit `config.R` to define your models:

```r
MODELS <- list(
  base = list(
    name = "base",
    description = "Base case model",
    inputs_dir = "mfcl/inputs/2026",
    exec_mode = "par",  # or "doitall"
    mfcl_version = "2026_01_22_vsn2278",
    par_input = "11.par",
    par_output = "12.par",
    mfcl_switches = "-switch 1 1 1 1"
  ),
  M1 = list(
    name = "M1",
    description = "Sensitivity M1",
    inputs_dir = "mfcl/inputs/2023",
    exec_mode = "doitall",
    mfcl_version = "2023",
    doitall_script = "doitall.sh"
  )
)
```

## Launch Jobs (Simple Method)

Use the wrapper scripts for easy job submission:

```bash
# Launch model runs
./launch_model.sh                    # Run base model
./launch_model.sh --model=M1         # Run specific model
./launch_model.sh --all              # Run all models

# Launch hessian calculation
./launch_hessian.sh                  # Base model, 200 parts
./launch_hessian.sh --parts=150      # Custom number of parts
./launch_hessian.sh --all            # All models

# Launch profile likelihood
./launch_prof.sh                     # Default scalers: 100,90,80,70,60,50
./launch_prof.sh --scalers=70,80,90,100
./launch_prof.sh --all

# Launch jitter analysis
./launch_jitter.sh                   # Default 100 runs
./launch_jitter.sh --runs=50
./launch_jitter.sh --all

# Fetch results from Condor
./fetch.sh develop/Feb_03_2026_model
./fetch.sh develop/Feb_03_2026_hessian --job=hessian
./fetch.sh develop/Feb_03_2026_all --job=all
```

## Launch Jobs (Advanced Method)

Use `launch.R` directly for more control:

```bash
# Model runs
Rscript launch.R model
Rscript launch.R model --model=base
Rscript launch.R model --model=base,M1,M2
Rscript launch.R model --model=all

# Multiple job types
Rscript launch.R model,hessian --model=base
Rscript launch.R all --model=all

# Custom remote directory
Rscript launch.R model --dir=develop/test_run

# Hessian with custom parts
Rscript launch.R hessian --nsplit=150

# Profile with custom scalers
Rscript launch.R prof --scalers=70,80,90,100

# Jitter with custom runs
Rscript launch.R jitter --njitter=50

# Local testing
Rscript launch.R model --local
```

## Fetch Results

```bash
# Fetch model results
Rscript fetch_results.R develop/Feb_03_2026_model

# Fetch hessian results
Rscript fetch_results.R develop/Feb_03_2026_hessian --job=hessian --parts=200

# Fetch profile results
Rscript fetch_results.R develop/Feb_03_2026_prof --job=prof

# Fetch jitter results
Rscript fetch_results.R develop/Feb_03_2026_jitter --job=jitter

# Fetch all job types
Rscript fetch_results.R develop/Feb_03_2026_all --job=all

# Fetch specific model only
Rscript fetch_results.R develop/Feb_03_2026_model --model=base

# Or use the simple wrapper
./fetch.sh develop/Feb_03_2026_model
./fetch.sh develop/Feb_03_2026_all --job=all
```

## Project Structure

```
.
├── config.R                  # Central configuration for all models
├── launch.R                  # Main launch script
├── launch_model.sh          # Simple wrapper for model runs
├── launch_hessian.sh        # Simple wrapper for hessian
├── launch_prof.sh           # Simple wrapper for profile
├── launch_jitter.sh         # Simple wrapper for jitter
├── fetch.sh                 # Simple wrapper for fetching results
├── fetch_results.R          # Fetch results from Condor
├── scripts/
│   ├── run_model.R          # Run MFCL model
│   ├── run_hessian.R        # Calculate Hessian part
│   ├── run_prof.R           # Profile likelihood
│   └── run_jitter.R         # Jitter analysis
├── mfcl/
│   ├── exe/                 # MFCL executables
│   └── inputs/              # Input files for different models
│       ├── 2026/            # Base model inputs
│       ├── 2023/            # Alternative inputs
│       └── 2023_6region/    # Sensitivity inputs
└── model/                   # Output directory
    ├── base/                # Base model results
    ├── M1/                  # M1 model results
    └── M2/                  # M2 model results
```

## Model Settings Explained

Each model in `config.R` can have:

- **inputs_dir**: Directory with input files (.frq, .tag, .age_length, etc.)
- **exec_mode**: 
  - `"par"`: Run single MFCL command with .par files
  - `"doitall"`: Run full doitall.sh script
- **mfcl_version**: MFCL executable version to use
- **par_input/par_output**: PAR files for par mode
- **mfcl_switches**: Command line switches for par mode
- **doitall_script**: Script name for doitall mode

## Examples

### Run base model on Condor
```bash
./launch_model.sh
# or
Rscript launch.R model --model=base
```

### Run all models with hessian
```bash
./launch_model.sh --all
./launch_hessian.sh --all
# or
Rscript launch.R model,hessian --model=all
```

### Test locally before Condor
```bash
Rscript launch.R model --local
```

### Fetch and analyze
```bash
./fetch.sh develop/Feb_03_2026_model
# Results will be in model/base/, model/M1/, model/M2/
```
