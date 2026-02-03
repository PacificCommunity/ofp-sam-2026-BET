# MFCL Assessment Workflow - BET 2026

## Quick Start

### Configuration
Edit [config.R](config.R) to set:
- `SPECIES` - Species code (e.g., "bet")
- `MFCL_VERSION` - MFCL version ("2026", "2023", or custom)
- `EXEC_MODE` - Execution mode ("par" or "doitall")

### Local Testing
```bash
make test-model      # Test model run
make test-hessian    # Test Hessian (part 1)
make test-prof       # Test profile (scaler 100%)
make test-jitter     # Test jitter (seed 1)
```

Or use the test script directly:
```bash
./test_local.R model
./test_local.R prof 80
```

### Condor Submission
```bash
Rscript launch.R model                          # Submit model run
Rscript launch.R hessian --nsplit=100           # Submit Hessian (100 parts)
Rscript launch.R prof --scalers="70,80,90,100,110,120"
Rscript launch.R jitter --njitter=50
```

## Directory Structure

```
├── config.R              # Central configuration
├── helpers.R             # Helper functions
├── launch.R              # Unified job launcher
├── test_local.R          # Quick local testing
├── makefile              # Make targets
│
├── scripts/              # Execution scripts
│   ├── run_model.R       # Model execution
│   ├── run_hessian.R     # Hessian calculation
│   ├── run_prof.R        # Profile likelihood
│   └── run_jitter.R      # Jitter analysis
│
├── tools/                # Analysis tools
│   ├── ProfLike_utils.R  # Profile likelihood utilities
│   └── plot_utils.R      # Plotting utilities
│
├── mfcl/                 # MFCL files
│   ├── exe/              # Executables
│   └── inputs/2026/      # Input files (.frq, .ini, .tag, mfcl.cfg)
│
├── model/                # Model outputs
│   ├── base/             # Base model run
│   ├── hessian/          # Hessian parts
│   ├── prof/             # Profile likelihood
│   └── jitter/           # Jitter runs
│
├── collate_hessian_mfcl.R    # Stitch Hessian parts
├── verify_hessian.R          # Verify Hessian quality
├── read_hessian.R            # Read binary Hessian
│
└── archive/              # Old/obsolete files
```

## Analysis Workflow

### 1. Model Run
```bash
# Local test
make test-model

# Condor submission
Rscript launch.R model
```

### 2. Hessian Calculation
```bash
# Submit parallel jobs (100 parts)
Rscript launch.R hessian --nsplit=100

# After completion, stitch parts
Rscript collate_hessian_mfcl.R

# Verify Hessian quality
Rscript verify_hessian.R
```

### 3. Profile Likelihood
Profile is fixed to **average biomass** (quantity_type=2).

Configure in [config.R](config.R):
```r
PROF_START_YEAR <- 0    # 0 = entire period
PROF_END_YEAR <- 0      # 0 = entire period
PROF_SCALERS <- seq(50, 130, by = 10)
```

Submit:
```bash
Rscript launch.R prof
```

### 4. Jitter Analysis
```bash
Rscript launch.R jitter --njitter=50
```

## Configuration Guide

### Basic Settings

```r
## Species and year
SPECIES <- "bet"
ASSESSMENT_YEAR <- 2026

## MFCL version
MFCL_VERSION <- "2026"  # "2026", "2023", or custom
```

### Execution Modes

**PAR mode** (default) - Use .par files:
```r
EXEC_MODE <- "par"
PAR_INPUT <- "11.par"
PAR_OUTPUT <- "12.par"
MFCL_SWITCHES <- "-switch 1 1 1 1"
```

**DOITALL mode** - Run doitall.sh:
```r
EXEC_MODE <- "doitall"
DOITALL_SCRIPT <- "doitall.sh"
```

### Profile Settings

Profile uses average biomass penalty method (ProfLike approach):

```r
PROF_QUANTITY_TYPE <- 2  # 1=depletion, 2=average biomass (fixed)
PROF_START_YEAR <- 0     # 0 = entire period start
PROF_END_YEAR <- 0       # 0 = entire period end
PROF_SCALERS <- seq(50, 130, by = 10)  # Percentage multipliers
PROF_PENALTIES <- c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000)
PROF_REPS <- c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500)
```

**Time period examples:**
- Entire period: `PROF_START_YEAR = 0, PROF_END_YEAR = 0`
- Last 150-5 timesteps: `PROF_START_YEAR = 150, PROF_END_YEAR = 5`

### Condor Settings

```r
CONDOR_USER <- "kyuhank"
CONDOR_HOST <- Sys.getenv("NOU_CONDOR")
GITHUB_PAT <- Sys.getenv("GIT_PAT")
GITHUB_REPO <- "ofp-sam-2026-bet"
GITHUB_BRANCH <- "develop_lik"
DOCKER_IMAGE <- "ghcr.io/pacificcommunity/bet-2026:v1.2"
```

## File Descriptions

### Core Files
- **config.R** - All configuration settings (140 lines, clean and simple)
- **helpers.R** - Utility functions (`get_mfcl_exe`, `get_input_files`, etc.)
- **launch.R** - Unified job launcher (replaces 5 old launch_condor_*.R files)
- **test_local.R** - Quick local testing without Condor
- **makefile** - Convenient make targets

### Execution Scripts (scripts/)
- **run_model.R** - Model execution (supports par/doitall modes)
- **run_hessian.R** - Calculate single Hessian part
- **run_prof.R** - Profile likelihood (average biomass)
- **run_jitter.R** - Jitter analysis (convergence testing)

### Analysis Tools
- **collate_hessian_mfcl.R** - Stitch Hessian parts using MFCL
- **verify_hessian.R** - Verify Hessian (symmetry, eigenvalues, covariance)
- **read_hessian.R** - Read MFCL binary Hessian files

### Input Files (mfcl/inputs/2026/)
- `bet.frq` - Frequency data
- `bet.ini` - Initial parameter file
- `bet.tag` - Tag data
- `bet.age_length` - Age-length key
- `mfcl.cfg` - MFCL configuration (always mfcl.cfg, not species-specific)

## Tips & Tricks

### Disable Workspace Save Prompt
Already configured in `.Rprofile`:
```r
options(save.image.defaults = "no")
```

### Change Species
Simply edit `SPECIES` in config.R:
```r
SPECIES <- "yft"  # Change from "bet" to "yft"
```
All file paths will update automatically.

### Check Condor Job Status
```bash
ssh kyuhank@condor_host 'condor_q'
```

### Download Results
```r
library(CondorBox)
bfh <- BatchFileHandler(
  action = 'fetch',
  fetch_dir = 'ofp-sam-2026-bet/runs/20260203/model'
)
```

### Debug MFCL Run
Check the log file:
```bash
cat model/base/mfcl_log.txt
```

## Common Issues

### "File not found" errors
- Check `INPUTS_DIR` path in config.R
- Verify all required files exist: .frq, .ini, .tag, .age_length, mfcl.cfg

### "Executable not found"
- Check `MFCL_VERSION` matches executable name
- Verify MFCL executable exists in `mfcl/exe/`

### Hessian assembly fails
- Ensure all parts completed successfully
- Check part files in `model/hessian/part_*/`
- Run `Rscript verify_hessian.R` for diagnostics

## Migration Notes

Obsolete files moved to `archive/`:
- `launch_condor_*.R` → Replaced by unified `launch.R`
- `run_*.R` (root) → Moved to `scripts/`
- Old config formats → Simplified to flat structure

See `docs/WORKFLOW.md` for detailed documentation.
