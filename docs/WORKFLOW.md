# MFCL Assessment Workflow

Refactored structure for efficient, species-agnostic MFCL model runs.

## 📁 Directory Structure

```
├── config.R                 # Central configuration (species, paths, settings)
├── launch.R                 # Unified Condor/local job launcher
├── makefile                 # Local execution shortcuts
│
├── scripts/                 # Execution scripts (no hardcoding!)
│   ├── run_model.R         # Run MFCL model
│   ├── run_hessian.R       # Calculate single Hessian part
│   ├── run_prof.R          # Profile likelihood
│   └── run_jitter.R        # Jitter analysis
│
├── tools/                   # Analysis utilities
│   ├── collate_hessian_mfcl.R    # Stitch Hessian parts (MFCL method)
│   ├── verify_hessian.R          # Check Hessian quality
│   ├── read_hessian.R            # Read binary Hessian file
│   └── ...
│
├── mfcl/
│   ├── exe/                # MFCL executables
│   └── inputs/             # Input files (frq, tag, age_length, etc.)
│
├── model/                  # Model outputs
│   └── base/
│       ├── hessian/        # Hessian calculations
│       ├── prof/           # Profile likelihood
│       └── jitter/         # Jitter results
│
└── docs/                   # Documentation
```

## 🚀 Quick Start

### 1. Configure

Edit `config.R`:
```r
SPECIES <- "bet"  # Change for different species
ASSESSMENT_YEAR <- 2026
```

All species-specific settings (file names, paths) are centralized here.

### 2. Run Locally

```bash
# Run model
make model

# Calculate Hessian (single part for testing)
make hessian

# Profile likelihood
make prof

# Jitter analysis
make jitter
```

### 3. Submit to Condor

```bash
# Run model on Condor
Rscript launch.R model

# Full Hessian calculation (100 parallel jobs)
Rscript launch.R hessian

# Profile likelihood
Rscript launch.R prof

# Jitter analysis
Rscript launch.R jitter

# Run everything
Rscript launch.R all
```

### 4. Advanced Options

```bash
# Use different model configuration
Rscript launch.R model --model=m2_s20

# Custom Hessian splits
Rscript launch.R hessian --nsplit=50

# Custom profile scalers
Rscript launch.R prof --scalers=0.8,0.9,1.0,1.1,1.2

# More jitter runs
Rscript launch.R jitter --njitter=100

# Local test run
Rscript launch.R model --local
```

## 📊 Workflow

### Typical Assessment Sequence

```
1. MODEL       →  Run base model(s)
   ↓
2. HESSIAN     →  Calculate parameter uncertainties
   ↓
3. VERIFY      →  Check Hessian quality
   ↓
4. PROFILE     →  Profile key parameters
   ↓
5. JITTER      →  Test model convergence
```

### After Condor Jobs Complete

```bash
# Download results
Rscript -e "
  library(CondorBox)
  BatchFileHandler(action='fetch', fetch_dir='path/to/remote/dir')
"

# Collate Hessian parts
make stitch-hessian

# Verify Hessian quality
Rscript tools/verify_hessian.R

# Analyze results
make plot
```

## 🔧 Configuration

### config.R - Main Settings

- **Species info**: `SPECIES`, `ASSESSMENT_YEAR`, `REGION`
- **Directories**: Automatically configured based on species
- **File names**: Generated from species code (no hardcoding!)
- **Condor settings**: Resource requests, Docker image, etc.
- **Analysis parameters**: Hessian splits, profile scalers, jitter runs

### Multiple Model Scenarios

Define in `config.R`:

```r
MODELS <- list(
  base = list(
    name = "base",
    switches = "-switch 4 1 1 1 5 1 124 1"
  ),
  
  high_M = list(
    name = "high_M",
    switches = "-switch 4 1 1 1 5 1 124 1 ..."
  )
)
```

Then run:
```bash
Rscript launch.R model --model=high_M
```

## 🎯 Key Features

### ✅ No Hardcoding
- All species-specific values in `config.R`
- Easy to adapt for YFT, SKJ, ALB, etc.

### ✅ Unified Interface
- Single `launch.R` for all job types
- Consistent command-line interface
- Local testing before Condor submission

### ✅ Organized Structure
- Clear separation: config → scripts → tools
- Dedicated output directories
- Comprehensive logging

### ✅ Workflow Support
- Sequential execution (model → hessian → prof → jitter)
- Parallel job management
- Result collation and verification

## 📝 Examples

### Different Species

```r
# In config.R
SPECIES <- "yft"  # Change to yellowfin tuna

# File names automatically become:
# yft.frq, yft.tag, yft.age_length, etc.
```

### Grid of Models

```bash
# Submit multiple model configurations
for model in base high_M low_h; do
  Rscript launch.R model --model=$model
done
```

### Sensitivity Analysis

```bash
# Profile multiple parameters
Rscript launch.R prof --scalers=0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5

# Extensive jitter testing
Rscript launch.R jitter --njitter=200
```

## 🔍 Verification

After Hessian calculation:

```bash
Rscript tools/verify_hessian.R model/base/hessian/bet.hes model/base/12.par
```

Checks:
- ✓ Matrix symmetry
- ✓ Positive definiteness  
- ✓ Eigenvalue spectrum
- ✓ Condition number
- ✓ Parameter standard errors

## 📚 Documentation

- `docs/WORKFLOW.md` - Detailed workflow guide
- `docs/CONFIG.md` - Configuration options
- `docs/HESSIAN.md` - Hessian calculation details

## 🐛 Troubleshooting

**Files not found?**
- Check `config.R` paths
- Verify input files exist in `mfcl/inputs/`

**Condor jobs fail?**
- Test locally first: `--local` flag
- Check logs in remote directory
- Verify Docker image accessibility

**Hessian not positive definite?**
- Check model convergence
- Review gradient values
- Consider parameter transformations
