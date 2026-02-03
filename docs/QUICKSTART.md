# MFCL Assessment - Quick Start Guide

## 🎯 What Changed?

### Before (Old Structure)
```
❌ Hard-coded "bet" everywhere
❌ Multiple launch_condor_*.R files
❌ Duplicated code
❌ Confusing to adapt for other species
```

### After (New Structure)
```
✅ Species-agnostic configuration
✅ Single unified launcher
✅ Clean separation of concerns
✅ Easy to use for any species
```

## 🚀 Quick Start

### Step 1: Check Configuration

```bash
make config
```

This shows:
- Species: bet
- Input files status (✓ or ✗)
- Directories
- MFCL executable

### Step 2: Edit for Your Species (if needed)

Edit `config.R`:
```r
SPECIES <- "yft"  # Change from "bet" to "yft"
```

That's it! All file names automatically update:
- `yft.frq`, `yft.tag`, `yft.age_length`, etc.

### Step 3: Run Locally (Test)

```bash
# Quick model test
make model

# Test Hessian (single part)
make hessian

# Or use launch.R for local test
Rscript launch.R model --local
```

### Step 4: Submit to Condor

```bash
# Submit model
Rscript launch.R model

# Full Hessian (100 parallel jobs)
Rscript launch.R hessian

# Profile likelihood
Rscript launch.R prof

# Everything
Rscript launch.R all
```

## 📁 New File Organization

```
config.R                    ← Edit this for your assessment
launch.R                    ← Use this to submit jobs

scripts/
├── run_model.R            ← No hardcoding!
├── run_hessian.R          ← Uses config.R
├── run_prof.R             ← Clean and simple
└── run_jitter.R           

tools/
├── collate_hessian_mfcl.R ← Stitch Hessian parts
├── verify_hessian.R       ← Check quality
└── read_hessian.R         ← Read binary files

makefile                    ← Updated with new structure
```

## 🔧 Common Tasks

### Run Different Model Scenarios

Add to `config.R`:
```r
MODELS <- list(
  base = list(...),
  sensitivity1 = list(...),
  sensitivity2 = list(...)
)
```

Then:
```bash
Rscript launch.R model --model=sensitivity1
```

### Customize Hessian

```bash
# Fewer parts (faster, less parallel)
Rscript launch.R hessian --nsplit=50

# More parts (more parallel)
Rscript launch.R hessian --nsplit=200
```

### Customize Profile

```bash
# Custom scalers
Rscript launch.R prof --scalers=0.7,0.8,0.9,1.0,1.1,1.2,1.3

# Different parameter
# Edit scripts/run_prof.R to change profiled parameter
```

### After Jobs Complete

```bash
# Collate Hessian
make stitch-hessian

# Verify quality
make verify-hessian

# Generate plots
make plot
```

## 🔍 Verification

Check Hessian quality:
```bash
Rscript tools/verify_hessian.R
```

Looks for:
- ✓ Symmetry
- ✓ Positive definite
- ✓ Good condition number
- ✓ Reasonable standard errors

## 📊 Workflow Example

```bash
# 1. Configure
edit config.R  # Set SPECIES, paths, etc.

# 2. Test locally
make model

# 3. Submit full run
Rscript launch.R model

# 4. Calculate uncertainties
Rscript launch.R hessian

# 5. Download results
# (wait for Condor jobs to finish)
library(CondorBox)
BatchFileHandler(action='fetch', fetch_dir='path/to/results')

# 6. Collate and verify
make stitch-hessian
make verify-hessian

# 7. Additional analyses
Rscript launch.R prof
Rscript launch.R jitter
```

## 🐛 Troubleshooting

### "File not found" errors

Check paths in `config.R`:
```r
INPUTS_DIR <- file.path(BASE_DIR, "mfcl/inputs/2026")
```

Verify files exist:
```bash
make check
```

### Condor jobs fail

Test locally first:
```bash
Rscript launch.R model --local
```

Check logs in output directories.

### Wrong species files

Check `SPECIES` in `config.R`:
```r
SPECIES <- "bet"  # Must match your .frq file prefix
```

## 💡 Key Concepts

### 1. Single Source of Truth

All species-specific settings in `config.R`:
- File names
- Directories
- Resource requests
- Analysis parameters

### 2. Unified Interface

One launcher for everything:
```bash
Rscript launch.R <job_type> [options]
```

### 3. Clean Separation

- `config.R` → Configuration
- `scripts/` → Execution
- `tools/` → Analysis
- `launch.R` → Job submission

### 4. Local Testing

Always test locally before Condor:
```bash
make model              # Local test
Rscript launch.R model  # Condor submission
```

## 📚 More Info

- `docs/WORKFLOW.md` - Detailed workflow
- `config.R` - All configuration options
- `launch.R --help` - Command-line options

## 🎉 Benefits

1. **No hardcoding** - Works for any species
2. **Less duplication** - One launcher, not five
3. **Easier maintenance** - Change once in config.R
4. **Better organized** - Clear file structure
5. **Safer** - Test locally before Condor
6. **More flexible** - Easy to customize

## ❓ Questions?

1. What species? → Edit `SPECIES` in config.R
2. Where are inputs? → Check `INPUTS_DIR` in config.R
3. How to submit? → `Rscript launch.R <type>`
4. Test first? → `make <target>` or `--local` flag
5. After jobs? → `make stitch-hessian` + `make verify-hessian`
