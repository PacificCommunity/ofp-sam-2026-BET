# Hessian Calculation Guide

## Overview

The Hessian calculation scripts enable parallel computation of the Hessian matrix for MFCL models. The calculation is split into multiple independent parts that can be run simultaneously on a cluster using Condor.

## Files

- `run_hessian.R`: Calculates a single part of the Hessian matrix
- `launch_condor_hessian.R`: Submits all Hessian parts to Condor for parallel execution
- `collate_hessian.R`: Combines partial results into a complete Hessian matrix

## Usage

### 1. Single Hessian Part (for testing)

Run a single part locally:

```bash
# Calculate part 1 of 20 total parts
hessian_part=1 nsplit=20 make hessian
```

Or with Docker:

```bash
make docker-hessian hessian_part=1 nsplit=20
```

### 2. Parallel Execution with Condor

Submit all parts to Condor:

```bash
Rscript launch_condor_hessian.R
```

This will:
- Split the Hessian calculation into 20 parts (configurable in the script)
- Submit each part as an independent Condor job
- Each job calculates rows of the Hessian matrix using parest_flags(223) and parest_flags(224)

Monitor job progress:

```bash
condor_q                    # View all jobs
condor_q <cluster_id>       # View specific cluster
condor_rm <cluster_id>      # Remove jobs if needed
```

### 3. Collate Results

After all parts complete, combine them into a single Hessian matrix:

```bash
Rscript collate_hessian.R
```

Or with make:

```bash
make collate-hessian
```

This will:
- Read all partial Hessian files from `model/base/hessian/part_*` directories
- Combine them into a complete matrix
- Save as binary file: `model/base/hessian/complete.hes`
- Save as RDS for R: `model/base/hessian/hessian_matrix.rds`
- Verify the result is symmetric

## Configuration

Edit `launch_condor_hessian.R` to change:

```r
nsplit <- 20                                    # Number of parts (more = finer parallelization)
program_path <- "mfcl/exe/mfclo64_2026_01_22_vsn2278"  # MFCL executable
base_dir <- "mfcl/inputs/2026"                 # Input files directory
model_dir <- "model/base"                       # Model output directory
```

## Output Structure

```
model/base/hessian/
├── submission_info.rds          # Condor submission details
├── collation_info.rds           # Collation metadata
├── complete.hes                 # Complete Hessian (binary)
├── hessian_matrix.rds           # Complete Hessian (R format)
├── part_1/
│   ├── hessian_info.rds        # Part 1 metadata
│   ├── hessian_1.par           # Part 1 output par
│   ├── *.hes                   # Part 1 Hessian rows
│   └── mfcl_hessian_log.txt    # Part 1 MFCL log
├── part_2/
│   └── ...
└── part_20/
    └── ...
```

## Technical Details

### Parameter Range Calculation

For `nsplit=20` and `npars=1000` parameters:
- Part 1: parameters 1-50
- Part 2: parameters 51-100
- ...
- Part 20: parameters 951-1000

### MFCL Switches

Each part runs with:
```
-switch 3 1 145 1 1 223 <start> 1 224 <end>
```

Where:
- `-switch 3`: Phase 3 (Hessian calculation)
- `1 145 1`: Standard flag for Hessian
- `1 223 <start>`: Starting parameter number
- `1 224 <end>`: Ending parameter number

### File Format

The Hessian is stored as a binary file with:
- Each row is `npars` double-precision values
- Total size: `npars × npars × 8 bytes`
- Can be read with `readBin()` in R

## Example Workflow

```bash
# 1. Run base model first
make model

# 2. Submit Hessian calculation to Condor
Rscript launch_condor_hessian.R

# 3. Monitor progress
condor_q

# 4. When all jobs complete, collate results
make collate-hessian

# 5. Use the Hessian in R
R
> hess <- readRDS("model/base/hessian/hessian_matrix.rds")
> dim(hess)
> eigen(hess)  # Check eigenvalues
```

## Troubleshooting

### Jobs not starting
- Check Condor status: `condor_status`
- Verify Docker image is available
- Check resource requirements in `launch_condor_hessian.R`

### Missing parts after collation
- Check which parts completed: `ls model/base/hessian/part_*/hessian_info.rds`
- Resubmit missing parts individually
- Check MFCL logs for errors

### Memory issues
- Increase `request_memory` in `launch_condor_hessian.R`
- Reduce `nsplit` for fewer, larger parts

## See Also

- MFCL User Guide Section A.4: Hessian Calculation in Parallel
- `run_prof.R`: Profile likelihood analysis
- `run_jitter.R`: Jitter analysis
