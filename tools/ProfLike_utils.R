# Function to generate a bash (.sh) file for running MFCL likelihood profile analysis

# Explanation of flags:
# age flag 32: estimate overall population scaling parameter
# par flag 187: no tag_rep file
# par flag 188: no ests.rep and plo.rep file
# fish flag 55: impact analysis
# par flag 346: activates penalty (1=depletion, 2=average biomass)
# par flag 347: target quantity
# par flag 348: weight of the penalty
# age flags 173,174: first and last periods for biomass calculation
# StartYr=nyears-af173+1
# EndYr=nyears-af174+1
# Default (i.e., 173=0, 174=0) means the whole year (i.e., StartYr=1 EndYear=nyears)

# 
# generate_proflike_script <- function(
#     Penalties = c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000),
#     Reps = c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500),
#     AgeFlags = c(Af173 = 150, Af174 = 5),
#     Prog = "../../mfcl/mfclo64",
#     Frq = "bet.frq",
#     Initp = "11.par",
#     Mults = c(90, 80, 70, 60, 50),
#     QuantityType = 2,  # 1 for depletion, 2 for average biomass
#     filename = "ProfLike.sh") {
# 
#   quantity_label <- ifelse(QuantityType == 1, "relative_depletion", "avg_bio")
# 
#   generate_switch <- function(pen, reps, target, weight, af173, af174, quant_type) {
#     sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s 1 346 %s 1 347 %s 1 348 %s 2 173 %s 2 174 %s",
#             reps, quant_type, target, pen, af173, af174)
#   }
# 
# 
#   bash_script <- c(
#     "#!/bin/bash",
#     "",
#     "# Define initial parameters",
#     sprintf("Pen1=%d", Penalties["Pen1"]),
#     sprintf("Pen2=%d", Penalties["Pen2"]),
#     sprintf("Pen3=%d", Penalties["Pen3"]),
#     sprintf("Reps1=%d", Reps["Reps1"]),
#     sprintf("Reps2=%d", Reps["Reps2"]),
#     sprintf("Reps3=%d", Reps["Reps3"]),
#     sprintf("Reps4=%d", Reps["Reps4"]),
#     sprintf("Reps5=%d", Reps["Reps5"]),
#     sprintf("Reps6=%d", Reps["Reps6"]),
#     sprintf("Af173=%d", AgeFlags["Af173"]),
#     sprintf("Af174=%d", AgeFlags["Af174"]),
#     sprintf("Prog=%s", Prog),
#     sprintf("Frq=%s", Frq),
#     sprintf("Initp=%s", Initp),
#     "",
#     #sprintf("cp $Initp %s.par", quantity_label),
#     "",
#     "function call_mf1 () {",
#     "  echo \"arg5=$5\"",
#     "  echo \"arg6=$6\"",
#     "  Temp=`bc -l <<< \"$5*$6/100\"`",
#     "  echo \"Temp=$Temp\"",
#     "  Target=`printf \"%.0f\" $Temp`",
#     "  echo \"Target=$Target\"",
#     "  if [ ! -f $4 ]; then",
#     "    echo \"file $4 does not exist\"",
#     sprintf("    $1 $2 $3 $4 \\\n    %s",
#             generate_switch("$7", "$8", "$Target", "$6", "$9", "$10", QuantityType)),
#     "  else",
#     "    echo \"file $4 exists already\"",
#     "  fi",
#     "}",
#     "",
#     sprintf("M0=0\nMLE=0\nif [ ! -f %s ];\n then", quantity_label),
#     sprintf("  call_mf1 $Prog $Frq $Initp %s${M0}a.par $M0 $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
#     "else",
#     sprintf("  echo \"file %s exists\"", quantity_label),
#     "fi",
#     sprintf("M1=`cat %s`", quantity_label),
#     "MLE=`printf \"%.0f\" $M1`",
#     "echo \"The MLE for biomass is $MLE\"",
#     "",
#     sprintf("for Mult in %s; do", paste(Mults, collapse = " ")),
#     sprintf("  call_mf1 $Prog $Frq $Initp %s${Mult}a.par $Mult $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}a.par %s${Mult}b.par $Mult $MLE $Pen2 $Reps2 $Af173 $Af174", quantity_label, quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}b.par %s${Mult}c.par $Mult $MLE $Pen3 $Reps3 $Af173 $Af174", quantity_label, quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}c.par %s${Mult}final.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}final.par %s${Mult}finalx.par $Mult $MLE $Pen3 $Reps5 $Af173 $Af174", quantity_label, quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}finalx.par %s${Mult}finaly.par $Mult $MLE $Pen3 $Reps6 $Af173 $Af174", quantity_label, quantity_label),
#     sprintf("  call_mf1 $Prog $Frq %s${Mult}finaly.par %s${Mult}finalz.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
#     #
#     sprintf("  mv test_plot_output test_plot_output_${Mult}"),
#     sprintf("  Initp=%s${Mult}finalz.par", quantity_label),
#     #sprintf("  Initp=%s${Mult}c.par", quantity_label),
# 
#     "done"
#   )
# 
#   writeLines(bash_script, con = filename)
#   Sys.chmod(filename, mode = "0755")
# 
# }
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# # Generate optimized parallel bash script for MFCL likelihood profile analysis
# 
# generate_proflike_script <- function(
#     Penalties = c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000),
#     Reps = c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500),
#     AgeFlags = c(Af173 = 150, Af174 = 5),
#     Prog = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",
#     Frq = "bet.frq",
#     Initp = "12.par",
#     Mults = c(90, 80, 70, 60, 50),
#     QuantityType = 2,
#     N_JOBS = 2,
#     use_parallel = TRUE,
#     filename = "ProfLike_parallel.sh") {
#   
#   quantity_label <- ifelse(QuantityType == 1, "relative_depletion", "avg_bio")
#   
#   # Same switch generator as original
#   generate_switch <- function(reps, target, weight, quant_type, af173, af174) {
#     sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s 1 346 %s 1 347 %s 1 348 %s 2 173 %s 2 174 %s",
#             reps, quant_type, target, weight, af173, af174)
#   }
#   
#   bash_script <- c(
#     "#!/bin/bash",
#     "",
#     "# ========================================",
#     "# MFCL Likelihood Profile - Parallel Version",
#     "# ========================================",
#     "",
#     "# Define initial parameters",
#     sprintf("Pen1=%d", Penalties["Pen1"]),
#     sprintf("Pen2=%d", Penalties["Pen2"]),
#     sprintf("Pen3=%d", Penalties["Pen3"]),
#     sprintf("Reps1=%d", Reps["Reps1"]),
#     sprintf("Reps2=%d", Reps["Reps2"]),
#     sprintf("Reps3=%d", Reps["Reps3"]),
#     sprintf("Reps4=%d", Reps["Reps4"]),
#     sprintf("Reps5=%d", Reps["Reps5"]),
#     sprintf("Reps6=%d", Reps["Reps6"]),
#     sprintf("Af173=%d", AgeFlags["Af173"]),
#     sprintf("Af174=%d", AgeFlags["Af174"]),
#     sprintf("Prog=%s", Prog),
#     sprintf("Frq=%s", Frq),
#     sprintf("Initp=%s", Initp),
#     sprintf("N_JOBS=%d", N_JOBS),
#     sprintf("QUANTITY_LABEL=%s", quantity_label),
#     "",
#     "# Function matching original call_mf1",
#     "function call_mf1 () {",
#     "  echo \"arg5=$5\"",
#     "  echo \"arg6=$6\"",
#     "  Temp=`bc -l <<< \"$5*$6/100\"`",
#     "  echo \"Temp=$Temp\"",
#     "  Target=`printf \"%.0f\" $Temp`",
#     "  echo \"Target=$Target\"",
#     "  if [ ! -f $4 ]; then",
#     "    echo \"file $4 does not exist\"",
#     sprintf("    $1 $2 $3 $4 \\"),
#     sprintf("    %s", generate_switch("$8", "$Target", "$7", QuantityType, "$9", "${10}")),
#     "  else",
#     "    echo \"file $4 exists already\"",
#     "  fi",
#     "}",
#     "",
#     "# Function to process single scalar",
#     "process_scalar() {",
#     "  local mult=$1",
#     "  local init_par=$2",
#     "  local label=$QUANTITY_LABEL",
#     "  ",
#     "  echo \"========================================\"",
#     "  echo \"Processing scalar: $mult%\"",
#     "  echo \"========================================\"",
#     "  ",
#     "  call_mf1 $Prog $Frq $init_par ${label}${mult}a.par $mult $MLE $Pen1 $Reps1 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}a.par ${label}${mult}b.par $mult $MLE $Pen2 $Reps2 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}b.par ${label}${mult}c.par $mult $MLE $Pen3 $Reps3 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}c.par ${label}${mult}final.par $mult $MLE $Pen3 $Reps4 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}final.par ${label}${mult}finalx.par $mult $MLE $Pen3 $Reps5 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}finalx.par ${label}${mult}finaly.par $mult $MLE $Pen3 $Reps6 $Af173 $Af174",
#     "  call_mf1 $Prog $Frq ${label}${mult}finaly.par ${label}${mult}finalz.par $mult $MLE $Pen3 $Reps4 $Af173 $Af174",
#     "  ",
#     "  mv test_plot_output test_plot_output_${mult} 2>/dev/null || true",
#     "  ",
#     "  echo \"Completed scalar $mult%\"",
#     "}",
#     "",
#     "# Export functions and variables",
#     "export -f call_mf1 process_scalar",
#     "export Prog Frq Pen1 Pen2 Pen3 Reps1 Reps2 Reps3 Reps4 Reps5 Reps6 Af173 Af174 QUANTITY_LABEL",
#     "",
#     "# ========================================",
#     "# Step 1: Get MLE for biomass",
#     "# ========================================",
#     "M0=0",
#     "MLE=0",
#     sprintf("if [ ! -f %s ]; then", quantity_label),
#     sprintf("  call_mf1 $Prog $Frq $Initp %s${M0}a.par $M0 $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
#     "else",
#     sprintf("  echo \"file %s exists\"", quantity_label),
#     "fi",
#     "",
#     sprintf("M1=`cat %s`", quantity_label),
#     "MLE=`printf \"%.0f\" $M1`",
#     "export MLE",
#     "echo \"The MLE for biomass is $MLE\"",
#     "",
#     "# ========================================",
#     "# Step 2: Run likelihood profile",
#     "# ========================================"
#   )
#   
#   if (use_parallel) {
#     bash_script <- c(
#       bash_script,
#       "",
#       "if command -v parallel &> /dev/null; then",
#       "  echo \"Using GNU Parallel for optimization\"",
#       "  echo \"Running with $N_JOBS parallel jobs\"",
#       "  ",
#       sprintf("  parallel -j $N_JOBS --joblog parallel_jobs.log \\"),
#       sprintf("    process_scalar {} $Initp ::: %s", paste(Mults, collapse = " ")),
#       "  ",
#       "  echo \"All scalars completed\"",
#       "  ",
#       "else",
#       "  echo \"GNU parallel not found. Using background jobs...\"",
#       "  ",
#       sprintf("  scalars=(%s)", paste(Mults, collapse = " ")),
#       "  pids=()",
#       "  ",
#       "  for mult in \"${scalars[@]}\"; do",
#       "    process_scalar $mult $Initp > log_${mult}.txt 2>&1 &",
#       "    pids+=($!)",
#       "    echo \"Started scalar $mult% with PID ${pids[-1]}\"",
#       "    ",
#       "    while (( $(jobs -r | wc -l) >= N_JOBS )); do",
#       "      sleep 2",
#       "    done",
#       "  done",
#       "  ",
#       "  echo \"Waiting for all jobs to complete...\"",
#       "  for pid in \"${pids[@]}\"; do",
#       "    wait $pid",
#       "  done",
#       "fi"
#     )
#   } else {
#     bash_script <- c(
#       bash_script,
#       "",
#       "echo \"Running sequentially...\"",
#       sprintf("for Mult in %s; do", paste(Mults, collapse = " ")),
#       sprintf("  call_mf1 $Prog $Frq $Initp %s${Mult}a.par $Mult $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}a.par %s${Mult}b.par $Mult $MLE $Pen2 $Reps2 $Af173 $Af174", quantity_label, quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}b.par %s${Mult}c.par $Mult $MLE $Pen3 $Reps3 $Af173 $Af174", quantity_label, quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}c.par %s${Mult}final.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}final.par %s${Mult}finalx.par $Mult $MLE $Pen3 $Reps5 $Af173 $Af174", quantity_label, quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}finalx.par %s${Mult}finaly.par $Mult $MLE $Pen3 $Reps6 $Af173 $Af174", quantity_label, quantity_label),
#       sprintf("  call_mf1 $Prog $Frq %s${Mult}finaly.par %s${Mult}finalz.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
#       "  mv test_plot_output test_plot_output_${Mult} 2>/dev/null || true",
#       sprintf("  Initp=%s${Mult}finalz.par", quantity_label),
#       "done"
#     )
#   }
#   
#   bash_script <- c(
#     bash_script,
#     "",
#     "echo \"========================================\"",
#     "echo \"Likelihood profile calculation complete\"",
#     "echo \"========================================\""
#   )
#   
#   writeLines(bash_script, con = filename)
#   Sys.chmod(filename, mode = "0755")
#   
#   cat("Generated bash script:", filename, "\n")
#   cat("  Scalars:", paste(Mults, collapse = ", "), "\n")
#   cat("  Parallel:", ifelse(use_parallel, paste("Yes (", N_JOBS, "jobs)"), "No"), "\n")
# }









generate_proflike_script <- function(
    Penalties = c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000),
    Reps = c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500),
    AgeFlags = c(Af173 = 150, Af174 = 5),
    Prog = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",
    Frq = "bet.frq",
    Initp = "12.par",
    Mults = c(90, 80, 70, 60, 50),
    QuantityType = 2,
    N_JOBS = 3,
    use_parallel = TRUE,
    additional_files = c("mfcl.cfg", "bet.age_length", "bet.tag"),  # 추가 파일들
    filename = "ProfLike_parallel.sh") {
  
  quantity_label <- ifelse(QuantityType == 1, "relative_depletion", "avg_bio")
  
  # Same switch generator as original
  generate_switch <- function(reps, target, weight, quant_type, af173, af174) {
    sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s 1 346 %s 1 347 %s 1 348 %s 2 173 %s 2 174 %s",
            reps, quant_type, target, weight, af173, af174)
  }
  
  bash_script <- c(
    "#!/bin/bash",
    "",
    "# ========================================",
    "# MFCL Likelihood Profile - Parallel Version",
    "# ========================================",
    "",
    "# Store absolute paths before changing directories",
    "SCRIPT_DIR=$(pwd)",
    sprintf("PROG_PATH=$(realpath %s)", Prog),
    sprintf("FRQ_PATH=$(realpath %s)", Frq),
    sprintf("INITP_PATH=$(realpath %s)", Initp),
    "",
    "# Store absolute paths for additional required files",
    paste(sprintf("FILE_%d=$(realpath %s 2>/dev/null || echo '')", 
                  seq_along(additional_files), additional_files), collapse = "\n"),
    "",
    "# Define initial parameters",
    sprintf("Pen1=%d", Penalties["Pen1"]),
    sprintf("Pen2=%d", Penalties["Pen2"]),
    sprintf("Pen3=%d", Penalties["Pen3"]),
    sprintf("Reps1=%d", Reps["Reps1"]),
    sprintf("Reps2=%d", Reps["Reps2"]),
    sprintf("Reps3=%d", Reps["Reps3"]),
    sprintf("Reps4=%d", Reps["Reps4"]),
    sprintf("Reps5=%d", Reps["Reps5"]),
    sprintf("Reps6=%d", Reps["Reps6"]),
    sprintf("Af173=%d", AgeFlags["Af173"]),
    sprintf("Af174=%d", AgeFlags["Af174"]),
    sprintf("N_JOBS=%d", N_JOBS),
    sprintf("QUANTITY_LABEL=%s", quantity_label),
    "",
    "# Function matching original call_mf1",
    "function call_mf1 () {",
    '  echo "arg5=$5"',
    '  echo "arg6=$6"',
    '  Temp=`bc -l <<< "$5*$6/100"`',
    '  echo "Temp=$Temp"',
    '  Target=`printf "%.0f" $Temp`',
    '  echo "Target=$Target"',
    "  if [ ! -f $4 ]; then",
    '    echo "file $4 does not exist"',
    sprintf("    $1 $2 $3 $4 \\"),
    sprintf("    %s", generate_switch("$8", "$Target", "$7", QuantityType, "$9", "${10}")),
    "  else",
    '    echo "file $4 exists already"',
    "  fi",
    "}",
    "",
    "# Function to process single scalar",
    "process_scalar() {",
    "  local mult=$1",
    "  local init_par_name=$2",
    "  local label=$QUANTITY_LABEL",
    "  ",
    "  # Create directory for this scalar",
    '  local scalar_dir="${SCRIPT_DIR}/scalar_${mult}"',
    "  mkdir -p $scalar_dir",
    '  echo "========================================"',
    '  echo "Processing scalar: $mult% in directory: $scalar_dir"',
    '  echo "========================================"',
    "  ",
    "  # Copy initial parameter file",
    "  cp $INITP_PATH $scalar_dir/",
    '  local init_par_file=$(basename $init_par_name)',
    "  ",
    "  # Change to scalar directory",
    "  cd $scalar_dir",
    "  ",
    "  # Create symlinks to all required input files",
    '  local frq_name=$(basename $FRQ_PATH)',
    "  [ ! -f $frq_name ] && ln -s $FRQ_PATH $frq_name",
    "  ",
    paste(sprintf(
      "  [ -n \"$FILE_%d\" ] && [ ! -f $(basename \"$FILE_%d\") ] && ln -s \"$FILE_%d\" $(basename \"$FILE_%d\")",
      seq_along(additional_files), seq_along(additional_files), 
      seq_along(additional_files), seq_along(additional_files)
    ), collapse = "\n"),
    "  ",
    "  # Run MFCL sequence",
    "  call_mf1 $PROG_PATH $frq_name $init_par_file ${label}${mult}a.par $mult $MLE $Pen1 $Reps1 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}a.par ${label}${mult}b.par $mult $MLE $Pen2 $Reps2 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}b.par ${label}${mult}c.par $mult $MLE $Pen3 $Reps3 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}c.par ${label}${mult}final.par $mult $MLE $Pen3 $Reps4 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}final.par ${label}${mult}finalx.par $mult $MLE $Pen3 $Reps5 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}finalx.par ${label}${mult}finaly.par $mult $MLE $Pen3 $Reps6 $Af173 $Af174",
    "  call_mf1 $PROG_PATH $frq_name ${label}${mult}finaly.par ${label}${mult}finalz.par $mult $MLE $Pen3 $Reps4 $Af173 $Af174",
    "  ",
    "  # Return to parent directory",
    "  cd $SCRIPT_DIR",
    "  ",
    '  echo "Completed scalar $mult%"',
    "}",
    "",
    "# Export functions and variables",
    "export -f call_mf1 process_scalar",
    sprintf("export PROG_PATH FRQ_PATH INITP_PATH SCRIPT_DIR %s Pen1 Pen2 Pen3 Reps1 Reps2 Reps3 Reps4 Reps5 Reps6 Af173 Af174 QUANTITY_LABEL",
            paste(sprintf("FILE_%d", seq_along(additional_files)), collapse = " ")),
    "",
    "# ========================================",
    "# Step 1: Get MLE for biomass",
    "# ========================================",
    "M0=0",
    "MLE=0",
    sprintf("if [ ! -f %s ]; then", quantity_label),
    sprintf("  call_mf1 $PROG_PATH $FRQ_PATH $INITP_PATH %s${M0}a.par $M0 $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
    "else",
    sprintf('  echo "file %s exists"', quantity_label),
    "fi",
    "",
    sprintf("M1=`cat %s`", quantity_label),
    'MLE=`printf "%.0f" $M1`',
    "export MLE",
    'echo "The MLE for biomass is $MLE"',
    "",
    "# ========================================",
    "# Step 2: Run likelihood profile",
    "# ========================================"
  )
  
  if (use_parallel) {
    bash_script <- c(
      bash_script,
      "",
      "if command -v parallel &> /dev/null; then",
      '  echo "Using GNU Parallel for optimization"',
      '  echo "Running with $N_JOBS parallel jobs"',
      "  ",
      sprintf("  parallel -j $N_JOBS --joblog parallel_jobs.log \\"),
      sprintf("    process_scalar {} $INITP_PATH ::: %s", paste(Mults, collapse = " ")),
      "  ",
      '  echo "All scalars completed"',
      "  ",
      "else",
      '  echo "GNU parallel not found. Using background jobs..."',
      "  ",
      sprintf("  scalars=(%s)", paste(Mults, collapse = " ")),
      "  pids=()",
      "  ",
      '  for mult in "${scalars[@]}"; do',
      "    process_scalar $mult $INITP_PATH > log_${mult}.txt 2>&1 &",
      "    pids+=($!)",
      '    echo "Started scalar $mult% with PID ${pids[-1]}"',
      "    ",
      "    while (( $(jobs -r | wc -l) >= N_JOBS )); do",
      "      sleep 2",
      "    done",
      "  done",
      "  ",
      '  echo "Waiting for all jobs to complete..."',
      '  for pid in "${pids[@]}"; do',
      "    wait $pid",
      "  done",
      "fi"
    )
  } else {
    bash_script <- c(
      bash_script,
      "",
      'echo "Running sequentially..."',
      sprintf("for Mult in %s; do", paste(Mults, collapse = " ")),
      "  process_scalar $Mult $INITP_PATH",
      "done"
    )
  }
  
  bash_script <- c(
    bash_script,
    "",
    'echo "========================================"',
    'echo "Likelihood profile calculation complete"',
    'echo "Results are in scalar_* directories"',
    'echo "========================================"'
  )
  
  writeLines(bash_script, con = filename)
  Sys.chmod(filename, mode = "0755")
  
  cat("Generated bash script:", filename, "\n")
  cat("  Scalars:", paste(Mults, collapse = ", "), "\n")
  cat("  Parallel:", ifelse(use_parallel, paste("Yes (", N_JOBS, "jobs)"), "No"), "\n")
  cat("  Additional files:", paste(additional_files, collapse = ", "), "\n")
  cat("  Output: Each scalar will have its own directory (scalar_<value>)\n")
}
