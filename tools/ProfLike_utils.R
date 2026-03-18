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
    Penalties = c(Pen1 = 5e4, Pen2 = 5e5, Pen3 = 5e6),
    Reps = c(Reps1 = 5, Reps2 = 10, Reps3 = 15, Reps4 = 200, Reps5 = 50, Reps6 = 200),
    AgeFlags = c(Af172 = 0, Af173 = 0, Af174 = 0),
    Prog = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",
    Frq = "bet.frq",
    Initp = "12.par",
    Mults = c(90, 80, 70, 60, 50),
    QuantityType = 2,
    UseQuantityPenalty = TRUE,
    FixedMLE = NA_real_,
    ExtraSwitch = "",
    IndepvarReps = NULL,
    IndepvarLockRds = "",
    IndepvarFile = "",
    LockScript = "",
    DistanceBreaks = c(mid = 20, far = 35),
    PenaltyScales = c(near = 1, mid = 2, far = 4),
    RepsScales = c(near = 1, mid = 1.25, far = 1.5),
    ExtraFarRefine = TRUE,
    filename = "ProfLike.sh") {
  
  quantity_label <- ifelse(QuantityType == 1, "relative_depletion", "avg_bio")
  indepvar_reps_val <- if (!is.null(IndepvarReps) && is.finite(as.integer(IndepvarReps)) && as.integer(IndepvarReps) >= 1L) {
    as.integer(IndepvarReps)
  } else {
    as.integer(Reps["Reps4"])
  }
  
  # Same switch generator as original
  generate_switch <- function(reps, target, weight, quant_type, af172, af173, af174, use_quantity_penalty = TRUE) {
    if (!isTRUE(use_quantity_penalty)) {
      return(sprintf("-switch 5 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s", reps))
    }
    sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s 1 346 %s 1 347 %s 1 348 %s 2 172 %s 2 173 %s 2 174 %s",
            reps, quant_type, target, weight, af172, af173, af174)
  }
  
  bash_script <- c(
    "#!/bin/bash",
    "",
    "# ========================================",
    "# MFCL Likelihood Profile",
    "# ========================================",
    "",
    "# Store absolute paths",
    "SCRIPT_DIR=$(pwd)",
    sprintf("PROG_PATH=$(realpath %s)", Prog),
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
    sprintf("Af172=%d", AgeFlags["Af172"]),
    sprintf("Af173=%d", AgeFlags["Af173"]),
    sprintf("Af174=%d", AgeFlags["Af174"]),
    sprintf("Frq=%s", Frq),
    sprintf("Initp=%s", Initp),
    sprintf("QUANTITY_LABEL=%s", quantity_label),
    sprintf("MID_BREAK=%d", as.integer(DistanceBreaks["mid"])),
    sprintf("FAR_BREAK=%d", as.integer(DistanceBreaks["far"])),
    sprintf("PEN_SCALE_NEAR=%.6f", as.numeric(PenaltyScales["near"])),
    sprintf("PEN_SCALE_MID=%.6f", as.numeric(PenaltyScales["mid"])),
    sprintf("PEN_SCALE_FAR=%.6f", as.numeric(PenaltyScales["far"])),
    sprintf("REPS_SCALE_NEAR=%.6f", as.numeric(RepsScales["near"])),
    sprintf("REPS_SCALE_MID=%.6f", as.numeric(RepsScales["mid"])),
    sprintf("REPS_SCALE_FAR=%.6f", as.numeric(RepsScales["far"])),
    sprintf("EXTRA_FAR_REFINE=%s", ifelse(isTRUE(ExtraFarRefine), "1", "0")),
    sprintf("MLE_FIXED=%s", ifelse(is.finite(FixedMLE), sprintf("%.0f", as.numeric(FixedMLE)), "")),
    sprintf("USE_QUANTITY_PENALTY=%s", ifelse(isTRUE(UseQuantityPenalty), "1", "0")),
    sprintf("INDEPVAR_REPS=%d", indepvar_reps_val),
    sprintf("EXTRA_SWITCH=%s", shQuote(trimws(ExtraSwitch), type = "sh")),
    sprintf("INDEPVAR_LOCK_RDS=%s", IndepvarLockRds),
    sprintf("INDEPVAR_FILE=%s", IndepvarFile),
    sprintf("LOCK_SCRIPT=%s", LockScript),
    "",
    "# Function matching original call_mf1",
    "function call_mf1 () {",
    '  echo "arg5=$5"',
    '  echo "arg6=$6"',
    '  Temp=`bc -l <<< "$5*$6/100"`',
    '  echo "Temp=$Temp"',
    '  Target=`printf "%.0f" $Temp`',
    '  echo "Target=$Target"',
    sprintf("  BASE_SWITCH=\"%s\"", generate_switch("$8", "$Target", "$7", QuantityType, "$9", "${10}", "${11}", UseQuantityPenalty)),
    "  FINAL_SWITCH=\"$BASE_SWITCH\"",
    "  EXTRA_SWITCH_TRIM=\"$(echo \"$EXTRA_SWITCH\" | sed 's/^ *//;s/ *$//')\"",
    "  if [ -n \"$EXTRA_SWITCH_TRIM\" ]; then",
    "    EXTRA_N=$(awk -v s=\"$EXTRA_SWITCH_TRIM\" 'BEGIN{n=split(s,a,/ +/); if(n%3!=0){print -1}else{print n/3}}')",
    "    if [ \"$EXTRA_N\" -lt 0 ]; then",
    "      echo \"Invalid prof_extra_switch (must be triplets: type flag value): $EXTRA_SWITCH_TRIM\"",
    "      return 1",
    "    fi",
    "    BASE_N=$(echo \"$BASE_SWITCH\" | awk '{print $2}')",
    "    BASE_TAIL=$(echo \"$BASE_SWITCH\" | cut -d' ' -f3-)",
    "    FINAL_N=$((BASE_N + EXTRA_N))",
    "    FINAL_SWITCH=\"-switch $FINAL_N $BASE_TAIL $EXTRA_SWITCH_TRIM\"",
    "  fi",
    "  if [ ! -f $4 ]; then",
    '    echo "file $4 does not exist"',
    sprintf("    $1 $2 $3 $4 \\"),
    "    $FINAL_SWITCH",
    "  else",
    '    echo "file $4 exists already"',
    "  fi",
    "}",
    "",
    "refresh_quantity_from_final() {",
    "  local in_par=$1",
    "  local out_par=$2",
    "  echo \"Refreshing quantity from $in_par -> $out_par (no penalty)\"",
    "  rm -f $QUANTITY_LABEL",
    sprintf("  BASE_SWITCH=\"%s\"", generate_switch("1", "0", "0", QuantityType, "$Af172", "$Af173", "$Af174", UseQuantityPenalty)),
    "  FINAL_SWITCH=\"$BASE_SWITCH\"",
    "  EXTRA_SWITCH_TRIM=\"$(echo \"$EXTRA_SWITCH\" | sed 's/^ *//;s/ *$//')\"",
    "  if [ -n \"$EXTRA_SWITCH_TRIM\" ]; then",
    "    EXTRA_N=$(awk -v s=\"$EXTRA_SWITCH_TRIM\" 'BEGIN{n=split(s,a,/ +/); if(n%3!=0){print -1}else{print n/3}}')",
    "    if [ \"$EXTRA_N\" -lt 0 ]; then",
    "      echo \"Invalid prof_extra_switch (must be triplets: type flag value): $EXTRA_SWITCH_TRIM\"",
    "      return 1",
    "    fi",
    "    BASE_N=$(echo \"$BASE_SWITCH\" | awk '{print $2}')",
    "    BASE_TAIL=$(echo \"$BASE_SWITCH\" | cut -d' ' -f3-)",
    "    FINAL_N=$((BASE_N + EXTRA_N))",
    "    FINAL_SWITCH=\"-switch $FINAL_N $BASE_TAIL $EXTRA_SWITCH_TRIM\"",
    "  fi",
    "  $PROG_PATH $Frq $in_par $out_par \\",
    "    $FINAL_SWITCH",
    "}",
    "",
    "lock_indepvar_par() {",
    "  local par_file=$1",
    "  if [ -z \"$INDEPVAR_LOCK_RDS\" ] || [ -z \"$LOCK_SCRIPT\" ]; then",
    "    return 0",
    "  fi",
    "  if [ ! -f \"$INDEPVAR_LOCK_RDS\" ] || [ ! -f \"$LOCK_SCRIPT\" ]; then",
    "    echo \"indepvar lock config/script missing; skip lock\"",
    "    return 0",
    "  fi",
    "  if [ ! -f \"$par_file\" ]; then",
    "    echo \"par file not found for lock: $par_file\"",
    "    return 1",
    "  fi",
    "  Rscript \"$LOCK_SCRIPT\" --par \"$par_file\" --lock \"$INDEPVAR_LOCK_RDS\" --indepvar \"$INDEPVAR_FILE\"",
    "}",
    "",
    "# Distance-based tuning with linear interpolation between near/mid/far",
    "interpolate_scale() {",
    "  local dist=$1",
    "  local break1=$2",
    "  local break2=$3",
    "  local scale_near=$4",
    "  local scale_mid=$5",
    "  local scale_far=$6",
    "  ",
    "  awk -v d=\"$dist\" -v b1=\"$break1\" -v b2=\"$break2\" -v s0=\"$scale_near\" -v s1=\"$scale_mid\" -v s2=\"$scale_far\" '",
    "    BEGIN {",
    "      if (d <= 0) { print s0; exit }",
    "      if (d <= b1) {",
    "        if (b1 <= 0) { print s1; exit }",
    "        print s0 + (s1 - s0) * (d / b1);",
    "        exit",
    "      }",
    "      if (d <= b2) {",
    "        if (b2 <= b1) { print s2; exit }",
    "        print s1 + (s2 - s1) * ((d - b1) / (b2 - b1));",
    "        exit",
    "      }",
    "      print s2",
    "    }'",
    "}",
    "",
    "scale_int() {",
    "  local base=$1",
    "  local scale=$2",
    "  awk -v b=\"$base\" -v s=\"$scale\" 'BEGIN{v=b*s; printf \"%d\", (v < 1 ? 1 : int(v + 0.5))}'",
    "}",
    "",
    "configure_schedule() {",
    "  local mult=$1",
    "  local dist=$(( 100 - mult ))",
    "  if (( dist < 0 )); then dist=$(( -dist )); fi",
    "  ",
    "  local pen_scale=$(interpolate_scale \"$dist\" \"$MID_BREAK\" \"$FAR_BREAK\" \"$PEN_SCALE_NEAR\" \"$PEN_SCALE_MID\" \"$PEN_SCALE_FAR\")",
    "  local reps_scale=$(interpolate_scale \"$dist\" \"$MID_BREAK\" \"$FAR_BREAK\" \"$REPS_SCALE_NEAR\" \"$REPS_SCALE_MID\" \"$REPS_SCALE_FAR\")",
    "  local extra_refine=0",
    "  ",
    "  if (( dist >= FAR_BREAK )); then",
    "    extra_refine=$EXTRA_FAR_REFINE",
    "  fi",
    "  ",
    "  CUR_PEN1=$(scale_int \"$Pen1\" \"$pen_scale\")",
    "  CUR_PEN2=$(scale_int \"$Pen2\" \"$pen_scale\")",
    "  CUR_PEN3=$(scale_int \"$Pen3\" \"$pen_scale\")",
    "  CUR_PEN4=$(scale_int \"$Pen3\" \"$(awk -v s=\"$pen_scale\" 'BEGIN{print s*2}')\")",
    "  ",
    "  CUR_REPS1=$(scale_int \"$Reps1\" \"$reps_scale\")",
    "  CUR_REPS2=$(scale_int \"$Reps2\" \"$reps_scale\")",
    "  CUR_REPS3=$(scale_int \"$Reps3\" \"$reps_scale\")",
    "  CUR_REPS4=$(scale_int \"$Reps4\" \"$reps_scale\")",
    "  CUR_REPS5=$(scale_int \"$Reps5\" \"$reps_scale\")",
    "  CUR_REPS6=$(scale_int \"$Reps6\" \"$reps_scale\")",
    "  CUR_EXTRA_REFINE=$extra_refine",
    "  ",
    "  echo \"mult=$mult dist=$dist pen_scale=$pen_scale reps_scale=$reps_scale extra_refine=$extra_refine\"",
    "}",
    "",
    "# ========================================",
    "# Run likelihood profile",
    "# ========================================",
    "",
    'echo "Running in current directory: $SCRIPT_DIR"',
    "",
    "# Resolve MLE (prefer fixed reference MLE from runner; fallback to Initp-derived MLE)",
    "if [ \"$USE_QUANTITY_PENALTY\" != \"1\" ]; then",
    "  MLE=0",
    "  echo \"Quantity penalty OFF: using MLE=0 placeholder\"",
    "elif [ -n \"$MLE_FIXED\" ]; then",
    "  MLE=$MLE_FIXED",
    "  echo \"Using fixed reference MLE: $MLE\"",
    "else",
    "  rm -f $QUANTITY_LABEL",
    "  M0=0",
    "  MLE=0",
    "  if [ ! -f $QUANTITY_LABEL ]; then",
    "    call_mf1 $PROG_PATH $Frq $Initp ${QUANTITY_LABEL}${M0}a.par $M0 $MLE $Pen1 $Reps1 $Af172 $Af173 $Af174",
    "  else",
    "    echo \"file $QUANTITY_LABEL exists\"",
    "  fi",
    "  M1=$(cat $QUANTITY_LABEL)",
    "  MLE=$(printf \"%.0f\" $M1)",
    "  echo \"Derived MLE from Initp: $MLE\"",
    "fi",
    "",
    "lock_indepvar_par \"$Initp\"",
    "",
    sprintf("for Mult in %s; do", paste(Mults, collapse = " ")),
    "  if [ \"$USE_QUANTITY_PENALTY\" = \"1\" ]; then",
    "    # Standard biomass-penalty profile: escalating penalty/reps ramp",
    "    configure_schedule $Mult",
    "    call_mf1 $PROG_PATH $Frq $Initp ${QUANTITY_LABEL}${Mult}a.par $Mult $MLE $CUR_PEN1 $CUR_REPS1 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}a.par",
    "    call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}a.par ${QUANTITY_LABEL}${Mult}b.par $Mult $MLE $CUR_PEN2 $CUR_REPS2 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}b.par",
    "    call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}b.par ${QUANTITY_LABEL}${Mult}c.par $Mult $MLE $CUR_PEN3 $CUR_REPS3 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}c.par",
    "    call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}c.par ${QUANTITY_LABEL}${Mult}final.par $Mult $MLE $CUR_PEN3 $CUR_REPS4 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}final.par",
    "    call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}final.par ${QUANTITY_LABEL}${Mult}finalx.par $Mult $MLE $CUR_PEN3 $CUR_REPS5 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}finalx.par",
    "    call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}finalx.par ${QUANTITY_LABEL}${Mult}finaly.par $Mult $MLE $CUR_PEN3 $CUR_REPS6 $Af172 $Af173 $Af174",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}finaly.par",
    "    if (( CUR_EXTRA_REFINE == 1 )); then",
    "      call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}finaly.par ${QUANTITY_LABEL}${Mult}finalzz.par $Mult $MLE $CUR_PEN4 $CUR_REPS4 $Af172 $Af173 $Af174",
    "      lock_indepvar_par ${QUANTITY_LABEL}${Mult}finalzz.par",
    "      call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}finalzz.par ${QUANTITY_LABEL}${Mult}finalz.par $Mult $MLE $CUR_PEN4 $CUR_REPS6 $Af172 $Af173 $Af174",
    "    else",
    "      call_mf1 $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}finaly.par ${QUANTITY_LABEL}${Mult}finalz.par $Mult $MLE $CUR_PEN3 $CUR_REPS4 $Af172 $Af173 $Af174",
    "    fi",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}finalz.par",
    "    refresh_quantity_from_final ${QUANTITY_LABEL}${Mult}finalz.par ${QUANTITY_LABEL}${Mult}finalmle.par",
    "    echo \"Completed quantity refresh for scalar $Mult: ${QUANTITY_LABEL}${Mult}finalmle.par\"",
    "  else",
    "    # Indepvar fixed-parameter profile: parameter already fixed in Initp,",
    "    # so no penalty ramp needed -- just optimize free parameters twice.",
    "    # Chain warm-starting (Initp = prev scalar's final par) is handled by",
    "    # run_prof_chain.R passing init_from_scalar, so sequential par flow is",
    "    # preserved across the profile grid.",
    sprintf("    BASE_SWITCH_INDEP=\"%s\"", generate_switch("$INDEPVAR_REPS", "0", "0", QuantityType, "$Af172", "$Af173", "$Af174", FALSE)),
    "    FINAL_SWITCH_INDEP=\"$BASE_SWITCH_INDEP\"",
    "    EXTRA_SWITCH_TRIM=\"$(echo \"$EXTRA_SWITCH\" | sed 's/^ *//;s/ *$//')\"",
    "    if [ -n \"$EXTRA_SWITCH_TRIM\" ]; then",
    "      EXTRA_N=$(awk -v s=\"$EXTRA_SWITCH_TRIM\" 'BEGIN{n=split(s,a,/ +/); if(n%3!=0){print -1}else{print n/3}}')",
    "      BASE_N=$(echo \"$BASE_SWITCH_INDEP\" | awk '{print $2}')",
    "      BASE_TAIL=$(echo \"$BASE_SWITCH_INDEP\" | cut -d' ' -f3-)",
    "      FINAL_N=$((BASE_N + EXTRA_N))",
    "      FINAL_SWITCH_INDEP=\"-switch $FINAL_N $BASE_TAIL $EXTRA_SWITCH_TRIM\"",
    "    fi",
    "    if [ ! -f ${QUANTITY_LABEL}${Mult}a.par ]; then",
    "      $PROG_PATH $Frq $Initp ${QUANTITY_LABEL}${Mult}a.par $FINAL_SWITCH_INDEP",
    "    fi",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}a.par",
    "    if [ ! -f ${QUANTITY_LABEL}${Mult}finalz.par ]; then",
    "      $PROG_PATH $Frq ${QUANTITY_LABEL}${Mult}a.par ${QUANTITY_LABEL}${Mult}finalz.par $FINAL_SWITCH_INDEP",
    "    fi",
    "    lock_indepvar_par ${QUANTITY_LABEL}${Mult}finalz.par",
    "  fi",
    "  echo \"Completed final par for scalar $Mult: ${QUANTITY_LABEL}${Mult}finalz.par\"",
    "done",
    "",
    'echo "========================================"',
    'echo "Likelihood profile calculation complete"',
    'echo "========================================"'
  )
  
  writeLines(bash_script, con = filename)
  Sys.chmod(filename, mode = "0755")
  
  cat("Generated bash script:", filename, "\n")
  cat("  Scalars:", paste(Mults, collapse = ", "), "\n")
  cat("  Distance breaks:", paste(names(DistanceBreaks), DistanceBreaks, collapse = ", "), "\n")
  cat("  Penalty scales:", paste(names(PenaltyScales), PenaltyScales, collapse = ", "), "\n")
  cat("  Reps scales:", paste(names(RepsScales), RepsScales, collapse = ", "), "\n")
  cat("  Age flags:", paste(names(AgeFlags), AgeFlags, collapse = ", "), "\n")
}
