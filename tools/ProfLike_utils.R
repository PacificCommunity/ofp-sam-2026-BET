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


generate_proflike_script <- function(
    Penalties = c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000),
    Reps = c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500),
    AgeFlags = c(Af173 = 150, Af174 = 5),
    Prog = "../../mfcl/mfclo64",
    Frq = "bet.frq",
    Initp = "11.par",
    Mults = c(90, 80, 70, 60, 50),
    QuantityType = 2,  # 1 for depletion, 2 for average biomass
    filename = "ProfLike.sh") {
  
  quantity_label <- ifelse(QuantityType == 1, "relative_depletion", "avg_bio")
  
  generate_switch <- function(pen, reps, target, weight, af173, af174, quant_type) {
    sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %s 1 346 %s 1 347 %s 1 348 %s 2 173 %s 2 174 %s",
            reps, quant_type, target, pen, af173, af174)
  }
  
  
  bash_script <- c(
    "#!/bin/bash",
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
    sprintf("Prog=%s", Prog),
    sprintf("Frq=%s", Frq),
    sprintf("Initp=%s", Initp),
    "",
    #sprintf("cp $Initp %s.par", quantity_label),
    "",
    "function call_mf1 () {",
    "  echo \"arg5=$5\"",
    "  echo \"arg6=$6\"",
    "  Temp=`bc -l <<< \"$5*$6/100\"`",
    "  echo \"Temp=$Temp\"",
    "  Target=`printf \"%.0f\" $Temp`",
    "  echo \"Target=$Target\"",
    "  if [ ! -f $4 ]; then",
    "    echo \"file $4 does not exist\"",
    sprintf("    $1 $2 $3 $4 \\\n    %s", 
            generate_switch("$7", "$8", "$Target", "$6", "$9", "$10", QuantityType)),
    "  else",
    "    echo \"file $4 exists already\"",
    "  fi",
    "}",
    "",
    sprintf("M0=0\nMLE=0\nif [ ! -f %s ];\n then", quantity_label),
    sprintf("  call_mf1 $Prog $Frq $Initp %s${M0}a.par $M0 $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
    "else",
    sprintf("  echo \"file %s exists\"", quantity_label),
    "fi",
    sprintf("M1=`cat %s`", quantity_label),
    "MLE=`printf \"%.0f\" $M1`",
    "echo \"The MLE for biomass is $MLE\"",
    "",
    sprintf("for Mult in %s; do", paste(Mults, collapse = " ")),
    sprintf("  call_mf1 $Prog $Frq $Initp %s${Mult}a.par $Mult $MLE $Pen1 $Reps1 $Af173 $Af174", quantity_label),
    sprintf("  call_mf1 $Prog $Frq %s${Mult}a.par %s${Mult}b.par $Mult $MLE $Pen2 $Reps2 $Af173 $Af174", quantity_label, quantity_label),
    sprintf("  call_mf1 $Prog $Frq %s${Mult}b.par %s${Mult}c.par $Mult $MLE $Pen3 $Reps3 $Af173 $Af174", quantity_label, quantity_label),
    # sprintf("  call_mf1 $Prog $Frq %s${Mult}c.par %s${Mult}final.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
    # sprintf("  call_mf1 $Prog $Frq %s${Mult}final.par %s${Mult}finalx.par $Mult $MLE $Pen3 $Reps5 $Af173 $Af174", quantity_label, quantity_label),
    # sprintf("  call_mf1 $Prog $Frq %s${Mult}finalx.par %s${Mult}finaly.par $Mult $MLE $Pen3 $Reps6 $Af173 $Af174", quantity_label, quantity_label),
    # sprintf("  call_mf1 $Prog $Frq %s${Mult}finaly.par %s${Mult}finalz.par $Mult $MLE $Pen3 $Reps4 $Af173 $Af174", quantity_label, quantity_label),
    # 
    sprintf("  mv test_plot_output test_plot_output_${Mult}"),
    #sprintf("  Initp=%s${Mult}finalz.par", quantity_label),
    sprintf("  Initp=%s${Mult}c.par", quantity_label),
    
    "done"
  )
  
  writeLines(bash_script, con = filename)
  Sys.chmod(filename, mode = "0755")
  
}

