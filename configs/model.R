models <- list(
  
  "mixP1" = list(
    mfcl_args = paste("bet.frq 11.par 12.par",
                          "-switch 2",
                          "1 1 10000",
                          "-9999 1 1",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "1 1 1 1 1 1",
    scalers = paste0((seq(110, 80, by=-10)), collapse = " ")
  ),
  
  "base" = list(
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1",
                          "1 1 1", 
                       #   "-9999 1 2",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "1 1 1 1 1 1",
    scalers = paste0((seq(110, 80, by=-10)), collapse = " ")
  ))

  # "mixP3" = list(
  #   mfcl_commands = paste("bet.frq 11.par 12.par",
  #                         "-switch 2",
  #                         "1 1 10000", 
  #                         "-9999 1 3",
  #                         sep = " "),
  #   program_path = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
  #   base_dir = "mfcl/inputs/2026"                   # Model-specific dir
  # ))













