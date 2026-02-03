# ==============================================================================
# COMPLETE JITTER VALIDATION SYSTEM
# ==============================================================================

library(FLR4MFCL)

# ------------------------------------------------------------------------------
# Helper function
# ------------------------------------------------------------------------------

calc_stats <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NULL)
  
  list(
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    observed_cv = sd(x),
    range = range(x),
    median = median(x),
    q25 = quantile(x, 0.25),
    q75 = quantile(x, 0.75)
  )
}


# ------------------------------------------------------------------------------
# Main validation function
# ------------------------------------------------------------------------------

validate_jitter_complete <- function(par_file, cv = 0.15, n_jitters = 50) {
  
  cat("\n")
  cat("================================================================\n")
  cat("COMPLETE JITTER VALIDATION - ALL PARAMETERS\n")
  cat("================================================================\n\n")
  cat(sprintf("Original par file: %s\n", par_file))
  cat(sprintf("CV: %.4f\n", cv))
  cat(sprintf("Number of jitters: %d\n\n", n_jitters))
  
  # Read original
  cat("Step 1: Loading original parameter file...\n")
  par_orig <- read.MFCLPar(par_file)
  cat("        Done.\n\n")
  
  # Get dimensions
  nFish <- dimensions(par_orig)["fisheries"]
  nAge <- dimensions(par_orig)["agecls"]
  nReg <- dim(region_pars(par_orig))[2]
  
  cat(sprintf("Model dimensions:\n"))
  cat(sprintf("  Fisheries: %d\n", nFish))
  cat(sprintf("  Age classes: %d\n", nAge))
  cat(sprintf("  Regions: %d\n\n", nReg))
  
  # Create jittered parameters
  cat(sprintf("Step 2: Creating %d jittered parameters...\n", n_jitters))
  par_list <- list()
  for (i in 1:n_jitters) {
    par_list[[i]] <- jitter_par(par_orig, cv = cv, seed = i)
    if (i %% 10 == 0) {
      cat(sprintf("        Created %d/%d\n", i, n_jitters))
    }
  }
  cat("        Done.\n\n")
  
  # Analyze ALL parameters
  cat("Step 3: Analyzing ALL parameter changes...\n\n")
  results <- analyze_all_parameters(par_orig, par_list, cv)
  
  # Print summary
  cat("\n")
  print_complete_summary(results, cv)
  
  cat("\n================================================================\n")
  cat("VALIDATION COMPLETE\n")
  cat("================================================================\n\n")
  
  return(results)
}


# ------------------------------------------------------------------------------
# Analyze ALL parameters
# ------------------------------------------------------------------------------

analyze_all_parameters <- function(par_orig, par_list, expected_cv) {
  
  n <- length(par_list)
  nFish <- dimensions(par_orig)["fisheries"]
  nAge <- dimensions(par_orig)["agecls"]
  
  results <- list(
    n_jitters = n,
    expected_cv = expected_cv
  )
  
  eps <- 1e-8
  
  cat("  Checking which parameters are estimated...\n\n")
  
  # ----------------------------------------------------------------------
  # 1. Tag Reporting Rate
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 2, 198)$value == 1) {
    cat("    - Tag reporting rates\n")
    nRRpars <- max(tag_fish_rep_grp(par_orig))
    tag_changes <- matrix(NA, nrow = n, ncol = nRRpars)
    
    for (i in 1:n) {
      par_jit <- par_list[[i]]
      for (grp in 1:nRRpars) {
        grp_idx <- which(tag_fish_rep_grp(par_orig) == grp)
        if (length(grp_idx) > 0) {
          orig_val <- tag_fish_rep_rate(par_orig)[grp_idx[1]]
          jit_val <- tag_fish_rep_rate(par_jit)[grp_idx[1]]
          if (!is.na(orig_val) && orig_val != 0) {
            tag_changes[i, grp] <- (jit_val - orig_val) / orig_val
          }
        }
      }
    }
    results$tag_reporting <- calc_stats(as.vector(tag_changes))
  }
  
  # ----------------------------------------------------------------------
  # 2. Total Population
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 2, 31)$value == 1) {
    cat("    - Total population\n")
    tot_pop_changes <- sapply(par_list, function(p) {
      (tot_pop(p) - tot_pop(par_orig)) / tot_pop(par_orig)
    })
    results$tot_pop <- calc_stats(tot_pop_changes)
  }
  
  # ----------------------------------------------------------------------
  # 3. Recruitment Deviations
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 2, 30)$value == 1) {
    cat("    - Recruitment deviations\n")
    n_rec <- length(rel_rec(par_orig))
    rec_changes <- matrix(NA, nrow = n, ncol = n_rec)
    
    for (i in 1:n) {
      rec_changes[i, ] <- (rel_rec(par_list[[i]]) - rel_rec(par_orig)) / rel_rec(par_orig)
    }
    results$recruitment <- calc_stats(as.vector(rec_changes))
  }
  
  # ----------------------------------------------------------------------
  # 4. Natural Mortality
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 2, 33)$value == 1) {
    cat("    - Natural mortality (scaled)\n")
    M_changes <- sapply(par_list, function(p) {
      (m(p) - m(par_orig)) / m(par_orig)
    })
    results$M <- calc_stats(M_changes)
  }
  
  if (flagval(par_orig, 1, 121)$value == 1) {
    cat("    - Natural mortality (Lorenzen)\n")
    logM_changes <- sapply(par_list, function(p) {
      log_m(p)[1,1,1,1] - log_m(par_orig)[1,1,1,1]
    })
    results$log_M <- calc_stats(logM_changes)
  }
  
  # ----------------------------------------------------------------------
  # 5. Fishery Selectivity - LOG-RATIO BASED (FIXED)
  # ----------------------------------------------------------------------
  uniqueSels <- max(flagval(par_orig, -1:-nFish, 24)$value)
  sel_estimated <- FALSE
  
  for (i in 1:uniqueSels) {
    Selsfish <- which(flagval(par_orig, -1:-nFish, 24)$value == i)
    if (length(Selsfish) > 0 && flagval(par_orig, -Selsfish[1], 48)$value == 1) {
      sel_estimated <- TRUE
      break
    }
  }
  
  if (sel_estimated) {
    cat("    - Fishery selectivity\n")
    sel_changes_list <- list()
    
    for (sel_grp in 1:uniqueSels) {
      Selsfish <- which(flagval(par_orig, -1:-nFish, 24)$value == sel_grp)
      if (length(Selsfish) > 0 && flagval(par_orig, -Selsfish[1], 48)$value == 1) {
        
        for (i in 1:n) {
          orig_sel <- c(aperm(fishery_sel(par_orig)[,,Selsfish[1],], c(4,1,2,3,5,6)))
          jit_sel <- c(aperm(fishery_sel(par_list[[i]])[,,Selsfish[1],], c(4,1,2,3,5,6)))
          
          for (j in seq_along(orig_sel)) {
            # 중요: 실제로 변한 값만 체크
            if (!is.na(orig_sel[j]) && !is.na(jit_sel[j])) {
              
              # 변화가 거의 없으면 skip (고정된 값)
              if (abs(jit_sel[j] - orig_sel[j]) < 1e-10) {
                next
              }
              
              # Protect from exact boundaries
              orig_val <- pmax(pmin(orig_sel[j], 1 - eps), eps)
              jit_val <- pmax(pmin(jit_sel[j], 1 - eps), eps)
              
              # Log-ratio
              log_change <- log(jit_val / orig_val)
              
              # Sanity check: 너무 큰 변화는 제외
              if (abs(log_change) < 5) {  # exp(5) ≈ 150배 변화는 비현실적
                sel_changes_list[[length(sel_changes_list) + 1]] <- log_change
              }
            }
          }
        }
      }
    }
    
    if (length(sel_changes_list) > 0) {
      results$selectivity <- calc_stats(unlist(sel_changes_list))
    }
  }
  # ----------------------------------------------------------------------
  # 6. Average Catchability
  # ----------------------------------------------------------------------
  if (any(flagval(par_orig, -1:-nFish, 1)$value == 1)) {
    cat("    - Average catchability coefficients\n")
    q_changes_list <- list()
    
    for (i in 1:n) {
      for (grp in 1:max(flagval(par_orig, -1:-nFish, 60)$value)) {
        matcher <- flagval(par_orig, -1:-nFish, 60)$value == grp
        if (any(matcher)) {
          orig_q_arr <- av_q_coffs(par_orig)[,,matcher,,,]
          jit_q_arr <- av_q_coffs(par_list[[i]])[,,matcher,,,]
          
          orig_q_vec <- as.vector(orig_q_arr)
          jit_q_vec <- as.vector(jit_q_arr)
          
          for (j in seq_along(orig_q_vec)) {
            if (!is.na(orig_q_vec[j]) && orig_q_vec[j] != 0) {
              change <- (jit_q_vec[j] - orig_q_vec[j]) / orig_q_vec[j]
              q_changes_list[[length(q_changes_list) + 1]] <- change
            }
          }
        }
      }
    }
    
    if (length(q_changes_list) > 0) {
      results$catchability <- calc_stats(unlist(q_changes_list))
    }
  }
  
  # ----------------------------------------------------------------------
  # 7. Diffusion Coefficients
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 2, 68)$value == 1) {
    cat("    - Diffusion coefficients\n")
    diff_changes_list <- list()
    
    for (i in 1:n) {
      diff_orig <- diff_coffs(par_orig)
      diff_jit <- diff_coffs(par_list[[i]])
      
      for (j in seq_along(diff_orig)) {
        if (!is.na(diff_orig[j]) && diff_orig[j] != 0) {
          diff_changes_list[[length(diff_changes_list) + 1]] <- 
            (diff_jit[j] - diff_orig[j]) / diff_orig[j]
        }
      }
    }
    
    if (length(diff_changes_list) > 0) {
      results$diffusion <- calc_stats(unlist(diff_changes_list))
    }
  }
  
  # ----------------------------------------------------------------------
  # 8. Regional Recruitment Distribution - LOG-RATIO BASED
  # ----------------------------------------------------------------------
  flags_subset <- subset(flags(par_orig), flagtype == -100000)
  idx.free <- which(flags_subset$value > 0)
  
  if (length(idx.free) > 0) {
    cat("    - Regional recruitment distribution\n")
    
    reg_changes <- c()
    reg_orig <- region_pars(par_orig)[1, idx.free]
    reg_orig <- pmax(reg_orig, eps)
    
    for (i in 1:n) {
      reg_jit <- region_pars(par_list[[i]])[1, idx.free]
      reg_jit <- pmax(reg_jit, eps)
      
      # Log-ratio for proportions
      log_ratio <- log(reg_jit / reg_orig)
      reg_changes <- c(reg_changes, log_ratio)
    }
    
    results$regional_recruitment <- calc_stats(reg_changes)
  }
  
  # ----------------------------------------------------------------------
  # 9. Growth Parameters
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 1, 12)$value == 1) {
    cat("    - Growth parameter L1\n")
    L1_changes <- sapply(par_list, function(p) {
      (growth(p)[1] - growth(par_orig)[1]) / growth(par_orig)[1]
    })
    results$L1 <- calc_stats(L1_changes)
  }
  
  if (flagval(par_orig, 1, 13)$value == 1) {
    cat("    - Growth parameter L2\n")
    L2_changes <- sapply(par_list, function(p) {
      (growth(p)[2] - growth(par_orig)[2]) / growth(par_orig)[2]
    })
    results$L2 <- calc_stats(L2_changes)
  }
  
  if (flagval(par_orig, 1, 14)$value == 1) {
    cat("    - Growth parameter k\n")
    k_changes <- sapply(par_list, function(p) {
      (growth(p)[3] - growth(par_orig)[3]) / growth(par_orig)[3]
    })
    results$k <- calc_stats(k_changes)
  }
  
  # ----------------------------------------------------------------------
  # 10. Growth Variance Parameters
  # ----------------------------------------------------------------------
  if (flagval(par_orig, 1, 15)$value == 1) {
    cat("    - Growth variance parameter 1\n")
    gvar1_changes <- sapply(par_list, function(p) {
      (growth_var_pars(p)[1] - growth_var_pars(par_orig)[1]) / growth_var_pars(par_orig)[1]
    })
    results$growth_var_1 <- calc_stats(gvar1_changes)
  }
  
  if (flagval(par_orig, 1, 16)$value == 1) {
    cat("    - Growth variance parameter 2\n")
    gvar2_changes <- sapply(par_list, function(p) {
      (growth_var_pars(p)[2] - growth_var_pars(par_orig)[2]) / growth_var_pars(par_orig)[2]
    })
    results$growth_var_2 <- calc_stats(gvar2_changes)
  }
  
  cat("\n  Done.\n")
  
  return(results)
}


# ------------------------------------------------------------------------------
# Print summary
# ------------------------------------------------------------------------------

print_complete_summary <- function(results, expected_cv) {
  
  cat("================================================================\n")
  cat("COMPLETE VALIDATION SUMMARY\n")
  cat("================================================================\n\n")
  cat(sprintf("Number of jitters: %d\n", results$n_jitters))
  cat(sprintf("Expected CV: %.4f\n\n", expected_cv))
  cat(sprintf("Acceptable range: [%.4f, %.4f]\n\n", 
              expected_cv * 0.7, expected_cv * 1.3))
  
  cat("----------------------------------------------------------------\n\n")
  
  # Print function
  print_param <- function(name, stats) {
    if (is.null(stats)) return()
    
    cat(sprintf("%s:\n", name))
    cat(sprintf("  n = %d\n", stats$n))
    cat(sprintf("  Mean: %.6f\n", stats$mean))
    cat(sprintf("  SD: %.6f\n", stats$sd))
    cat(sprintf("  Observed CV: %.4f\n", stats$observed_cv))
    cat(sprintf("  Range: [%.6f, %.6f]\n", stats$range[1], stats$range[2]))
    
    lower <- expected_cv * 0.7
    upper <- expected_cv * 1.3
    pass <- (stats$observed_cv >= lower & stats$observed_cv <= upper)
    cat(sprintf("  Expected CV range: [%.4f, %.4f]\n", lower, upper))
    cat(sprintf("  Status: %s\n\n", ifelse(pass, "✓ PASS", "✗ FAIL")))
  }
  
  # Print all
  print_param("Tag Reporting Rate", results$tag_reporting)
  print_param("Total Population", results$tot_pop)
  print_param("Recruitment Deviations", results$recruitment)
  print_param("Natural Mortality (M)", results$M)
  print_param("Fishery Selectivity", results$selectivity)
  print_param("Average Catchability", results$catchability)
  print_param("Diffusion Coefficients", results$diffusion)
  print_param("Regional Recruitment Distribution", results$regional_recruitment)
  print_param("Growth L1", results$L1)
  print_param("Growth L2", results$L2)
  print_param("Growth k", results$k)
  print_param("Growth Variance 1", results$growth_var_1)
  print_param("Growth Variance 2", results$growth_var_2)
  
  cat("================================================================\n")
  
  # Overall
  all_cvs <- c()
  param_names <- c()
  
  for (param in c("tag_reporting", "tot_pop", "recruitment", "M", "selectivity",
                  "catchability", "diffusion", "regional_recruitment",
                  "L1", "L2", "k", "growth_var_1", "growth_var_2")) {
    if (!is.null(results[[param]])) {
      all_cvs <- c(all_cvs, results[[param]]$observed_cv)
      param_names <- c(param_names, param)
    }
  }
  
  if (length(all_cvs) > 0) {
    cat("\nOVERALL ASSESSMENT:\n")
    cat(sprintf("  Mean observed CV: %.4f\n", mean(all_cvs)))
    cat(sprintf("  Expected CV: %.4f\n", expected_cv))
    
    lower <- expected_cv * 0.7
    upper <- expected_cv * 1.3
    n_pass <- sum(all_cvs >= lower & all_cvs <= upper)
    n_total <- length(all_cvs)
    
    cat(sprintf("  Parameters passing: %d / %d\n", n_pass, n_total))
    
    if (n_pass == n_total) {
      cat("  Status: ✓ ALL PASS\n")
    } else {
      cat(sprintf("  Status: ✗ %d FAILED\n", n_total - n_pass))
      cat("  Failed parameters:\n")
      for (i in which(all_cvs < lower | all_cvs > upper)) {
        cat(sprintf("    - %s (CV=%.4f)\n", param_names[i], all_cvs[i]))
      }
    }
  }
  
  cat("================================================================\n")
}



# 완전한 validation 실행
results <- validate_jitter_complete("11.par", cv = 0.001, n_jitters = 200)

# 결과 저장
saveRDS(results, "jitter_validation_results.rds")

# 특정 파라미터 확인
results$tag_reporting           # Tag reporting rates
results$regional_recruitment    # Regional recruitment
results$recruitment             # Recruitment deviations
results$catchability           # Catchability
results$diffusion              # Movement parameters

# CV 체크
results$recruitment$observed_cv      # Should be ≈ 0.15
results$regional_recruitment$observed_cv  # Should be ≈ 0.15
