#' ==============================================================================
#' MFCL Parameter Jittering Function (FINAL - CLEAN & GENERIC)
#' ==============================================================================
#' 
#' @param par MFCLPar object
#' @param cv Coefficient of variation (0.01-0.25 recommended)
#' @param seed Random seed for reproducibility
#' 
#' @return Jittered MFCLPar object
#' 
#' @export
#' ==============================================================================

setGeneric("jitter_par", function(par, cv, seed) {
  standardGeneric("jitter_par")
})

setMethod("jitter_par", signature(par = "MFCLPar", cv = "numeric", seed = "numeric"),
          function(par, cv, seed) {
            
            set.seed(seed)
            
            nFish    <- dimensions(par)["fisheries"]
            nAge     <- dimensions(par)["agecls"]
            nSeasons <- dimensions(par)["seasons"]
            nYears   <- dimensions(par)["years"]
            nAgeYr   <- nAge / nSeasons
            nReg     <- dim(region_pars(par))[2]
            
            
            if (cv < 1e-6) {
              message(sprintf("CV = %.2e: Too small, returning unchanged", cv))
              return(par)
            }
            
            
            # ==================================================================
            # STANDARD MULTIPLICATIVE JITTER: value * exp(rnorm(0, cv))
            # ==================================================================
            
            # Tag Reporting Rate
            if (flagval(par, 2, 198)$value == 1) {
              nRRpars <- max(tag_fish_rep_grp(par))
              maxRR   <- flagval(par, 1, 33)$value / 100
              
              if (length(which(tag_fish_rep_flags(par) == 0)) > 0) {
                orig.rep.rate <- tag_fish_rep_rate(par)
                idx.fixed     <- which(tag_fish_rep_flags(par) == 0)
                
                for (i in 1:nRRpars) {
                  grp_idx <- which(tag_fish_rep_grp(par) == i)
                  grp_free <- setdiff(grp_idx, idx.fixed)
                  
                  if (length(grp_free) > 0) {
                    current_val <- orig.rep.rate[grp_free[1]]
                    new_val <- current_val * exp(rnorm(1, 0, cv))
                    new_val <- max(0, min(maxRR, new_val))
                    tag_fish_rep_rate(par)[grp_idx] <- new_val
                  }
                }
                tag_fish_rep_rate(par)[idx.fixed] <- orig.rep.rate[idx.fixed]
              } else {
                for (i in 1:nRRpars) {
                  grp_idx <- which(tag_fish_rep_grp(par) == i)
                  current_val <- tag_fish_rep_rate(par)[grp_idx[1]]
                  new_val <- current_val * exp(rnorm(1, 0, cv))
                  new_val <- max(0, min(maxRR, new_val))
                  tag_fish_rep_rate(par)[grp_idx] <- new_val
                }
              }
            }
            
            # Total Population
            if (flagval(par, 2, 31)$value == 1) {
              tot_pop(par) <- tot_pop(par) * exp(rnorm(1, 0, cv))
            }
            
            # Recruitment Deviations
            if (flagval(par, 2, 30)$value == 1) {
              rel_rec(par) <- rel_rec(par) * exp(rnorm(length(rel_rec(par)), 0, cv))
            }
            
            
            # ==================================================================
            # Fishery Selectivity - LOGIT TRANSFORMATION (FIXED)
            # ==================================================================
            
            uniqueSels <- max(flagval(par, -1:-nFish, 24)$value)
            for (i in 1:uniqueSels) {
              Selsfish <- which(flagval(par, -1:-nFish, 24)$value == i)
              if (flagval(par, -Selsfish[1], 48)$value == 1) {
                CurrentSel <- c(aperm(fishery_sel(par)[, , Selsfish[1], ], c(4,1,2,3,5,6)))
                
                # Protect from exact 0/1
                eps <- 1e-8
                CurrentSel <- pmax(pmin(CurrentSel, 1 - eps), eps)
                
                # Logit transformation
                logit_sel <- log(CurrentSel / (1 - CurrentSel))
                
                # Jitter in logit space
                # 핵심: logit scale의 변화를 크게 설정해야 original scale에서 cv 수준의 변화가 됨
                # 경험적으로: sigma ≈ cv * 2.0 정도가 필요
                sigma_logit <- cv * 2.0
                logit_sel_jit <- logit_sel + rnorm(nAge, 0, sigma_logit)
                
                # Back-transform
                NewSel <- 1 / (1 + exp(-logit_sel_jit))
                
                # Final bounds
                NewSel <- pmax(0.001, pmin(0.999, NewSel))
                
                fishery_sel(par)[, , Selsfish] <-
                  aperm(array(NewSel, c(nSeasons, nAgeYr, 1, length(Selsfish), 1, 1)),
                        c(2, 3, 4, 1, 5, 6))
              }
            }
            
            # ==================================================================
            # STANDARD MULTIPLICATIVE (continued)
            # ==================================================================
            
            # Natural Mortality
            if (flagval(par, 2, 33)$value == 1) {
              m(par) <- m(par) * exp(rnorm(1, 0, cv))
            }
            if (flagval(par, 1, 121)$value == 1) {
              log_m(par)[1, 1, 1, 1] <- log_m(par)[1, 1, 1, 1] + rnorm(1, 0, cv)
            }
            
            # Average Catchability
            if (any(flagval(par, -1:-nFish, 1)$value == 1)) {
              for (i in 1:max(flagval(par, -1:-nFish, 60)$value)) {
                matcher <- flagval(par, -1:-nFish, 60)$value == i
                av_q_coffs(par)[, , matcher, , , ] <- av_q_coffs(par)[, , matcher, , , ] *
                  exp(rnorm(1, 0, cv))
              }
            }
            
            # Diffusion Coefficients
            if (flagval(par, 2, 68)$value == 1) {
              diff_coffs(par) <- diff_coffs(par) * exp(rnorm(length(diff_coffs(par)), 0, cv))
              diff_coffs(par)[diff_coffs(par) <= 0] <- 1e-16
              diff_coffs(par)[diff_coffs(par) >= 3] <- 2.9999999
            }
            
            # Cross-diffusion Coefficients
            if (flagval(par, 2, 184)$value == 1) {
              xdiff_coffs(par) <- xdiff_coffs(par) *
                exp(rnorm(length(xdiff_coffs(par)), 0, cv))
            }
            
            
            # ==================================================================
            # Regional Recruitment Distribution - GENERIC MULTIPLICATIVE + RENORM
            # ==================================================================
            
            if (sum(subset(flags(par), flagtype == -100000)$value > 0) > 0) {
              
              idx.free <- which(subset(flags(par), flagtype == -100000)$value == 1)
              
              if (length(idx.free) > 0) {
                must.sum <- 1 - sum(region_pars(par)[1, -idx.free])
                current_props <- region_pars(par)[1, idx.free]
                
                # Numerical protection
                eps <- 1e-12
                current_props <- pmax(current_props, eps)
                
                # Log-normal jitter with calibrated k_factor
                k_factor <- 0.62
                sigma <- k_factor * cv
                
                log_jitter <- rnorm(length(idx.free), mean = 0, sd = sigma)
                jittered_raw <- current_props * exp(log_jitter)
                
                # Renormalize to must.sum
                jittered_raw <- pmax(jittered_raw, eps)
                new_props <- jittered_raw / sum(jittered_raw) * must.sum
                
                # Numerical stability
                new_props <- pmax(new_props, eps)
                new_props <- pmin(new_props, must.sum - eps * (length(idx.free) - 1))
                
                region_pars(par)[1, idx.free] <- new_props
              }
            }
            
            
            # ==================================================================
            # REMAINING PARAMETERS - STANDARD MULTIPLICATIVE
            # ==================================================================
            
            # Extra Fishery Parameters
            for (i in 1:nFish) {
              if (flagval(par, -i, 27)$value == 1) {
                fish_params(par)[1:2, i] <- fish_params(par)[1:2, i] * exp(rnorm(2, 0, cv))
              }
            }
            
            if (any(flagval(par, -1:-nFish, 43)$value == 1)) {
              nVars <- max(flagval(par, -1:-nFish, 44)$value)
              for (i in 1:nVars) {
                matcher <- flagval(par, -1:-nFish, 44)$value == i
                if (all(flagval(par, -1:-nFish, 43)$value[matcher] == 1)) {
                  fish_params(par)[4, matcher] <- fish_params(par)[4, matcher] *
                    exp(rnorm(1, 0, cv))
                }
              }
            }
            
            # Growth Deviations
            if (flagval(par, 1, 173)$value > 1 && flagval(par, 1, 184)$value > 0) {
              growth_devs_age(par) <- growth_devs_age(par) * exp(rnorm(nAge, 0, cv))
            }
            
            # Region Parameters
            if (any(flagval(par, -100000, 1:nReg)$value == 1)) {
              estRegs <- flagval(par, -100000, 1:nReg)$value == 1
              region_pars(par)[1, estRegs] <- region_pars(par)[1, estRegs] *
                exp(rnorm(length(region_pars(par)[1, estRegs]), 0, cv))
            }
            
            # Von Bertalanffy Parameters
            if (flagval(par, 1, 12)$value == 1) {
              growth(par)[1] <- growth(par)[1] * exp(rnorm(1, 0, cv))
            }
            if (flagval(par, 1, 13)$value == 1) {
              growth(par)[2] <- growth(par)[2] * exp(rnorm(1, 0, cv))
            }
            if (flagval(par, 1, 14)$value == 1) {
              growth(par)[3] <- growth(par)[3] * exp(rnorm(1, 0, cv))
            }
            
            # Richards Shape Parameter
            if (flagval(par, 1, 227)$value == 1) {
              current_val <- richards(par)
              if (abs(current_val) > 0.01) {
                richards(par) <- current_val * exp(rnorm(1, 0, cv))
              } else {
                richards(par) <- current_val + 0.1 * rnorm(1, 0, cv)
              }
            }
            
            # Growth Variance Parameters
            if (flagval(par, 1, 15)$value == 1) {
              original_val <- growth_var_pars(par)[1]
              lower_bound <- growth_var_pars(par)[1, 2]
              upper_bound <- growth_var_pars(par)[1, 3]
              
              max_attempts <- 100
              attempt <- 0
              new_val <- original_val * exp(rnorm(1, 0, cv))
              
              while ((new_val < lower_bound || new_val > upper_bound) && attempt < max_attempts) {
                new_val <- original_val * exp(rnorm(1, 0, cv))
                attempt <- attempt + 1
              }
              
              if (new_val >= lower_bound && new_val <= upper_bound) {
                growth_var_pars(par)[1] <- new_val
              } else {
                warning(sprintf("Growth variance parameter 1: Could not find valid jittered value within bounds after %d attempts. Keeping original value.", max_attempts))
              }
            }
            
            if (flagval(par, 1, 16)$value == 1) {
              original_val <- growth_var_pars(par)[2]
              lower_bound <- growth_var_pars(par)[2, 2]
              upper_bound <- growth_var_pars(par)[2, 3]
              
              max_attempts <- 100
              attempt <- 0
              new_val <- original_val * exp(rnorm(1, 0, cv))
              
              while ((new_val < lower_bound || new_val > upper_bound) && attempt < max_attempts) {
                new_val <- original_val * exp(rnorm(1, 0, cv))
                attempt <- attempt + 1
              }
              
              if (new_val >= lower_bound && new_val <= upper_bound) {
                growth_var_pars(par)[2] <- new_val
              } else {
                warning(sprintf("Growth variance parameter 2: Could not find valid jittered value within bounds after %d attempts. Keeping original value.", max_attempts))
              }
            }
            
            # Catch Deviation Coefficients
            if (any(flagval(par, -1:-nFish, 10)$value == 1)) {
              for (i in 1:max(flagval(par, -1:-nFish, 29)$value)) {
                matcher <- which(flagval(par, -1:-nFish, 29)$value == i)
                if (flagval(par, -matcher[1], 10)$value == 1) {
                  current_vals <- catch_dev_coffs(par)[[i]]
                  scale_factors <- pmax(abs(current_vals), 0.1)
                  catch_dev_coffs(par)[[i]] <- current_vals + 
                    scale_factors * rnorm(length(current_vals), 0, cv)
                }
              }
            }
            
            return(par)
          }
)


# ==============================================================================
# CLEAN & GENERIC METHOD
# ==============================================================================
#
# - Most parameters: multiplicative jitter (value * exp(rnorm(0, cv)))
# - Selectivity: logit transformation to handle [0,1] bounds properly
# - Regional recruitment: log-normal jitter + renormalization (k_factor = 0.62)
#
# This method works consistently across CV range 0.01-0.25
# ==============================================================================
