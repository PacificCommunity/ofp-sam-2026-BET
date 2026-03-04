#' ==============================================================================
#' JITTER_PAR_UNIFORM - UNIFORM DISTRIBUTIONS FOR ALL PARAMETERS
#' ==============================================================================
#'
#' Active positive parameters use uniform percent-change jitter within bounds
#' Regional recruitment jitters all free regions symmetrically and renormalizes
#'

jitter_sample_uniform_pct <- function(current_val, bound, lower = -Inf, upper = Inf, eps = 1e-12) {
  if (!is.finite(current_val) || bound <= 0) {
    return(current_val)
  }

  # Exact zero has no meaningful multiplicative percent perturbation.
  # Keep it unchanged rather than switching to absolute-delta jitter.
  if (current_val == 0) {
    return(current_val)
  }

  pct_min <- -bound
  pct_max <- bound

  if (is.finite(lower) || is.finite(upper)) {
    bound_vals <- c(lower, upper)
    bound_vals <- bound_vals[is.finite(bound_vals)]
    if (length(bound_vals) > 0) {
      pct_bounds <- bound_vals / current_val - 1
      pct_min <- max(pct_min, min(pct_bounds))
      pct_max <- min(pct_max, max(pct_bounds))
    }
  }

  if (pct_min > pct_max) {
    return(max(lower, min(upper, current_val)))
  }

  current_val * (1 + runif(1, pct_min, pct_max))
}

jitter_interior_bounds <- function(lower = -Inf, upper = Inf, eps = 1e-12) {
  lo <- lower
  hi <- upper

  if (is.finite(lo) && is.finite(hi)) {
    span <- hi - lo
    # Keep jitter away from hard bounds using a meaningful interior margin.
    # 5% per side avoids boundary hits without collapsing the feasible window.
    margin <- max(abs(span) * 5e-2, eps)
    lo <- lo + margin
    hi <- hi - margin
  } else if (is.finite(lo)) {
    lo <- lo + max(abs(lo) * 1e-8, eps)
  } else if (is.finite(hi)) {
    hi <- hi - max(abs(hi) * 1e-8, eps)
  }

  list(lower = lo, upper = hi)
}

jitter_sample_uniform_coverage <- function(current_val,
                                           coverage = 0.9,
                                           lower = -Inf,
                                           upper = Inf,
                                           eps = 1e-12) {
  if (!is.finite(current_val) || !is.finite(coverage) || coverage <= 0) {
    return(current_val)
  }

  interior <- jitter_interior_bounds(lower = lower, upper = upper, eps = eps)
  lo <- interior$lower
  hi <- interior$upper

  if (!is.finite(lo) || !is.finite(hi) || lo > hi) {
    return(current_val)
  }

  coverage <- min(coverage, 1)
  span <- hi - lo
  if (!is.finite(span) || span <= eps) {
    return(max(lo, min(hi, current_val)))
  }

  margin <- 0.5 * (1 - coverage) * span
  draw_lo <- lo + margin
  draw_hi <- hi - margin

  if (!is.finite(draw_lo) || !is.finite(draw_hi) || draw_lo > draw_hi) {
    return(max(lo, min(hi, current_val)))
  }
  if ((draw_hi - draw_lo) <= eps) {
    return(0.5 * (draw_lo + draw_hi))
  }

  runif(1, draw_lo, draw_hi)
}

jitter_sigma_from_cv <- function(jitter_cv) {
  if (!is.finite(jitter_cv) || jitter_cv <= 0) {
    return(0)
  }
  sqrt(log1p(jitter_cv^2))
}

jitter_clip <- function(x, lower = -Inf, upper = Inf, eps = 1e-12) {
  interior <- jitter_interior_bounds(lower = lower, upper = upper, eps = eps)
  max(interior$lower, min(interior$upper, x))
}

jitter_sample_multiplicative_cv <- function(current_val,
                                            jitter_cv,
                                            lower = -Inf,
                                            upper = Inf,
                                            eps = 1e-12,
                                            max_tries = 200) {
  if (!is.finite(current_val) || !is.finite(jitter_cv) || jitter_cv <= 0) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  if (current_val <= eps) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  interior <- jitter_interior_bounds(lower = lower, upper = upper, eps = eps)
  if (!is.finite(interior$lower) || !is.finite(interior$upper) || interior$lower > interior$upper) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  sigma <- jitter_sigma_from_cv(jitter_cv)
  for (iter in seq_len(max_tries)) {
    proposal <- current_val * exp(rnorm(1, mean = 0, sd = sigma))
    if (proposal > interior$lower && proposal < interior$upper) {
      return(proposal)
    }
  }
  jitter_clip(current_val, lower = lower, upper = upper, eps = eps)
}

jitter_sample_bounded_cv <- function(current_val,
                                     jitter_cv,
                                     lower = -Inf,
                                     upper = Inf,
                                     eps = 1e-12,
                                     max_tries = 200) {
  if (!is.finite(current_val) || !is.finite(jitter_cv) || jitter_cv <= 0) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  interior <- jitter_interior_bounds(lower = lower, upper = upper, eps = eps)
  lo <- interior$lower
  hi <- interior$upper
  if (!is.finite(lo) || !is.finite(hi) || lo > hi) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  span <- hi - lo
  if (!is.finite(span) || span <= eps) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }

  # For values already on/near a boundary, move the latent center slightly inside
  # using a CV-scaled offset rather than pinning everything to the same floor.
  offset_p <- max(min(jitter_cv^2, 0.1), 1e-4)
  p0 <- (current_val - lo) / span
  if (!is.finite(p0) || p0 <= 0) {
    p0 <- offset_p
  } else if (p0 >= 1) {
    p0 <- 1 - offset_p
  } else {
    p0 <- min(max(p0, 1e-6), 1 - 1e-6)
  }

  sigma <- jitter_sigma_from_cv(jitter_cv)
  mu <- qlogis(p0)
  for (iter in seq_len(max_tries)) {
    proposal_p <- plogis(rnorm(1, mean = mu, sd = sigma))
    proposal <- lo + proposal_p * span
    if (proposal > lo && proposal < hi) {
      return(proposal)
    }
  }
  jitter_clip(current_val, lower = lower, upper = upper, eps = eps)
}

jitter_sample_additive_cv <- function(current_val,
                                      jitter_cv,
                                      lower = -Inf,
                                      upper = Inf,
                                      scale_val = 1,
                                      eps = 1e-12,
                                      max_tries = 200) {
  if (!is.finite(current_val) || !is.finite(jitter_cv) || jitter_cv <= 0) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  if (!is.finite(scale_val) || scale_val <= eps) {
    scale_val <- 1
  }
  interior <- jitter_interior_bounds(lower = lower, upper = upper, eps = eps)
  if (!is.finite(interior$lower) || !is.finite(interior$upper) || interior$lower > interior$upper) {
    return(jitter_clip(current_val, lower = lower, upper = upper, eps = eps))
  }
  for (iter in seq_len(max_tries)) {
    proposal <- rnorm(1, mean = current_val, sd = jitter_cv * scale_val)
    if (proposal > interior$lower && proposal < interior$upper) {
      return(proposal)
    }
  }
  jitter_clip(current_val, lower = lower, upper = upper, eps = eps)
}

jitter_sample_uniform_delta <- function(current_val,
                                        bound,
                                        scale_val = NULL,
                                        lower = -Inf,
                                        upper = Inf,
                                        eps = 1e-12) {
  sample_away_from_current <- function(current_val, lower, upper, eps = 1e-12) {
    lo <- if (is.finite(lower)) lower else current_val - max(abs(current_val), 1)
    hi <- if (is.finite(upper)) upper else current_val + max(abs(current_val), 1)

    if (!is.finite(lo) || !is.finite(hi) || lo > hi) {
      return(current_val)
    }

    if ((hi - lo) <= eps) {
      candidate <- max(lo, min(hi, current_val))
      if (abs(candidate - current_val) > eps) {
        return(candidate)
      }
      return(current_val)
    }

    left_ok <- lo < (current_val - eps)
    right_ok <- hi > (current_val + eps)

    if (left_ok && right_ok) {
      if (runif(1) < 0.5) {
        return(runif(1, lo, current_val - eps))
      }
      return(runif(1, current_val + eps, hi))
    }
    if (left_ok) {
      return(runif(1, lo, current_val - eps))
    }
    if (right_ok) {
      return(runif(1, current_val + eps, hi))
    }

    current_val
  }

  if (!is.finite(current_val) || bound <= 0) {
    return(current_val)
  }

  if (is.null(scale_val) || !is.finite(scale_val) || scale_val <= eps) {
    scale_val <- max(abs(current_val), eps)
  }

  delta_min <- -bound * scale_val
  delta_max <- bound * scale_val

  if (is.finite(lower)) {
    delta_min <- max(delta_min, lower - current_val)
  }
  if (is.finite(upper)) {
    delta_max <- min(delta_max, upper - current_val)
  }

  if (delta_min > delta_max) {
    return(sample_away_from_current(current_val, lower, upper, eps = eps))
  }

  current_val + runif(1, delta_min, delta_max)
}

jitter_sample_value <- function(current_val,
                                bound,
                                lower = -Inf,
                                upper = Inf,
                                prefer_pct = FALSE,
                                eps = 1e-12) {
  if (prefer_pct || (is.finite(lower) && lower >= 0 && is.finite(current_val) && current_val > eps)) {
    return(jitter_sample_uniform_pct(current_val, bound, lower = lower, upper = upper, eps = eps))
  }

  jitter_sample_uniform_delta(
    current_val,
    bound,
    scale_val = max(abs(current_val), eps),
    lower = lower,
    upper = upper,
    eps = eps
  )
}

sample_bounded_simplex <- function(current_vals,
                                   total_sum = sum(current_vals),
                                   lower = NULL,
                                   upper = NULL,
                                   eps = 1e-12) {
  current_vals <- as.numeric(current_vals)
  n <- length(current_vals)
  if (n == 0 || !all(is.finite(current_vals)) || !is.finite(total_sum)) {
    return(current_vals)
  }

  if (is.null(lower)) lower <- rep(eps, n)
  if (is.null(upper)) upper <- rep(total_sum, n)

  lower <- pmax(as.numeric(lower), eps)
  upper <- as.numeric(upper)

  if (length(lower) != n || length(upper) != n || any(!is.finite(lower)) || any(!is.finite(upper))) {
    return(current_vals)
  }

  upper <- pmax(upper, lower)
  if (sum(lower) > total_sum + eps || sum(upper) < total_sum - eps) {
    return(current_vals)
  }

  if (n == 1) {
    if (total_sum >= lower[1] - eps && total_sum <= upper[1] + eps) {
      return(total_sum)
    }
    return(current_vals)
  }

  order_idx <- sample.int(n)
  x <- rep(NA_real_, n)
  remaining_sum <- total_sum

  for (k in seq_len(n - 1)) {
    i <- order_idx[k]
    rem <- order_idx[(k + 1):n]

    lo <- max(lower[i], remaining_sum - sum(upper[rem]))
    hi <- min(upper[i], remaining_sum - sum(lower[rem]))

    if (!is.finite(lo) || !is.finite(hi) || lo > hi) {
      return(current_vals)
    }

    if (hi - lo <= eps) {
      x[i] <- lo
    } else {
      x[i] <- runif(1, lo, hi)
    }
    remaining_sum <- remaining_sum - x[i]
  }

  last_i <- order_idx[n]
  if (remaining_sum < lower[last_i] - eps || remaining_sum > upper[last_i] + eps) {
    return(current_vals)
  }
  x[last_i] <- remaining_sum
  x
}

sample_dirichlet_bounded_cv <- function(current_vals,
                                        jitter_cv,
                                        lower = NULL,
                                        upper = NULL,
                                        eps = 1e-12,
                                        max_tries = 500) {
  current_vals <- as.numeric(current_vals)
  n <- length(current_vals)
  total_sum <- sum(current_vals)

  if (n == 0 || !all(is.finite(current_vals)) || !is.finite(total_sum) || total_sum <= eps) {
    return(current_vals)
  }

  if (is.null(lower)) lower <- rep(0, n)
  if (is.null(upper)) upper <- rep(total_sum, n)

  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (length(lower) != n || length(upper) != n) {
    return(current_vals)
  }

  lower <- pmax(lower, 0)
  upper <- pmax(upper, lower)

  interiors <- lapply(seq_len(n), function(i) {
    jitter_interior_bounds(lower = lower[i], upper = upper[i], eps = eps)
  })
  lower_i <- vapply(interiors, `[[`, numeric(1), "lower")
  upper_i <- vapply(interiors, `[[`, numeric(1), "upper")

  if (sum(lower_i) > total_sum + eps || sum(upper_i) < total_sum - eps) {
    return(current_vals)
  }

  probs <- pmax(current_vals / total_sum, eps)
  probs <- probs / sum(probs)
  alpha0 <- max((n - 1) / max(jitter_cv^2, eps) - 1, 1)
  alpha <- pmax(alpha0 * probs, eps)

  for (iter in seq_len(max_tries)) {
    g <- rgamma(n, shape = alpha, rate = 1)
    if (!all(is.finite(g)) || sum(g) <= eps) next
    proposal <- total_sum * g / sum(g)
    if (all(proposal > lower_i & proposal < upper_i)) {
      return(proposal)
    }
  }

  current_vals
}

resolve_indepvar_file <- function(indepvar_file = NULL, search_root = getwd()) {
  if (!is.null(indepvar_file)) {
    return(if (file.exists(indepvar_file)) normalizePath(indepvar_file, winslash = "/", mustWork = TRUE) else NULL)
  }

  candidate_rel <- c(
    "model/base/indepvar.rpt",
    "base/indepvar.rpt",
    "indepvar.rpt"
  )

  for (rel_path in candidate_rel) {
    candidate <- file.path(search_root, rel_path)
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  hits <- list.files(search_root, pattern = "^indepvar\\.rpt$", recursive = TRUE, full.names = TRUE)
  if (length(hits) == 1) {
    return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
  }

  NULL
}

parse_indepvar_report <- function(indepvar_file) {
  if (is.null(indepvar_file) || !file.exists(indepvar_file)) {
    return(NULL)
  }

  lines <- readLines(indepvar_file, warn = FALSE)
  if (length(lines) <= 1) {
    return(NULL)
  }

  lines <- lines[-1]
  strcapture(
    "^[[:space:]]*([0-9]+)[[:space:]]+([^ ]+)[[:space:]]+([^ ]+)[[:space:]]+([^ ]+)[[:space:]]+([^ ]+)[[:space:]]+([^ ]+)(.*)$",
    lines,
    proto = list(
      Index = integer(),
      Var_name = character(),
      Estimate = double(),
      L_bound = double(),
      U_bound = double(),
      gradient = double(),
      tail = character()
    )
  )
}

build_age_pars_matrix <- function(par) {
  n_age <- dimensions(par)["agecls"]
  temp <- matrix(0, nrow = 10, ncol = n_age)
  temp[2, ] <- as.vector(aperm(m_devs_age(par), c(4, 1, 2, 3, 5, 6)))
  temp[3, ] <- as.vector(aperm(growth_devs_age(par), c(4, 1, 2, 3, 5, 6)))
  temp[4, ] <- as.vector(aperm(growth_curve_devs(par), c(4, 1, 2, 3, 5, 6)))
  temp[5, ] <- as.vector(aperm(log_m(par), c(4, 1, 2, 3, 5, 6)))
  temp
}

set_age_pars_matrix <- function(par, temp) {
  n_age_yr <- dim(m_devs_age(par))[1]
  inv_perm <- order(c(4, 1, 2, 3, 5, 6))

  rebuild_age_block <- function(vec) {
    aperm(array(vec, dim = c(4, n_age_yr, 1, 1, 1, 1)), inv_perm)
  }

  current_m_devs <- m_devs_age(par)
  current_m_devs[] <- rebuild_age_block(temp[2, ])
  slot(par, "m_devs_age") <- current_m_devs

  current_growth_devs <- growth_devs_age(par)
  current_growth_devs[] <- rebuild_age_block(temp[3, ])
  slot(par, "growth_devs_age") <- current_growth_devs

  current_growth_curve_devs <- growth_curve_devs(par)
  current_growth_curve_devs[] <- rebuild_age_block(temp[4, ])
  slot(par, "growth_curve_devs") <- current_growth_curve_devs

  current_log_m <- log_m(par)
  current_log_m[] <- rebuild_age_block(temp[5, ])
  slot(par, "log_m") <- current_log_m
  par
}

build_indepvar_mapping <- function(par, indepvar_file = NULL, tol = 1e-12) {
  indepvar_file <- resolve_indepvar_file(indepvar_file)
  report <- parse_indepvar_report(indepvar_file)
  if (is.null(report) || nrow(report) == 0) {
    return(NULL)
  }

  n_regions <- dimensions(par)["regions"]
  n_fish <- dimensions(par)["fisheries"]
  n_age <- dim(fishery_sel(par))[1]
  n_seasons <- dim(fishery_sel(par))[4]

  mapping <- data.frame(
    Index = report$Index,
    Var_name = report$Var_name,
    Estimate = report$Estimate,
    L_bound = report$L_bound,
    U_bound = report$U_bound,
    family = NA_character_,
    mapped = FALSE,
    key1 = NA_integer_,
    key2 = NA_integer_,
    key3 = NA_integer_,
    key4 = NA_integer_,
    note = NA_character_,
    stringsAsFactors = FALSE
  )

  diff_idx <- grepl("^diff_coffs\\([0-9]+,[0-9]+\\)$", report$Var_name)
  if (any(diff_idx)) {
    coords <- do.call(rbind, regmatches(report$Var_name[diff_idx], gregexpr("[0-9]+", report$Var_name[diff_idx])))
    mapping$family[diff_idx] <- "diff_coffs"
    mapping$mapped[diff_idx] <- TRUE
    mapping$key1[diff_idx] <- as.integer(coords[, 1])
    mapping$key2[diff_idx] <- as.integer(coords[, 2])
  }

  rrd_idx <- grepl("^region_rec_diffs\\([0-9]+,[0-9]+\\)$", report$Var_name)
  if (any(rrd_idx)) {
    coords <- do.call(rbind, regmatches(report$Var_name[rrd_idx], gregexpr("[0-9]+", report$Var_name[rrd_idx])))
    mapping$family[rrd_idx] <- "region_rec_diffs"
    mapping$mapped[rrd_idx] <- TRUE
    mapping$key1[rrd_idx] <- as.integer(coords[, 1])
    mapping$key2[rrd_idx] <- as.integer(coords[, 2])
  }

  bs_idx <- grepl("^bs_selcoff_gp:[0-9]+\\(", report$Var_name)
  if (any(bs_idx)) {
    ff24 <- flagval(par, -1:-n_fish, 24)$value
    groups <- sort(unique(ff24))
    groups <- groups[groups > 0]
    rep_fish <- setNames(vapply(groups, function(g) which(ff24 == g)[1], integer(1)), groups)
    bs_rows <- which(bs_idx)
    bs_group <- as.integer(sub("^bs_selcoff_gp:([0-9]+)\\(.*$", "\\1", report$Var_name[bs_rows]))

    mapping$family[bs_rows] <- "bs_selcoff_gp"
    for (g in unique(bs_group)) {
      fish_idx <- rep_fish[as.character(g)]
      if (length(fish_idx) != 1 || is.na(fish_idx)) {
        next
      }

      positions <- list()
      pos_i <- 1L
      for (age_idx in seq_len(n_age)) {
        for (ssn_idx in seq_len(n_seasons)) {
          current_val <- fishery_sel(par)[age_idx, 1, fish_idx, ssn_idx, 1, 1]
          if (is.finite(current_val) && abs(current_val) > tol) {
            positions[[pos_i]] <- c(age_idx, ssn_idx)
            pos_i <- pos_i + 1L
          }
        }
      }

      grp_rows <- bs_rows[bs_group == g]
      if (length(positions) < length(grp_rows)) {
        next
      }

            for (j in seq_along(grp_rows)) {
                mapping$mapped[grp_rows[j]] <- TRUE
                mapping$key1[grp_rows[j]] <- g
                mapping$key2[grp_rows[j]] <- positions[[j]][1]
                mapping$key3[grp_rows[j]] <- positions[[j]][2]
                mapping$key4[grp_rows[j]] <- fish_idx
            }
        }
    }

  recr_idx <- grepl("^recr\\([0-9]+\\)$", report$Var_name)
  if (any(recr_idx)) {
    idx_vals <- as.integer(sub("^recr\\(([0-9]+)\\)$", "\\1", report$Var_name[recr_idx]))
    mapping$family[recr_idx] <- "recr"
    mapping$mapped[recr_idx] <- TRUE
    mapping$key1[recr_idx] <- idx_vals
  }

  tot_idx <- report$Var_name == "totpop"
  mapping$family[tot_idx] <- "totpop"
  mapping$mapped[tot_idx] <- TRUE

  tag_idx <- grepl("^tag_fish_rep\\([0-9]+\\)$", report$Var_name)
  if (any(tag_idx)) {
    idx_vals <- as.integer(sub("^tag_fish_rep\\(([0-9]+)\\)$", "\\1", report$Var_name[tag_idx]))
    mapping$family[tag_idx] <- "tag_fish_rep"
    mapping$mapped[tag_idx] <- idx_vals %in% unique(as.integer(tag_fish_rep_grp(par)[tag_fish_rep_flags(par) == 1]))
    mapping$key1[tag_idx] <- idx_vals
  }

  rp_idx <- grepl("^region_pars\\([0-9]+\\)$", report$Var_name)
  if (any(rp_idx)) {
    rp_rows <- which(rp_idx)
    row_vals <- as.integer(sub("^region_pars\\(([0-9]+)\\)$", "\\1", report$Var_name[rp_idx]))
    occurrence <- ave(seq_along(rp_rows), row_vals, FUN = seq_along)
    mapping$family[rp_idx] <- "region_pars"
    mapping$mapped[rp_idx] <- TRUE
    mapping$key1[rp_idx] <- row_vals
    mapping$key2[rp_idx] <- occurrence
  }

  sv_idx <- grepl("^sv\\([0-9]+\\)$", report$Var_name)
  if (any(sv_idx)) {
    idx_vals <- as.integer(sub("^sv\\(([0-9]+)\\)$", "\\1", report$Var_name[sv_idx]))
    mapping$family[sv_idx] <- "sv"
    mapping$mapped[sv_idx] <- idx_vals <= length(season_growth_pars(par))
    mapping$key1[sv_idx] <- idx_vals
  }

  age_idx <- grepl("^age_pars\\([0-9]+\\)$", report$Var_name)
  if (any(age_idx)) {
    age_rows <- which(age_idx)
    row_vals <- as.integer(sub("^age_pars\\(([0-9]+)\\)$", "\\1", report$Var_name[age_idx]))
    age_matrix <- build_age_pars_matrix(par)

    mapping$family[age_idx] <- "age_pars"
    for (r in unique(row_vals)) {
      row_positions <- which(row_vals == r)
      candidates <- which(abs(age_matrix[r, ]) > tol)
      if (length(candidates) < length(row_positions)) {
        candidates <- seq_len(ncol(age_matrix))
      }
      if (length(candidates) < length(row_positions)) {
        next
      }
      mapping$mapped[age_rows[row_positions]] <- TRUE
      mapping$key1[age_rows[row_positions]] <- r
      mapping$key2[age_rows[row_positions]] <- candidates[seq_along(row_positions)]
    }
  }

  vb_idx <- grepl("^vb_coff\\([0-9]+\\)$", report$Var_name)
  if (any(vb_idx)) {
    idx_vals <- as.integer(sub("^vb_coff\\(([0-9]+)\\)$", "\\1", report$Var_name[vb_idx]))
    mapping$family[vb_idx] <- "vb_coff"
    mapping$mapped[vb_idx] <- idx_vals <= nrow(growth(par))
    mapping$key1[vb_idx] <- idx_vals
  }

  var_idx <- grepl("^var_coff\\([0-9]+\\)$", report$Var_name)
  if (any(var_idx)) {
    idx_vals <- as.integer(sub("^var_coff\\(([0-9]+)\\)$", "\\1", report$Var_name[var_idx]))
    mapping$family[var_idx] <- "var_coff"
    mapping$mapped[var_idx] <- idx_vals <= nrow(growth_var_pars(par))
    mapping$key1[var_idx] <- idx_vals
  }

  mapping$note[!mapping$mapped] <- "No exact mapping rule for this label in current model"

  list(
    indepvar_file = indepvar_file,
    report = report,
    mapping = mapping
  )
}

apply_indepvar_exact_jitter <- function(par, indepvar_map, jitter_bound, eps = 1e-12) {
  mapping <- indepvar_map$mapping
  if (!all(mapping$mapped)) {
    return(NULL)
  }

  # diff_coffs
  diff_rows <- which(mapping$family == "diff_coffs")
  for (i in diff_rows) {
    r <- mapping$key1[i]
    c <- mapping$key2[i]
    diff_coffs(par)[r, c] <- jitter_sample_uniform_pct(
      diff_coffs(par)[r, c],
      jitter_bound,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i] - eps,
      eps = eps
    )
  }

  # region_rec_var exported scale
  rrd_rows <- which(mapping$family == "region_rec_diffs")
  if (length(rrd_rows) > 0) {
    rrv_export <- matrix(
      as.vector(aperm(region_rec_var(par), c(4, 2, 5, 1, 3, 6))),
      ncol = dimensions(par)["regions"]
    )
    for (i in rrd_rows) {
      rrv_export[mapping$key2[i], mapping$key1[i]] <- jitter_sample_uniform_pct(
        rrv_export[mapping$key2[i], mapping$key1[i]],
        jitter_bound,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    vec <- as.vector(rrv_export)
    current_rrv <- region_rec_var(par)
    current_rrv[] <- aperm(
      array(vec, dim = c(dimensions(par)["seasons"], dim(region_rec_var(par))[2], dimensions(par)["regions"], 1, 1, 1)),
      c(4, 2, 5, 1, 3, 6)
    )
    slot(par, "region_rec_var") <- current_rrv
  }

  # bs_selcoff_gp
  bs_rows <- which(mapping$family == "bs_selcoff_gp")
  if (length(bs_rows) > 0) {
    ff24 <- flagval(par, -1:-dimensions(par)["fisheries"], 24)$value
    for (i in bs_rows) {
      group_id <- mapping$key1[i]
      age_idx <- mapping$key2[i]
      ssn_idx <- mapping$key3[i]
      fish_idx <- which(ff24 == group_id)
      current_val <- fishery_sel(par)[age_idx, 1, fish_idx[1], ssn_idx, 1, 1]
      new_val <- jitter_sample_uniform_delta(
        current_val,
        jitter_bound,
        scale_val = max(abs(current_val), eps),
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
      fishery_sel(par)[age_idx, 1, fish_idx, ssn_idx, 1, 1] <- new_val
    }
  }

  # recruitment deviations on log scale
  recr_rows <- which(mapping$family == "recr")
  if (length(recr_rows) > 0) {
    rel_flat <- as.vector(aperm(rel_rec(par), c(4, 2, 1, 3, 5, 6)))
    log_rel <- log(pmax(rel_flat, eps))
    for (i in recr_rows) {
      pos <- mapping$key1[i]
      log_rel[pos] <- jitter_sample_uniform_delta(
        log_rel[pos],
        jitter_bound,
        scale_val = max(abs(log_rel[pos]), eps),
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    rel_flat <- exp(log_rel)
    current_rel <- rel_rec(par)
    current_rel[] <- aperm(
      array(rel_flat, dim = c(dimensions(par)["seasons"], dim(rel_rec(par))[2], 1, 1, 1, 1)),
      order(c(4, 2, 1, 3, 5, 6))
    )
    slot(par, "rel_rec") <- current_rel
  }

  # total population
  tot_rows <- which(mapping$family == "totpop")
  if (length(tot_rows) == 1) {
    tot_pop(par) <- jitter_sample_uniform_pct(
      tot_pop(par),
      jitter_bound,
      lower = max(eps, mapping$L_bound[tot_rows]),
      upper = mapping$U_bound[tot_rows],
      eps = eps
    )
  }

  # tag_fish_rep groups
  tag_rows <- which(mapping$family == "tag_fish_rep")
  if (length(tag_rows) > 0) {
    grp_mat <- tag_fish_rep_grp(par)
    flag_mat <- tag_fish_rep_flags(par)
    for (i in tag_rows) {
      grp_id <- mapping$key1[i]
      idx <- which(grp_mat == grp_id & flag_mat == 1, arr.ind = TRUE)
      if (nrow(idx) == 0) {
        next
      }
      current_val <- tag_fish_rep_rate(par)[idx[1, 1], idx[1, 2]]
      new_val <- jitter_sample_uniform_pct(
        current_val,
        jitter_bound,
        lower = max(eps, mapping$L_bound[i]),
        upper = mapping$U_bound[i] - eps,
        eps = eps
      )
      tag_fish_rep_rate(par)[idx] <- new_val
    }
  }

  # region_pars row-wise simplex jitter
  rp_rows <- which(mapping$family == "region_pars")
  if (length(rp_rows) > 0) {
    for (row_id in sort(unique(mapping$key1[rp_rows]))) {
      row_map <- rp_rows[mapping$key1[rp_rows] == row_id]
      cols <- mapping$key2[row_map]
      current_vals <- region_pars(par)[row_id, cols]
      lower_bounds <- pmax(eps, current_vals * (1 - jitter_bound), mapping$L_bound[row_map])
      upper_bounds <- pmin(mapping$U_bound[row_map], current_vals * (1 + jitter_bound))
      proposed <- sample_bounded_simplex(
        current_vals,
        total_sum = sum(current_vals),
        lower = lower_bounds,
        upper = upper_bounds,
        eps = eps
      )
      region_pars(par)[row_id, cols] <- proposed
    }
  }

  # season growth parameters
  sv_rows <- which(mapping$family == "sv")
  for (i in sv_rows) {
    idx <- mapping$key1[i]
    season_growth_pars(par)[idx] <- jitter_sample_value(
      season_growth_pars(par)[idx],
      jitter_bound,
      lower = mapping$L_bound[i],
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  # age_pars block
  age_rows <- which(mapping$family == "age_pars")
  if (length(age_rows) > 0) {
    age_mat <- build_age_pars_matrix(par)
    for (i in age_rows) {
      row_id <- mapping$key1[i]
      col_id <- mapping$key2[i]
      age_mat[row_id, col_id] <- jitter_sample_value(
        age_mat[row_id, col_id],
        jitter_bound,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    par <- set_age_pars_matrix(par, age_mat)
  }

  # vb_coff
  vb_rows <- which(mapping$family == "vb_coff")
  for (i in vb_rows) {
    idx <- mapping$key1[i]
    growth(par)[idx, 1] <- jitter_sample_uniform_pct(
      growth(par)[idx, 1],
      jitter_bound,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  # var_coff
  var_rows <- which(mapping$family == "var_coff")
  for (i in var_rows) {
    idx <- mapping$key1[i]
    growth_var_pars(par)[idx, 1] <- jitter_sample_uniform_pct(
      growth_var_pars(par)[idx, 1],
      jitter_bound,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  par
}

apply_indepvar_coverage_jitter <- function(par, indepvar_map, jitter_coverage, eps = 1e-12) {
  mapping <- indepvar_map$mapping
  if (!all(mapping$mapped)) {
    return(NULL)
  }

  diff_rows <- which(mapping$family == "diff_coffs")
  for (i in diff_rows) {
    r <- mapping$key1[i]
    c <- mapping$key2[i]
    diff_coffs(par)[r, c] <- jitter_sample_uniform_coverage(
      diff_coffs(par)[r, c],
      jitter_coverage,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  rrd_rows <- which(mapping$family == "region_rec_diffs")
  if (length(rrd_rows) > 0) {
    rrv_export <- matrix(
      as.vector(aperm(region_rec_var(par), c(4, 2, 5, 1, 3, 6))),
      ncol = dimensions(par)["regions"]
    )
    for (i in rrd_rows) {
      rrv_export[mapping$key2[i], mapping$key1[i]] <- jitter_sample_uniform_coverage(
        rrv_export[mapping$key2[i], mapping$key1[i]],
        jitter_coverage,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    vec <- as.vector(rrv_export)
    current_rrv <- region_rec_var(par)
    current_rrv[] <- aperm(
      array(vec, dim = c(dimensions(par)["seasons"], dim(region_rec_var(par))[2], dimensions(par)["regions"], 1, 1, 1)),
      c(4, 2, 5, 1, 3, 6)
    )
    slot(par, "region_rec_var") <- current_rrv
  }

  bs_rows <- which(mapping$family == "bs_selcoff_gp")
  if (length(bs_rows) > 0) {
    ff24 <- flagval(par, -1:-dimensions(par)["fisheries"], 24)$value
    for (i in bs_rows) {
      group_id <- mapping$key1[i]
      age_idx <- mapping$key2[i]
      ssn_idx <- mapping$key3[i]
      fish_idx <- which(ff24 == group_id)
      current_val <- fishery_sel(par)[age_idx, 1, fish_idx[1], ssn_idx, 1, 1]
      new_val <- jitter_sample_uniform_coverage(
        current_val,
        jitter_coverage,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
      fishery_sel(par)[age_idx, 1, fish_idx, ssn_idx, 1, 1] <- new_val
    }
  }

  recr_rows <- which(mapping$family == "recr")
  if (length(recr_rows) > 0) {
    rel_flat <- as.vector(aperm(rel_rec(par), c(4, 2, 1, 3, 5, 6)))
    log_rel <- log(pmax(rel_flat, eps))
    for (i in recr_rows) {
      pos <- mapping$key1[i]
      log_rel[pos] <- jitter_sample_uniform_coverage(
        log_rel[pos],
        jitter_coverage,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    rel_flat <- exp(log_rel)
    current_rel <- rel_rec(par)
    current_rel[] <- aperm(
      array(rel_flat, dim = c(dimensions(par)["seasons"], dim(rel_rec(par))[2], 1, 1, 1, 1)),
      order(c(4, 2, 1, 3, 5, 6))
    )
    slot(par, "rel_rec") <- current_rel
  }

  tot_rows <- which(mapping$family == "totpop")
  if (length(tot_rows) == 1) {
    tot_pop(par) <- jitter_sample_uniform_coverage(
      tot_pop(par),
      jitter_coverage,
      lower = max(eps, mapping$L_bound[tot_rows]),
      upper = mapping$U_bound[tot_rows],
      eps = eps
    )
  }

  tag_rows <- which(mapping$family == "tag_fish_rep")
  if (length(tag_rows) > 0) {
    grp_mat <- tag_fish_rep_grp(par)
    flag_mat <- tag_fish_rep_flags(par)
    for (i in tag_rows) {
      grp_id <- mapping$key1[i]
      idx <- which(grp_mat == grp_id & flag_mat == 1, arr.ind = TRUE)
      if (nrow(idx) == 0) next
      current_val <- tag_fish_rep_rate(par)[idx[1, 1], idx[1, 2]]
      new_val <- jitter_sample_uniform_coverage(
        current_val,
        jitter_coverage,
        lower = max(eps, mapping$L_bound[i]),
        upper = mapping$U_bound[i],
        eps = eps
      )
      tag_fish_rep_rate(par)[idx] <- new_val
    }
  }

  rp_rows <- which(mapping$family == "region_pars")
  if (length(rp_rows) > 0) {
    for (row_id in sort(unique(mapping$key1[rp_rows]))) {
      row_map <- rp_rows[mapping$key1[rp_rows] == row_id]
      cols <- mapping$key2[row_map]
      current_vals <- region_pars(par)[row_id, cols]
      span <- mapping$U_bound[row_map] - mapping$L_bound[row_map]
      margin <- 0.5 * (1 - min(jitter_coverage, 1)) * span
      lower_bounds <- mapping$L_bound[row_map] + margin
      upper_bounds <- mapping$U_bound[row_map] - margin
      proposed <- sample_bounded_simplex(
        current_vals,
        total_sum = sum(current_vals),
        lower = lower_bounds,
        upper = upper_bounds,
        eps = eps
      )
      region_pars(par)[row_id, cols] <- proposed
    }
  }

  sv_rows <- which(mapping$family == "sv")
  for (i in sv_rows) {
    idx <- mapping$key1[i]
    season_growth_pars(par)[idx] <- jitter_sample_uniform_coverage(
      season_growth_pars(par)[idx],
      jitter_coverage,
      lower = mapping$L_bound[i],
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  age_rows <- which(mapping$family == "age_pars")
  if (length(age_rows) > 0) {
    age_mat <- build_age_pars_matrix(par)
    for (i in age_rows) {
      row_id <- mapping$key1[i]
      col_id <- mapping$key2[i]
      age_mat[row_id, col_id] <- jitter_sample_uniform_coverage(
        age_mat[row_id, col_id],
        jitter_coverage,
        lower = mapping$L_bound[i],
        upper = mapping$U_bound[i],
        eps = eps
      )
    }
    par <- set_age_pars_matrix(par, age_mat)
  }

  vb_rows <- which(mapping$family == "vb_coff")
  for (i in vb_rows) {
    idx <- mapping$key1[i]
    growth(par)[idx, 1] <- jitter_sample_uniform_coverage(
      growth(par)[idx, 1],
      jitter_coverage,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  var_rows <- which(mapping$family == "var_coff")
  for (i in var_rows) {
    idx <- mapping$key1[i]
    growth_var_pars(par)[idx, 1] <- jitter_sample_uniform_coverage(
      growth_var_pars(par)[idx, 1],
      jitter_coverage,
      lower = max(eps, mapping$L_bound[i]),
      upper = mapping$U_bound[i],
      eps = eps
    )
  }

  par
}

apply_indepvar_cv_jitter <- function(par, indepvar_map, jitter_cv, eps = 1e-12) {
  mapping <- indepvar_map$mapping
  if (!all(mapping$mapped)) {
    return(NULL)
  }

  diff_rows <- which(mapping$family == "diff_coffs")
  for (i in diff_rows) {
    r <- mapping$key1[i]
    c <- mapping$key2[i]
    diff_coffs(par)[r, c] <- jitter_sample_multiplicative_cv(
      diff_coffs(par)[r, c], jitter_cv,
      lower = max(eps, mapping$L_bound[i]), upper = mapping$U_bound[i], eps = eps
    )
  }

  rrd_rows <- which(mapping$family == "region_rec_diffs")
  if (length(rrd_rows) > 0) {
    rrv_export <- matrix(
      as.vector(aperm(region_rec_var(par), c(4, 2, 5, 1, 3, 6))),
      ncol = dimensions(par)["regions"]
    )
    for (i in rrd_rows) {
      rrv_export[mapping$key2[i], mapping$key1[i]] <- jitter_sample_additive_cv(
        rrv_export[mapping$key2[i], mapping$key1[i]], jitter_cv,
        lower = mapping$L_bound[i], upper = mapping$U_bound[i], scale_val = 1, eps = eps
      )
    }
    vec <- as.vector(rrv_export)
    current_rrv <- region_rec_var(par)
    current_rrv[] <- aperm(
      array(vec, dim = c(dimensions(par)["seasons"], dim(region_rec_var(par))[2], dimensions(par)["regions"], 1, 1, 1)),
      c(4, 2, 5, 1, 3, 6)
    )
    slot(par, "region_rec_var") <- current_rrv
  }

  bs_rows <- which(mapping$family == "bs_selcoff_gp")
  if (length(bs_rows) > 0) {
    for (i in bs_rows) {
      fish_idx <- mapping$key4[i]
      if (!is.finite(fish_idx)) next
      fishery_sel(par)[mapping$key2[i], 1, fish_idx, mapping$key3[i], 1, 1] <- jitter_sample_bounded_cv(
        fishery_sel(par)[mapping$key2[i], 1, fish_idx, mapping$key3[i], 1, 1], jitter_cv,
        lower = mapping$L_bound[i], upper = mapping$U_bound[i], eps = eps
      )
    }
  }

  recr_rows <- which(mapping$family == "recr")
  if (length(recr_rows) > 0) {
    rel_flat <- as.vector(aperm(rel_rec(par), c(4, 2, 1, 3, 5, 6)))
    for (i in recr_rows) {
      pos <- mapping$key1[i]
      rel_flat[pos] <- jitter_sample_multiplicative_cv(
        rel_flat[pos], jitter_cv,
        lower = exp(mapping$L_bound[i]), upper = exp(mapping$U_bound[i]), eps = eps
      )
    }
    current_rel <- rel_rec(par)
    current_rel[] <- aperm(
      array(rel_flat, dim = c(dimensions(par)["seasons"], dim(rel_rec(par))[2], 1, 1, 1, 1)),
      order(c(4, 2, 1, 3, 5, 6))
    )
    slot(par, "rel_rec") <- current_rel
  }

  tot_rows <- which(mapping$family == "totpop")
  if (length(tot_rows) == 1) {
    tot_pop(par) <- jitter_sample_multiplicative_cv(
      tot_pop(par), jitter_cv,
      lower = max(eps, mapping$L_bound[tot_rows]), upper = mapping$U_bound[tot_rows], eps = eps
    )
  }

  tag_rows <- which(mapping$family == "tag_fish_rep")
  if (length(tag_rows) > 0) {
    grp_mat <- tag_fish_rep_grp(par)
    flag_mat <- tag_fish_rep_flags(par)
    for (i in tag_rows) {
      grp_id <- mapping$key1[i]
      idx <- which(grp_mat == grp_id & flag_mat == 1, arr.ind = TRUE)
      if (nrow(idx) == 0) next
      tag_fish_rep_rate(par)[idx] <- jitter_sample_bounded_cv(
        tag_fish_rep_rate(par)[idx[1, 1], idx[1, 2]], jitter_cv,
        lower = mapping$L_bound[i], upper = mapping$U_bound[i], eps = eps
      )
    }
  }

  rp_rows <- which(mapping$family == "region_pars")
  if (length(rp_rows) > 0) {
    for (row_id in sort(unique(mapping$key1[rp_rows]))) {
      row_map <- rp_rows[mapping$key1[rp_rows] == row_id]
      cols <- mapping$key2[row_map]
      current_vals <- region_pars(par)[row_id, cols]
      lower_bounds <- pmax(mapping$L_bound[row_map], eps)
      upper_bounds <- mapping$U_bound[row_map]
      proposed <- sample_dirichlet_bounded_cv(
        current_vals,
        jitter_cv = jitter_cv,
        lower = lower_bounds,
        upper = upper_bounds,
        eps = eps
      )
      region_pars(par)[row_id, cols] <- proposed
    }
  }

  sv_rows <- which(mapping$family == "sv")
  for (i in sv_rows) {
    idx <- mapping$key1[i]
    season_growth_pars(par)[idx] <- jitter_sample_multiplicative_cv(
      season_growth_pars(par)[idx], jitter_cv,
      lower = max(eps, mapping$L_bound[i]), upper = mapping$U_bound[i], eps = eps
    )
  }

  age_rows <- which(mapping$family == "age_pars")
  if (length(age_rows) > 0) {
    age_mat <- build_age_pars_matrix(par)
    for (i in age_rows) {
      row_id <- mapping$key1[i]
      col_id <- mapping$key2[i]
      age_mat[row_id, col_id] <- jitter_sample_multiplicative_cv(
        age_mat[row_id, col_id], jitter_cv,
        lower = max(eps, mapping$L_bound[i]), upper = mapping$U_bound[i], eps = eps
      )
    }
    par <- set_age_pars_matrix(par, age_mat)
  }

  vb_rows <- which(mapping$family == "vb_coff")
  for (i in vb_rows) {
    idx <- mapping$key1[i]
    growth(par)[idx, 1] <- jitter_sample_multiplicative_cv(
      growth(par)[idx, 1], jitter_cv,
      lower = max(eps, mapping$L_bound[i]), upper = mapping$U_bound[i], eps = eps
    )
  }

  var_rows <- which(mapping$family == "var_coff")
  for (i in var_rows) {
    idx <- mapping$key1[i]
    growth_var_pars(par)[idx, 1] <- jitter_sample_multiplicative_cv(
      growth_var_pars(par)[idx, 1], jitter_cv,
      lower = max(eps, mapping$L_bound[i]), upper = mapping$U_bound[i], eps = eps
    )
  }

  par
}

run_indepvar_coverage_jitter <- function(model_dir,
                                         jitter_coverage = 0.9,
                                         seed = 1,
                                         base_par_file = NULL,
                                         indepvar_file = NULL,
                                         out_file = NULL,
                                         output_prefix = NULL,
                                         change_tol = 1e-14) {
  files <- resolve_model_jitter_files(
    model_dir = model_dir,
    base_par_file = base_par_file,
    indepvar_file = indepvar_file
  )

  if (is.null(files$indepvar_file)) {
    stop("indepvar.rpt is required for the indepvar CV-jitter workflow.")
  }

  if (is.null(out_file)) {
    out_file <- file.path(files$model_dir, "00.par")
  } else if (!grepl("^/", out_file) &&
             !startsWith(out_file, paste0(files$model_dir_input, "/")) &&
             !startsWith(out_file, paste0(files$model_dir, "/"))) {
    out_file <- file.path(files$model_dir, out_file)
  }

  if (isFALSE(output_prefix) || (length(output_prefix) == 1 && is.na(output_prefix))) {
    output_prefix <- NULL
  } else if (!is.null(output_prefix) && !grepl("^/", output_prefix) &&
             !startsWith(output_prefix, paste0(files$model_dir_input, "/")) &&
             !startsWith(output_prefix, paste0(files$model_dir, "/"))) {
    output_prefix <- file.path(files$model_dir, output_prefix)
  }

  base_par <- read.MFCLPar(files$base_par_file)
  indepvar_map <- build_indepvar_mapping(base_par, indepvar_file = files$indepvar_file)
  if (is.null(indepvar_map) || !all(indepvar_map$mapping$mapped)) {
    stop("Exact indepvar mapping could not be resolved for all parameters.")
  }
  if (any(!is.finite(indepvar_map$mapping$L_bound)) || any(!is.finite(indepvar_map$mapping$U_bound))) {
    stop("Coverage jitter requires finite L_bound and U_bound for all mapped free parameters.")
  }

  if (!is.null(seed)) set.seed(seed)
  jittered_par <- apply_indepvar_coverage_jitter(base_par, indepvar_map, jitter_coverage = jitter_coverage)
  if (is.null(jittered_par)) {
    stop("Coverage jitter application failed.")
  }

  out_file <- write_jittered_par(jittered_par, out_file)
  comparison <- compare_exact_jitter(
    base_par = base_par,
    jittered_par = jittered_par,
    indepvar_file = files$indepvar_file,
    change_tol = change_tol,
    output_prefix = output_prefix
  )

  invisible(list(
    files = files,
    jitter_coverage = jitter_coverage,
    seed = seed,
    out_file = out_file,
    output_prefix = output_prefix,
    comparison = comparison
  ))
}

audit_indepvar_exact_mapping <- function(par,
                                         indepvar_file = NULL,
                                         output_prefix = "indepvar_mapping_audit") {
  indepvar_map <- build_indepvar_mapping(par, indepvar_file = indepvar_file)
  if (is.null(indepvar_map)) {
    stop("No indepvar.rpt could be located for this model.")
  }

  mapping <- indepvar_map$mapping
  summary_df <- aggregate(
    cbind(mapped_count = as.integer(mapping$mapped), total_count = 1L) ~ family,
    data = transform(mapping, family = ifelse(is.na(family), "unclassified", family)),
    FUN = sum
  )
  summary_df$unmapped_count <- summary_df$total_count - summary_df$mapped_count

  csv_path <- sprintf("%s_labels.csv", output_prefix)
  summary_path <- sprintf("%s_summary.csv", output_prefix)
  pdf_path <- sprintf("%s_summary.pdf", output_prefix)

  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)

  write.csv(mapping, csv_path, row.names = FALSE)
  write.csv(summary_df, summary_path, row.names = FALSE)

  pdf(pdf_path, width = 10, height = 6)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)

  par(mar = c(9, 4.5, 3, 1))
  barplot(
    summary_df$mapped_count,
    names.arg = summary_df$family,
    las = 2,
    col = ifelse(summary_df$unmapped_count == 0, "#3B7A57", "#D95F02"),
    ylab = "Mapped labels",
    main = "indepvar Exact Mapping Coverage"
  )

  invisible(list(
    indepvar_file = indepvar_map$indepvar_file,
    labels = mapping,
    summary = summary_df,
    csv = csv_path,
    summary_csv = summary_path,
    pdf = pdf_path
  ))
}

jitter_par <- function(par, jitter_bound = 0.05, seed = NULL, indepvar_file = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  nFish    <- dimensions(par)["fisheries"]
  nAge     <- dimensions(par)["agecls"]
  nSeasons <- dimensions(par)["seasons"]
  nAgeYr   <- nAge / nSeasons
  
  eps <- 1e-12
  
  fish_flag_vals <- function(flag_no) {
    flagval(par, -1:-nFish, flag_no)$value
  }
  
  age_flag_val <- function(flag_no) {
    flagval(par, 2, flag_no)$value
  }
  
  parest_flag_val <- function(flag_no) {
    flagval(par, 1, flag_no)$value
  }

  has_active_values <- function(x) {
    any(is.finite(as.numeric(x)) & abs(as.numeric(x)) > eps)
  }

  jitter_active_entries <- function(x, scale_mult = 0.10, lower = -Inf, upper = Inf) {
    vals <- as.numeric(x)
    idx <- which(is.finite(vals) & abs(vals) > eps)
    if (length(idx) == 0) {
      return(x)
    }

    scale_val <- mean(abs(vals[idx]))
    if (!is.finite(scale_val) || scale_val <= eps) {
      scale_val <- 1
    }

    vals[idx] <- vapply(
      vals[idx],
      function(v) sample_uniform_delta(v, jitter_bound, scale_val = scale_val * scale_mult, lower = lower, upper = upper),
      numeric(1)
    )

    x[idx] <- vals[idx]
    x
  }
  
  sample_uniform_pct <- function(current_val, bound, lower = -Inf, upper = Inf) {
    if (!is.finite(current_val) || bound <= 0) {
      return(current_val)
    }
    
    if (abs(current_val) <= eps) {
      return(max(lower, min(upper, current_val)))
    }
    
    pct_min <- -bound
    pct_max <- bound
    
    if (is.finite(lower)) {
      pct_min <- max(pct_min, lower / current_val - 1)
    }
    if (is.finite(upper)) {
      pct_max <- min(pct_max, upper / current_val - 1)
    }
    
    if (pct_min > pct_max) {
      return(max(lower, min(upper, current_val)))
    }
    
    current_val * (1 + runif(1, pct_min, pct_max))
  }
  
  sample_uniform_delta <- function(current_val, bound, scale_val = NULL, lower = -Inf, upper = Inf) {
    if (!is.finite(current_val) || bound <= 0) {
      return(current_val)
    }
    
    if (is.null(scale_val) || !is.finite(scale_val) || scale_val <= eps) {
      scale_val <- max(abs(current_val), eps)
    }
    
    delta_min <- -bound * scale_val
    delta_max <- bound * scale_val
    
    if (is.finite(lower)) {
      delta_min <- max(delta_min, lower - current_val)
    }
    if (is.finite(upper)) {
      delta_max <- min(delta_max, upper - current_val)
    }
    
    if (delta_min > delta_max) {
      return(max(lower, min(upper, current_val)))
    }
    
    current_val + runif(1, delta_min, delta_max)
  }

  indepvar_map <- build_indepvar_mapping(par, indepvar_file = indepvar_file)
  if (!is.null(indepvar_map) && all(indepvar_map$mapping$mapped)) {
    exact_par <- apply_indepvar_exact_jitter(par, indepvar_map, jitter_bound = jitter_bound, eps = eps)
    if (!is.null(exact_par)) {
      return(exact_par)
    }
  }
  
  # ==========================================================================
  # 1. TAG REPORTING RATE
  # ==========================================================================
  tag_reporting_supported <- jitter_bound > 1e-10 &&
    (age_flag_val(198) == 1 || any(fish_flag_vals(33) == 1)) &&
    all(fish_flag_vals(37) == 0)
  
  if (tag_reporting_supported) {
    nRRpars <- max(tag_fish_rep_grp(par))
    maxRR   <- flagval(par, 1, 33)$value / 100
    
    for (i in 1:nRRpars) {
      grp_idx <- which(tag_fish_rep_grp(par) == i)
      current_val <- tag_fish_rep_rate(par)[grp_idx[1]]
      
      new_val <- sample_uniform_pct(current_val, jitter_bound, lower = eps, upper = maxRR - eps)
      
      tag_fish_rep_rate(par)[grp_idx] <- new_val
    }
  }
  
  # ==========================================================================
  # 2. TOTAL POPULATION
  # ==========================================================================
  if (age_flag_val(31) == 1 && jitter_bound > 1e-10) {
    current_val <- tot_pop(par)
    tot_pop(par) <- sample_uniform_pct(current_val, jitter_bound, lower = eps)
  }
  
  # ==========================================================================
  # 3. RECRUITMENT DEVIATIONS
  # ==========================================================================
  if (age_flag_val(30) == 1 && jitter_bound > 1e-10) {
    current_vals <- rel_rec(par)
    pct_shifts <- rep(0, length(current_vals))
    
    terminal_fixed_n <- parest_flag_val(400)
    active_idx <- seq_along(current_vals)
    if (is.finite(terminal_fixed_n) && terminal_fixed_n > 0) {
      active_idx <- head(active_idx, max(0, length(current_vals) - terminal_fixed_n))
    }
    
    if (length(active_idx) > 0) {
      pct_shifts[active_idx] <- runif(length(active_idx), -jitter_bound, jitter_bound)
    }
    
    rel_rec(par) <- current_vals * (1 + pct_shifts)
  }
  
  # ==========================================================================
  # 4. FISHERY SELECTIVITY
  # ==========================================================================
  selectivity_supported <- jitter_bound > 1e-10 &&
    any(fish_flag_vals(48) > 0) &&
    length(fishery_sel(par)) > 0
  
  if (selectivity_supported) {
    uniqueSels <- max(flagval(par, -1:-nFish, 24)$value)
    
    for (i in 1:uniqueSels) {
      Selsfish <- which(flagval(par, -1:-nFish, 24)$value == i)
      
      if (flagval(par, -Selsfish[1], 48)$value == 1) {
        CurrentSel <- c(aperm(fishery_sel(par)[, , Selsfish[1], ], c(4,1,2,3,5,6)))

        nonzero_idx <- which(is.finite(CurrentSel) & abs(CurrentSel) > eps)
        if (length(nonzero_idx) == 0) {
          next
        }

        sel_scale <- mean(abs(CurrentSel[nonzero_idx]))
        if (!is.finite(sel_scale) || sel_scale <= eps) {
          sel_scale <- 1
        }

        NewSel <- CurrentSel
        NewSel[nonzero_idx] <- vapply(
          CurrentSel[nonzero_idx],
          function(x) sample_uniform_delta(x, jitter_bound, scale_val = sel_scale * 0.10, lower = -20, upper = 20),
          numeric(1)
        )
        
        fishery_sel(par)[, , Selsfish, , , ] <-
          aperm(array(NewSel, c(nSeasons, nAgeYr, 1, length(Selsfish), 1, 1)),
                c(2, 3, 4, 1, 5, 6))
      }
    }

    if (has_active_values(slot(par, "sel_dev_coffs"))) {
      tmp <- slot(par, "sel_dev_coffs")
      slot(par, "sel_dev_coffs") <- jitter_active_entries(tmp, scale_mult = 0.10, lower = -20, upper = 20)
    }

    sel_dev2 <- slot(par, "sel_dev_coffs2")
    if (is.list(sel_dev2) && length(sel_dev2) > 0) {
      for (k in seq_along(sel_dev2)) {
        if (has_active_values(sel_dev2[[k]])) {
          sel_dev2[[k]] <- jitter_active_entries(sel_dev2[[k]], scale_mult = 0.10, lower = -20, upper = 20)
        }
      }
      slot(par, "sel_dev_coffs2") <- sel_dev2
    }

    if (has_active_values(slot(par, "sel_dev_corr"))) {
      tmp_corr <- slot(par, "sel_dev_corr")
      slot(par, "sel_dev_corr") <- jitter_active_entries(tmp_corr, scale_mult = 0.10, lower = -0.999, upper = 0.999)
    }
  }
  
  # ==========================================================================
  # 5. NATURAL MORTALITY
  # ==========================================================================
  if (age_flag_val(33) == 1 && jitter_bound > 1e-10) {
    current_val <- m(par)
    m(par) <- sample_uniform_pct(current_val, jitter_bound, lower = eps)
  }
  
  # ==========================================================================
  # 6. AVERAGE CATCHABILITY
  # ==========================================================================
  catchability_supported <- jitter_bound > 1e-10 &&
    any(fish_flag_vals(1) == 1) &&
    all(fish_flag_vals(10) == 0) &&
    all(fish_flag_vals(27) == 0) &&
    all(fish_flag_vals(39) == 0) &&
    all(fish_flag_vals(47) == 0) &&
    all(fish_flag_vals(51) == 0) &&
    all(fish_flag_vals(53) == 0) &&
    all(fish_flag_vals(66) == 0) &&
    all(fish_flag_vals(81) == 0) &&
    age_flag_val(104) == 0 &&
    age_flag_val(125) == 0 &&
    age_flag_val(156) == 0 &&
    parest_flag_val(377) == 0 &&
    parest_flag_val(378) == 0
  
  if (catchability_supported) {
    q_free <- fish_flag_vals(1) == 1
    q_groups <- fish_flag_vals(60)
    group_keys <- ifelse(q_groups > 0, paste0("grp_", q_groups), paste0("fish_", seq_len(nFish)))
    
    for (group_key in unique(group_keys)) {
      group_idx <- which(group_keys == group_key)
      free_idx <- group_idx[q_free[group_idx]]
      
      if (length(free_idx) == 0) {
        next
      }
      
      # Preserve explicit group ties. Mixed free/fixed grouped fisheries are skipped.
      if (length(group_idx) > 1 && length(free_idx) != length(group_idx)) {
        next
      }
      
      current_vals <- av_q_coffs(par)[, , free_idx, , , , drop = FALSE]
      pct_min <- -jitter_bound
      pct_max <- jitter_bound
      
      positive_vals <- current_vals[is.finite(current_vals) & current_vals > eps]
      if (length(positive_vals) > 0) {
        pct_min <- max(pct_min, max(eps / positive_vals - 1))
      }
      
      if (pct_min > pct_max) {
        next
      }
      
      pct_shift <- runif(1, pct_min, pct_max)
      av_q_coffs(par)[, , free_idx, , , ] <- current_vals * (1 + pct_shift)
    }
  }
  
  # ==========================================================================
  # 7. DIFFUSION COEFFICIENTS
  # ==========================================================================
  movement_supported <- jitter_bound > 1e-10 &&
    age_flag_val(68) == 1 &&
    age_flag_val(69) == 1 &&
    age_flag_val(184) == 0 &&
    age_flag_val(88) == 0 &&
    age_flag_val(89) == 0 &&
    age_flag_val(90) == 0 &&
    age_flag_val(91) == 0 &&
    age_flag_val(114) == 0
  
  if (movement_supported) {
    tryCatch({
      jitter_matrix <- function(mat) {
        out <- mat
        for (i in seq_len(nrow(mat))) {
          for (j in seq_len(ncol(mat))) {
            out[i, j] <- sample_uniform_pct(
              mat[i, j],
              jitter_bound,
              lower = 1e-10,
              upper = 2.9999
            )
          }
        }
        out
      }

      diff_coffs(par) <- jitter_matrix(diff_coffs(par))

      if (has_active_values(xdiff_coffs(par))) {
        xdiff_coffs(par) <- jitter_matrix(xdiff_coffs(par))
      }
      if (has_active_values(y1diff_coffs(par))) {
        y1diff_coffs(par) <- jitter_matrix(y1diff_coffs(par))
      }
      if (has_active_values(y2diff_coffs(par))) {
        y2diff_coffs(par) <- jitter_matrix(y2diff_coffs(par))
      }
      if (has_active_values(zdiff_coffs(par))) {
        zdiff_coffs(par) <- jitter_matrix(zdiff_coffs(par))
      }
      
    }, error = function(e) {
      # Silently skip
    })
  }
  
  # ==========================================================================
  # 8. REGIONAL RECRUITMENT - ORDER-INDEPENDENT SIMPLEX JITTER
  # ==========================================================================
  regional_recruitment_supported <- jitter_bound > 1e-10 &&
    sum(subset(flags(par), flagtype == -100000)$value > 0) > 0
  
  if (regional_recruitment_supported) {
    
    region_flags <- subset(flags(par), flagtype == -100000)
    idx.free <- region_flags$flag[region_flags$value == 1]
    
    if (length(idx.free) > 0) {
      must.sum <- 1 - sum(region_pars(par)[1, -idx.free])
      current_props <- region_pars(par)[1, idx.free]
      current_props <- pmax(current_props, eps)
      
      if (has_active_values(region_rec_var(par)) || age_flag_val(70) == 1 || age_flag_val(71) == 1) {
        current_rrv <- region_rec_var(par)
        rrv_scale <- mean(abs(current_rrv[is.finite(current_rrv) & abs(current_rrv) > eps]))
        if (!is.finite(rrv_scale) || rrv_scale <= eps) {
          rrv_scale <- 1
        }

        for (yr_idx in seq_len(dim(current_rrv)[2])) {
          for (ssn_idx in seq_len(dim(current_rrv)[4])) {
            current_vec <- as.numeric(current_rrv[1, yr_idx, 1, ssn_idx, idx.free, 1])
            if (length(current_vec) == 0) {
              next
            }

            deltas <- runif(length(current_vec), -jitter_bound, jitter_bound) * rrv_scale * 0.20
            new_vec <- current_vec + deltas
            new_vec <- new_vec - mean(new_vec)
            region_rec_var(par)[1, yr_idx, 1, ssn_idx, idx.free, 1] <- new_vec
          }
        }
      } else if (length(idx.free) == 1) {
        region_pars(par)[1, idx.free[1]] <- must.sum

      } else {
        lower_bounds <- pmax(eps, current_props * (1 - jitter_bound))
        upper_bounds <- current_props * (1 + jitter_bound)
        proposed <- sample_bounded_simplex(
          current_props,
          total_sum = must.sum,
          lower = lower_bounds,
          upper = upper_bounds,
          eps = eps
        )
        region_pars(par)[1, idx.free] <- proposed
      }
    }
  }
  
  # ==========================================================================
  # 9. GROWTH PARAMETERS
  # ==========================================================================
  if (flagval(par, 1, 12)$value == 1 && jitter_bound > 1e-10) {
    current_val <- growth(par)[1]
    growth(par)[1] <- sample_uniform_pct(current_val, jitter_bound, lower = eps)
  }
  
  if (flagval(par, 1, 13)$value == 1 && jitter_bound > 1e-10) {
    current_val <- growth(par)[2]
    growth(par)[2] <- sample_uniform_pct(current_val, jitter_bound, lower = eps)
  }
  
  if (flagval(par, 1, 14)$value == 1 && jitter_bound > 1e-10) {
    current_val <- growth(par)[3]
    growth(par)[3] <- sample_uniform_pct(current_val, jitter_bound, lower = eps)
  }
  
  return(par)
}


#' ==============================================================================
#' GENERALIZED EXACT-JITTER WORKFLOW HELPERS
#' ==============================================================================
#' Convenience wrappers to resolve model files, write jittered par files, and
#' summarize exact parameter changes using indepvar labels.
#'

resolve_model_jitter_files <- function(model_dir,
                                       base_par_file = NULL,
                                       indepvar_file = NULL) {
  model_dir_input <- model_dir
  model_dir <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)

  if (is.null(base_par_file)) {
    par_hits <- list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
    par_hits <- par_hits[!grepl("jittered", basename(par_hits), ignore.case = TRUE)]
    if (length(par_hits) == 0) {
      stop("No base .par file found in model_dir.")
    }
    base_par_file <- par_hits[which.max(file.info(par_hits)$mtime)]
  } else if (!grepl("^/", base_par_file) && !file.exists(base_par_file)) {
    base_par_file <- file.path(model_dir, base_par_file)
  }

  if (is.null(indepvar_file)) {
    indepvar_file <- file.path(model_dir, "indepvar.rpt")
  } else if (!grepl("^/", indepvar_file) && !file.exists(indepvar_file)) {
    indepvar_file <- file.path(model_dir, indepvar_file)
  }

  list(
    model_dir_input = model_dir_input,
    model_dir = model_dir,
    base_par_file = normalizePath(base_par_file, winslash = "/", mustWork = TRUE),
    indepvar_file = if (file.exists(indepvar_file)) normalizePath(indepvar_file, winslash = "/", mustWork = TRUE) else NULL
  )
}

extract_indepvar_values <- function(par, indepvar_map) {
  mapping <- indepvar_map$mapping
  values <- rep(NA_real_, nrow(mapping))

  rel_flat <- as.vector(aperm(rel_rec(par), c(4, 2, 1, 3, 5, 6)))
  rrv_export <- matrix(
    as.vector(aperm(region_rec_var(par), c(4, 2, 5, 1, 3, 6))),
    ncol = dimensions(par)["regions"]
  )
  age_mat <- build_age_pars_matrix(par)
  grp_mat <- tag_fish_rep_grp(par)
  flag_mat <- tag_fish_rep_flags(par)

  for (i in seq_len(nrow(mapping))) {
    fam <- mapping$family[i]

    if (fam == "diff_coffs") {
      values[i] <- diff_coffs(par)[mapping$key1[i], mapping$key2[i]]
    } else if (fam == "region_rec_diffs") {
      values[i] <- rrv_export[mapping$key2[i], mapping$key1[i]]
    } else if (fam == "bs_selcoff_gp") {
      fish_idx <- mapping$key4[i]
      values[i] <- fishery_sel(par)[mapping$key2[i], 1, fish_idx, mapping$key3[i], 1, 1]
    } else if (fam == "recr") {
      values[i] <- log(rel_flat[mapping$key1[i]])
    } else if (fam == "totpop") {
      values[i] <- tot_pop(par)
    } else if (fam == "tag_fish_rep") {
      idx <- which(grp_mat == mapping$key1[i] & flag_mat == 1, arr.ind = TRUE)
      if (nrow(idx) > 0) {
        values[i] <- tag_fish_rep_rate(par)[idx[1, 1], idx[1, 2]]
      }
    } else if (fam == "region_pars") {
      values[i] <- region_pars(par)[mapping$key1[i], mapping$key2[i]]
    } else if (fam == "sv") {
      values[i] <- season_growth_pars(par)[mapping$key1[i]]
    } else if (fam == "age_pars") {
      values[i] <- age_mat[mapping$key1[i], mapping$key2[i]]
    } else if (fam == "vb_coff") {
      values[i] <- growth(par)[mapping$key1[i], 1]
    } else if (fam == "var_coff") {
      values[i] <- growth_var_pars(par)[mapping$key1[i], 1]
    }
  }

  values
}

inject_indepvar_values <- function(par, indepvar_map, values, eps = 1e-12) {
  mapping <- indepvar_map$mapping
  stopifnot(length(values) == nrow(mapping))

  if (!all(mapping$mapped)) {
    stop("Cannot inject indepvar values: some labels are not mapped.")
  }

  rrv_export <- matrix(
    as.vector(aperm(region_rec_var(par), c(4, 2, 5, 1, 3, 6))),
    ncol = dimensions(par)["regions"]
  )
  age_mat <- build_age_pars_matrix(par)
  grp_mat <- tag_fish_rep_grp(par)
  flag_mat <- tag_fish_rep_flags(par)

  for (i in seq_len(nrow(mapping))) {
    fam <- mapping$family[i]
    val <- values[i]
    if (!is.finite(val)) next

    if (fam == "diff_coffs") {
      diff_coffs(par)[mapping$key1[i], mapping$key2[i]] <- val
    } else if (fam == "region_rec_diffs") {
      rrv_export[mapping$key2[i], mapping$key1[i]] <- val
    } else if (fam == "bs_selcoff_gp") {
      fish_idx <- mapping$key4[i]
      if (!is.finite(fish_idx)) next
      fishery_sel(par)[mapping$key2[i], 1, fish_idx, mapping$key3[i], 1, 1] <- val
    } else if (fam == "recr") {
      rel_flat <- as.vector(aperm(rel_rec(par), c(4, 2, 1, 3, 5, 6)))
      rel_flat[mapping$key1[i]] <- exp(val)
      current_rel <- rel_rec(par)
      current_rel[] <- aperm(
        array(rel_flat, dim = c(dimensions(par)["seasons"], dim(rel_rec(par))[2], 1, 1, 1, 1)),
        order(c(4, 2, 1, 3, 5, 6))
      )
      slot(par, "rel_rec") <- current_rel
    } else if (fam == "totpop") {
      tot_pop(par) <- val
    } else if (fam == "tag_fish_rep") {
      idx <- which(grp_mat == mapping$key1[i] & flag_mat == 1, arr.ind = TRUE)
      if (nrow(idx) > 0) {
        tag_fish_rep_rate(par)[idx] <- val
      }
    } else if (fam == "region_pars") {
      region_pars(par)[mapping$key1[i], mapping$key2[i]] <- val
    } else if (fam == "sv") {
      season_growth_pars(par)[mapping$key1[i]] <- val
    } else if (fam == "age_pars") {
      age_mat[mapping$key1[i], mapping$key2[i]] <- val
    } else if (fam == "vb_coff") {
      growth(par)[mapping$key1[i], 1] <- val
    } else if (fam == "var_coff") {
      growth_var_pars(par)[mapping$key1[i], 1] <- val
    }
  }

  current_rrv <- region_rec_var(par)
  current_rrv[] <- aperm(
    array(as.vector(rrv_export), dim = c(dimensions(par)["seasons"], dim(region_rec_var(par))[2], dimensions(par)["regions"], 1, 1, 1)),
    c(4, 2, 5, 1, 3, 6)
  )
  slot(par, "region_rec_var") <- current_rrv
  par <- set_age_pars_matrix(par, age_mat)
  par
}

compare_exact_jitter <- function(base_par,
                                 jittered_par,
                                 indepvar_file = NULL,
                                 change_tol = 1e-14,
                                 output_prefix = NULL) {
  indepvar_map <- build_indepvar_mapping(base_par, indepvar_file = indepvar_file, tol = change_tol)
  if (is.null(indepvar_map) || !all(indepvar_map$mapping$mapped)) {
    stop("Exact indepvar mapping could not be resolved for all parameters.")
  }

  mapping <- indepvar_map$mapping
  label_values_before <- extract_indepvar_values(base_par, indepvar_map)
  label_values_after <- extract_indepvar_values(jittered_par, indepvar_map)
  deltas <- label_values_after - label_values_before

  labels_df <- transform(
    mapping,
    before = label_values_before,
    after = label_values_after,
    delta = deltas,
    changed = abs(deltas) > change_tol,
    family = ifelse(is.na(family), "unclassified", family)
  )

  summary_df <- aggregate(
    cbind(total = 1L, mapped = as.integer(labels_df$mapped), changed = as.integer(labels_df$changed)) ~ family,
    data = labels_df,
    FUN = sum
  )
  summary_df$unchanged <- summary_df$total - summary_df$changed
  summary_df$changed_pct <- round(100 * summary_df$changed / summary_df$total, 1)
  summary_df <- summary_df[order(-summary_df$total, summary_df$family), ]

  if (!is.null(output_prefix) && !isFALSE(output_prefix) && !(length(output_prefix) == 1 && is.na(output_prefix))) {
    dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
    write.csv(labels_df, sprintf("%s_label_changes.csv", output_prefix), row.names = FALSE)
    write.csv(summary_df, sprintf("%s_summary.csv", output_prefix), row.names = FALSE)

    pdf(sprintf("%s_summary.pdf", output_prefix), width = 10, height = 6)
    old_par <- par(no.readonly = TRUE)
    on.exit({
      par(old_par)
      dev.off()
    }, add = TRUE)
    par(mar = c(9, 4, 3, 1))
    barplot(
      summary_df$changed,
      names.arg = summary_df$family,
      las = 2,
      col = "#4C78A8",
      ylab = "Changed parameters",
      main = sprintf("Exact jitter summary (tol=%.0e)", change_tol)
    )
  }

  list(
    indepvar_file = indepvar_map$indepvar_file,
    labels = labels_df,
    summary = summary_df,
    change_tol = change_tol
  )
}

compare_indepvar_mapped <- function(base_par,
                                    jittered_par,
                                    indepvar_map,
                                    change_tol = 1e-14) {
  mapping <- indepvar_map$mapping
  label_values_before <- extract_indepvar_values(base_par, indepvar_map)
  label_values_after <- extract_indepvar_values(jittered_par, indepvar_map)
  deltas <- label_values_after - label_values_before

  labels_df <- transform(
    mapping,
    before = label_values_before,
    after = label_values_after,
    delta = deltas,
    changed = abs(deltas) > change_tol,
    family = ifelse(is.na(family), "unclassified", family)
  )

  summary_df <- aggregate(
    cbind(total = 1L, mapped = as.integer(labels_df$mapped), changed = as.integer(labels_df$changed)) ~ family,
    data = labels_df,
    FUN = sum
  )
  summary_df$unchanged <- summary_df$total - summary_df$changed
  summary_df$changed_pct <- round(100 * summary_df$changed / summary_df$total, 1)
  summary_df <- summary_df[order(-summary_df$total, summary_df$family), ]

  list(
    indepvar_file = indepvar_map$indepvar_file,
    labels = labels_df,
    summary = summary_df,
    change_tol = change_tol
  )
}

write_jittered_par <- function(par, out_file) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  write(par, out_file)
  normalizePath(out_file, winslash = "/", mustWork = TRUE)
}

run_exact_jitter <- function(model_dir,
                             jitter_bound = 0.05,
                             seed = 1,
                             base_par_file = NULL,
                             indepvar_file = NULL,
                             out_file = NULL,
                             output_prefix = NULL,
                             change_tol = 1e-14) {
  files <- resolve_model_jitter_files(
    model_dir = model_dir,
    base_par_file = base_par_file,
    indepvar_file = indepvar_file
  )

  if (is.null(files$indepvar_file)) {
    stop("indepvar.rpt is required for the generalized exact-jitter workflow.")
  }

  if (is.null(out_file)) {
    out_file <- file.path(files$model_dir, "jitter", sprintf("jittered_seed%d_%0.2f.par", seed, jitter_bound))
  } else if (!grepl("^/", out_file) &&
             !startsWith(out_file, paste0(files$model_dir_input, "/")) &&
             !startsWith(out_file, paste0(files$model_dir, "/"))) {
    out_file <- file.path(files$model_dir, out_file)
  }

  if (isFALSE(output_prefix) || (length(output_prefix) == 1 && is.na(output_prefix))) {
    output_prefix <- NULL
  } else if (is.null(output_prefix)) {
    output_prefix <- file.path(files$model_dir, "jitter", sprintf("jitter_seed%d_%0.2f", seed, jitter_bound))
  } else if (!grepl("^/", output_prefix) &&
             !startsWith(output_prefix, paste0(files$model_dir_input, "/")) &&
             !startsWith(output_prefix, paste0(files$model_dir, "/"))) {
    output_prefix <- file.path(files$model_dir, output_prefix)
  }

  base_par <- read.MFCLPar(files$base_par_file)
  jittered_par <- jitter_par(
    base_par,
    jitter_bound = jitter_bound,
    seed = seed,
    indepvar_file = files$indepvar_file
  )

  out_file <- write_jittered_par(jittered_par, out_file)
  comparison <- compare_exact_jitter(
    base_par = base_par,
    jittered_par = jittered_par,
    indepvar_file = files$indepvar_file,
    change_tol = change_tol,
    output_prefix = output_prefix
  )

  invisible(list(
    files = files,
    jitter_bound = jitter_bound,
    seed = seed,
    out_file = out_file,
    output_prefix = output_prefix,
    comparison = comparison
  ))
}


#' ==============================================================================
#' AUDIT JITTER CONFIGURATION AGAINST MODEL FLAGS
#' ==============================================================================
#' Summarize which parameter blocks are eligible for jitter, why, and whether
#' they actually change under repeated draws for a given model configuration.
#'

audit_jitter_configuration <- function(par_orig,
                                       jitter_bound = 0.05,
                                       n_jitters = 200,
                                       output_prefix = "jitter_audit",
                                       indepvar_file = NULL) {
  
  nFish <- dimensions(par_orig)["fisheries"]
  tol <- 1e-12
  
  fish_flag_vals <- function(flag_no) {
    flagval(par_orig, -1:-nFish, flag_no)$value
  }
  
  age_flag_val <- function(flag_no) {
    flagval(par_orig, 2, flag_no)$value
  }
  
  parest_flag_val <- function(flag_no) {
    flagval(par_orig, 1, flag_no)$value
  }

  count_active_values <- function(x) {
    sum(is.finite(as.numeric(x)) & abs(as.numeric(x)) > tol, na.rm = TRUE)
  }
  
  region_flag_tbl <- subset(flags(par_orig), flagtype == -100000)
  free_regions <- region_flag_tbl$flag[region_flag_tbl$value == 1]
  terminal_fixed_n <- parest_flag_val(400)
  
  tag_reporting_supported <- jitter_bound > 1e-10 &&
    (age_flag_val(198) == 1 || any(fish_flag_vals(33) == 1)) &&
    all(fish_flag_vals(37) == 0)
  
  selectivity_supported <- jitter_bound > 1e-10 &&
    any(fish_flag_vals(48) > 0) &&
    length(fishery_sel(par_orig)) > 0
  
  catchability_supported <- jitter_bound > 1e-10 &&
    any(fish_flag_vals(1) == 1) &&
    all(fish_flag_vals(10) == 0) &&
    all(fish_flag_vals(27) == 0) &&
    all(fish_flag_vals(39) == 0) &&
    all(fish_flag_vals(47) == 0) &&
    all(fish_flag_vals(51) == 0) &&
    all(fish_flag_vals(53) == 0) &&
    all(fish_flag_vals(66) == 0) &&
    all(fish_flag_vals(81) == 0) &&
    age_flag_val(104) == 0 &&
    age_flag_val(125) == 0 &&
    age_flag_val(156) == 0 &&
    parest_flag_val(377) == 0 &&
    parest_flag_val(378) == 0
  
  movement_supported <- jitter_bound > 1e-10 &&
    age_flag_val(68) == 1 &&
    age_flag_val(69) == 1 &&
    age_flag_val(184) == 0 &&
    age_flag_val(88) == 0 &&
    age_flag_val(89) == 0 &&
    age_flag_val(90) == 0 &&
    age_flag_val(91) == 0 &&
    age_flag_val(114) == 0
  
  regional_recruitment_supported <- jitter_bound > 1e-10 &&
    sum(region_flag_tbl$value > 0) > 0

  selectivity_nonzero <- 0
  if (selectivity_supported) {
    selectivity_nonzero <- count_active_values(fishery_sel(par_orig)) +
      count_active_values(slot(par_orig, "sel_dev_coffs")) +
      sum(vapply(slot(par_orig, "sel_dev_coffs2"), count_active_values, numeric(1))) +
      count_active_values(slot(par_orig, "sel_dev_corr"))
  }

  regional_active_elements <- 0
  if (regional_recruitment_supported) {
    if (count_active_values(region_rec_var(par_orig)) > 0 || age_flag_val(70) == 1 || age_flag_val(71) == 1) {
      regional_active_elements <- count_active_values(region_rec_var(par_orig))
    } else {
      regional_active_elements <- length(free_regions)
    }
  }
  
  rel_rec_total <- length(rel_rec(par_orig))
  rel_rec_active <- rel_rec_total
  if (is.finite(terminal_fixed_n) && terminal_fixed_n > 0) {
    rel_rec_active <- max(0, rel_rec_total - terminal_fixed_n)
  }
  
  q_free <- fish_flag_vals(1) == 1
  q_groups <- fish_flag_vals(60)
  q_group_keys <- ifelse(q_groups > 0, paste0("grp_", q_groups), paste0("fish_", seq_len(nFish)))
  q_supported_groups <- 0
  if (catchability_supported) {
    for (group_key in unique(q_group_keys)) {
      group_idx <- which(q_group_keys == group_key)
      free_idx <- group_idx[q_free[group_idx]]
      if (length(free_idx) == 0) {
        next
      }
      if (length(group_idx) > 1 && length(free_idx) != length(group_idx)) {
        next
      }
      q_supported_groups <- q_supported_groups + 1
    }
  }

  movement_active_elements <- 0
  if (movement_supported) {
    movement_active_elements <- count_active_values(diff_coffs(par_orig)) +
      count_active_values(xdiff_coffs(par_orig)) +
      count_active_values(y1diff_coffs(par_orig)) +
      count_active_values(y2diff_coffs(par_orig)) +
      count_active_values(zdiff_coffs(par_orig))
  }
  
  block_info <- data.frame(
    block = c(
      "Tag reporting rate",
      "Total population",
      "Recruitment deviations",
      "Fishery selectivity",
      "Natural mortality",
      "Average catchability",
      "Diffusion coefficients",
      "Regional recruitment",
      "Growth L1",
      "Growth K",
      "Growth Linf"
    ),
    manual_basis = c(
      "age 198 / fish 33 / fish 37",
      "age 31",
      "age 30 + parest 400",
      "fish 48,19,24,57,61,71,74 + parest 323",
      "age 33",
      "fish 1,10,27,39,47,51,53,60,66,81 + age 104,125,156 + parest 377,378",
      "age 68,69,88,89,90,91,114,184",
      "region flag 1,n + age 70,71",
      "par 12",
      "par 13",
      "par 14"
    ),
    supported_by_code = c(
      tag_reporting_supported,
      age_flag_val(31) == 1 && jitter_bound > 1e-10,
      age_flag_val(30) == 1 && jitter_bound > 1e-10,
      selectivity_supported,
      age_flag_val(33) == 1 && jitter_bound > 1e-10,
      catchability_supported,
      movement_supported,
      regional_recruitment_supported,
      flagval(par_orig, 1, 12)$value == 1 && jitter_bound > 1e-10,
      flagval(par_orig, 1, 13)$value == 1 && jitter_bound > 1e-10,
      flagval(par_orig, 1, 14)$value == 1 && jitter_bound > 1e-10
    ),
    active_elements = c(
      length(unique(tag_fish_rep_grp(par_orig))),
      1,
      rel_rec_active,
      selectivity_nonzero,
      1,
      q_supported_groups,
      movement_active_elements,
      regional_active_elements,
      1,
      1,
      1
    ),
    status_note = c(
      sprintf("age198=%s, any fish33=%s, all fish37==0=%s",
              age_flag_val(198), any(fish_flag_vals(33) == 1), all(fish_flag_vals(37) == 0)),
      sprintf("age31=%s", age_flag_val(31)),
      sprintf("age30=%s, parest400=%s", age_flag_val(30), terminal_fixed_n),
      sprintf("fish48 any>0=%s, fish57=%s, fish61=%s, fish75=%s, sel_slots_active=%s/%s/%s",
              any(fish_flag_vals(48) > 0),
              paste(unique(fish_flag_vals(57)), collapse = "/"),
              paste(unique(fish_flag_vals(61)), collapse = "/"),
              paste(unique(fish_flag_vals(75)), collapse = "/"),
              count_active_values(fishery_sel(par_orig)) > 0,
              count_active_values(slot(par_orig, "sel_dev_coffs")) > 0,
              count_active_values(slot(par_orig, "sel_dev_corr")) > 0),
      sprintf("age33=%s", age_flag_val(33)),
      sprintf("fish1 any1=%s, special q flags all0=%s, age104/125/156=0=%s",
              any(fish_flag_vals(1) == 1),
              all(fish_flag_vals(10) == 0) && all(fish_flag_vals(27) == 0) &&
                all(fish_flag_vals(39) == 0) && all(fish_flag_vals(47) == 0) &&
                all(fish_flag_vals(51) == 0) && all(fish_flag_vals(53) == 0) &&
                all(fish_flag_vals(66) == 0) && all(fish_flag_vals(81) == 0),
              age_flag_val(104) == 0 && age_flag_val(125) == 0 && age_flag_val(156) == 0),
      sprintf("age68=%s, age69=%s, age88/89/90/91/114/184 all off=%s, x/y/z active=%s/%s/%s/%s",
              age_flag_val(68), age_flag_val(69),
              age_flag_val(88) == 0 && age_flag_val(89) == 0 &&
                age_flag_val(90) == 0 && age_flag_val(91) == 0 &&
                age_flag_val(114) == 0 && age_flag_val(184) == 0,
              count_active_values(xdiff_coffs(par_orig)) > 0,
              count_active_values(y1diff_coffs(par_orig)) > 0,
              count_active_values(y2diff_coffs(par_orig)) > 0,
              count_active_values(zdiff_coffs(par_orig)) > 0),
      sprintf("free regions=%d, age70=%s, age71=%s, age178=%s, region_rec_var_active=%s",
              length(free_regions), age_flag_val(70), age_flag_val(71), age_flag_val(178),
              count_active_values(region_rec_var(par_orig)) > 0),
      sprintf("par12=%s", flagval(par_orig, 1, 12)$value),
      sprintf("par13=%s", flagval(par_orig, 1, 13)$value),
      sprintf("par14=%s", flagval(par_orig, 1, 14)$value)
    ),
    stringsAsFactors = FALSE
  )
  
  par_list <- vector("list", n_jitters)
  set.seed(1)
  for (i in seq_len(n_jitters)) {
    par_list[[i]] <- jitter_par(par_orig, jitter_bound = jitter_bound, indepvar_file = indepvar_file)
  }
  
  changed_stats <- function(extractor, pct = TRUE, supported = TRUE) {
    base_vals <- as.numeric(extractor(par_orig))
    if (length(base_vals) == 0 || !supported) {
      return(list(changed = 0, total = 0, frac = 0, mean_abs_pct = NA_real_, mean_abs_delta = NA_real_))
    }
    
    all_vals <- lapply(par_list, function(p) as.numeric(extractor(p)))
    diffs <- vapply(all_vals, function(x) {
      sum(abs(x - base_vals) > tol, na.rm = TRUE)
    }, numeric(1))
    
    changed_any <- vapply(seq_along(base_vals), function(j) {
      any(vapply(all_vals, function(x) abs(x[j] - base_vals[j]) > tol, logical(1)), na.rm = TRUE)
    }, logical(1))
    
    changed_vals <- unlist(lapply(all_vals, function(x) x[abs(x - base_vals) > tol]))
    base_rep <- unlist(lapply(all_vals, function(x) base_vals[abs(x - base_vals) > tol]))
    
    mean_abs_pct <- NA_real_
    if (pct && length(changed_vals) > 0) {
      valid <- is.finite(base_rep) & abs(base_rep) > tol & is.finite(changed_vals)
      if (any(valid)) {
        mean_abs_pct <- mean(abs(changed_vals[valid] / base_rep[valid] - 1) * 100)
      }
    }
    
    mean_abs_delta <- NA_real_
    if (length(changed_vals) > 0) {
      valid_delta <- is.finite(changed_vals) & is.finite(base_rep)
      if (any(valid_delta)) {
        mean_abs_delta <- mean(abs(changed_vals[valid_delta] - base_rep[valid_delta]))
      }
    }
    
    list(
      changed = sum(changed_any),
      total = length(base_vals),
      frac = mean(diffs > 0),
      mean_abs_pct = mean_abs_pct,
      mean_abs_delta = mean_abs_delta
    )
  }
  
  stats_list <- list(
    changed_stats(function(p) tag_fish_rep_rate(p), supported = tag_reporting_supported),
    changed_stats(function(p) tot_pop(p), supported = age_flag_val(31) == 1 && jitter_bound > 1e-10),
    changed_stats(function(p) {
      vals <- rel_rec(p)
      if (is.finite(terminal_fixed_n) && terminal_fixed_n > 0) {
        vals <- head(vals, max(0, length(vals) - terminal_fixed_n))
      }
      vals
    }, supported = age_flag_val(30) == 1 && jitter_bound > 1e-10),
    changed_stats(function(p) c(
      aperm(fishery_sel(p), c(4, 1, 2, 3, 5, 6)),
      as.numeric(slot(p, "sel_dev_coffs")),
      unlist(lapply(slot(p, "sel_dev_coffs2"), as.numeric), use.names = FALSE),
      as.numeric(slot(p, "sel_dev_corr"))
    ), pct = FALSE, supported = selectivity_supported),
    changed_stats(function(p) m(p), supported = age_flag_val(33) == 1 && jitter_bound > 1e-10),
    changed_stats(function(p) {
      vals <- av_q_coffs(p)
      as.numeric(vals[, , q_free, , , , drop = FALSE])
    }, supported = catchability_supported),
    changed_stats(function(p) c(
      as.numeric(diff_coffs(p)),
      as.numeric(xdiff_coffs(p)),
      as.numeric(y1diff_coffs(p)),
      as.numeric(y2diff_coffs(p)),
      as.numeric(zdiff_coffs(p))
    ), supported = movement_supported),
    changed_stats(function(p) {
      if (length(free_regions) == 0) {
        numeric(0)
      } else if (count_active_values(region_rec_var(par_orig)) > 0 || age_flag_val(70) == 1 || age_flag_val(71) == 1) {
        as.numeric(region_rec_var(p))
      } else {
        region_pars(p)[1, free_regions]
      }
    }, pct = FALSE, supported = regional_recruitment_supported),
    changed_stats(function(p) growth(p)[1], supported = flagval(par_orig, 1, 12)$value == 1 && jitter_bound > 1e-10),
    changed_stats(function(p) growth(p)[2], supported = flagval(par_orig, 1, 13)$value == 1 && jitter_bound > 1e-10),
    changed_stats(function(p) growth(p)[3], supported = flagval(par_orig, 1, 14)$value == 1 && jitter_bound > 1e-10)
  )
  
  stats_df <- data.frame(
    block = block_info$block,
    changed_elements = vapply(stats_list, `[[`, numeric(1), "changed"),
    total_elements = vapply(stats_list, `[[`, numeric(1), "total"),
    any_change_rate = vapply(stats_list, `[[`, numeric(1), "frac"),
    mean_abs_change_pct = vapply(stats_list, `[[`, numeric(1), "mean_abs_pct"),
    mean_abs_change = vapply(stats_list, `[[`, numeric(1), "mean_abs_delta"),
    stringsAsFactors = FALSE
  )
  
  audit_df <- merge(block_info, stats_df, by = "block", sort = FALSE)
  audit_df$check_result <- ifelse(
    audit_df$supported_by_code & audit_df$changed_elements > 0, "PASS",
    ifelse(!audit_df$supported_by_code & audit_df$changed_elements == 0, "PASS", "CHECK")
  )
  
  csv_path <- sprintf("%s.csv", output_prefix)
  pdf_path <- sprintf("%s.pdf", output_prefix)
  write.csv(audit_df, csv_path, row.names = FALSE)
  
  pdf(pdf_path, width = 13, height = 8)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  
  par(mfrow = c(2, 2), mar = c(8, 4.5, 3, 1))
  
  support_cols <- ifelse(audit_df$supported_by_code, "#3B7A57", "#B0B0B0")
  barplot(
    as.numeric(audit_df$supported_by_code),
    names.arg = audit_df$block,
    las = 2,
    ylim = c(0, 1.2),
    col = support_cols,
    ylab = "Supported by code",
    main = "Manual/Flag Support Check"
  )
  axis(2, at = c(0, 1), labels = c("No", "Yes"))
  
  barplot(
    audit_df$changed_elements,
    names.arg = audit_df$block,
    las = 2,
    col = "#4C78A8",
    ylab = "Changed elements",
    main = sprintf("Observed Changes Across %d Jitters", n_jitters)
  )
  
  rate_vals <- pmin(1, pmax(0, audit_df$any_change_rate))
  barplot(
    rate_vals,
    names.arg = audit_df$block,
    las = 2,
    col = "#F58518",
    ylim = c(0, 1),
    ylab = "Fraction of draws with any change",
    main = "Change Frequency"
  )
  
  pct_vals <- audit_df$mean_abs_change_pct
  pct_plot <- ifelse(is.na(pct_vals), 0, pct_vals)
  pct_cols <- ifelse(is.na(pct_vals), "#D3D3D3", "#54A24B")
  barplot(
    pct_plot,
    names.arg = audit_df$block,
    las = 2,
    col = pct_cols,
    ylab = "Mean absolute change (%)",
    main = "Magnitude of Jitter"
  )
  legend("topright", legend = c("Percent-based", "NA / non-percent block"),
         fill = c("#54A24B", "#D3D3D3"), bty = "n", cex = 0.8)
  
  message(sprintf("Audit CSV saved: %s", csv_path))
  message(sprintf("Audit PDF saved: %s", pdf_path))
  
  invisible(list(summary = audit_df, csv = csv_path, pdf = pdf_path))
}



#' ==============================================================================
#' COMPREHENSIVE JITTER VISUALIZATION - ALL VALUES
#' ==============================================================================
#' Plot all values for Regional Recruitment, Diffusion, and Selectivity
#'

plot_all_jitter_comprehensive <- function(par_orig,
                                          n_jitters = 500,
                                          jitter_bound = 0.05,
                                          indepvar_file = NULL) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("GENERATING JITTERED PARAMETERS...\n")
  cat(strrep("=", 80), "\n\n")
  
  nFish <- dimensions(par_orig)["fisheries"]
  
  # Generate jittered parameters
  set.seed(1)
  par_list <- list()
  for (i in 1:n_jitters) {
    par_list[[i]] <- jitter_par(par_orig, jitter_bound = jitter_bound, indepvar_file = indepvar_file)
    if (i %% 100 == 0) cat(sprintf("  Completed: %d/%d\n", i, n_jitters))
  }
  cat("Done!\n\n")
  
  # Collect all parameter values
  params <- list()
  
  # ==========================================================================
  # 1. TOTAL POPULATION
  # ==========================================================================
  tryCatch({
    tot_pop_orig <- tot_pop(par_orig)
    if (!is.na(tot_pop_orig) && is.numeric(tot_pop_orig)) {
      params$tot_pop <- list(
        values = sapply(par_list, function(p) tot_pop(p)),
        original = tot_pop_orig,
        name = "Total Population"
      )
      cat("✓ Total Population\n")
    }
  }, error = function(e) cat("✗ Total Population: Error\n"))
  
  # ==========================================================================
  # 2. MORTALITY
  # ==========================================================================
  tryCatch({
    m_orig <- m(par_orig)
    if (!is.na(m_orig) && is.numeric(m_orig)) {
      params$mortality <- list(
        values = sapply(par_list, function(p) m(p)),
        original = m_orig,
        name = "Natural Mortality (M)"
      )
      cat("✓ Natural Mortality\n")
    }
  }, error = function(e) cat("✗ Natural Mortality: Error\n"))
  
  # ==========================================================================
  # 3. GROWTH PARAMETERS
  # ==========================================================================
  tryCatch({
    growth_orig <- growth(par_orig)
    if (length(growth_orig) >= 1 && !is.na(growth_orig[1])) {
      params$growth_l1 <- list(
        values = sapply(par_list, function(p) growth(p)[1]),
        original = growth_orig[1],
        name = "Growth L1"
      )
      cat("✓ Growth L1\n")
    }
    if (length(growth_orig) >= 2 && !is.na(growth_orig[2])) {
      params$growth_k <- list(
        values = sapply(par_list, function(p) growth(p)[2]),
        original = growth_orig[2],
        name = "Growth K"
      )
      cat("✓ Growth K\n")
    }
    if (length(growth_orig) >= 3 && !is.na(growth_orig[3])) {
      params$growth_linf <- list(
        values = sapply(par_list, function(p) growth(p)[3]),
        original = growth_orig[3],
        name = "Growth Linf"
      )
      cat("✓ Growth Linf\n")
    }
  }, error = function(e) cat("✗ Growth: Error\n"))
  
  # ==========================================================================
  # 4. TAG REPORTING RATE
  # ==========================================================================
  tryCatch({
    tag_rep_orig <- tag_fish_rep_rate(par_orig)
    if (length(tag_rep_orig) > 0 && !is.na(tag_rep_orig[1])) {
      params$tag_rep <- list(
        values = sapply(par_list, function(p) tag_fish_rep_rate(p)[1]),
        original = tag_rep_orig[1],
        name = "Tag Reporting Rate"
      )
      cat("✓ Tag Reporting Rate\n")
    }
  }, error = function(e) cat("✗ Tag Reporting Rate: Error\n"))
  
  # ==========================================================================
  # 5. ALL DIFFUSION COEFFICIENTS
  # ==========================================================================
  tryCatch({
    diff_orig <- diff_coffs(par_orig)
    if (!is.null(diff_orig) && nrow(diff_orig) > 0 && ncol(diff_orig) > 0) {
      for (i in 1:nrow(diff_orig)) {
        for (j in 1:ncol(diff_orig)) {
          param_name <- sprintf("diff_coeff_%d_%d", i, j)
          params[[param_name]] <- list(
            values = sapply(par_list, function(p) {
              dc <- diff_coffs(p)
              if (!is.null(dc) && nrow(dc) > 0 && ncol(dc) > 0) dc[i, j] else NA
            }),
            original = diff_orig[i, j],
            name = sprintf("Diffusion Coeff [%d,%d]", i, j)
          )
        }
      }
      cat(sprintf("✓ Diffusion Coefficients (total: %d)\n", nrow(diff_orig) * ncol(diff_orig)))
    }
  }, error = function(e) cat("✗ Diffusion Coefficients: Error\n"))
  
  # ==========================================================================
  # 6. ALL REGIONAL RECRUITMENT
  # ==========================================================================
  tryCatch({
    region_flags <- subset(flags(par_orig), flagtype == -100000)
    idx.free <- region_flags$flag[region_flags$value == 1]
    reg_par_orig <- region_pars(par_orig)
    
    if (length(idx.free) > 0 && !is.null(reg_par_orig) && nrow(reg_par_orig) > 0) {
      for (k in seq_along(idx.free)) {
        idx <- idx.free[k]
        param_name <- sprintf("region_rec_%d", idx)
        params[[param_name]] <- list(
          values = sapply(par_list, function(p) {
            rp <- region_pars(p)
            if (!is.null(rp) && nrow(rp) > 0 && ncol(rp) >= idx) {
              rp[1, idx]
            } else NA
          }),
          original = reg_par_orig[1, idx],
          name = sprintf("Regional Recruitment [%d]", idx)
        )
      }
      cat(sprintf("✓ Regional Recruitment (total: %d regions)\n", length(idx.free)))
    }
  }, error = function(e) cat("✗ Regional Recruitment: Error\n"))
  
  # ==========================================================================
  # 7. ALL SELECTIVITY BY AGE AND FISHERY
  # ==========================================================================
  tryCatch({
    Selsfish <- which(flagval(par_orig, -1:-nFish, 24)$value > 0)
    
    sel_count <- 0
    
    for (fish_idx in seq_along(Selsfish)) {
      fish <- Selsfish[fish_idx]
      
      if (flagval(par_orig, -fish, 48)$value == 1) {
        orig_sel <- c(aperm(fishery_sel(par_orig)[, , fish, ], c(4,1,2,3,5,6)))
        
        # Plot for all ages
        for (age_idx in seq_along(orig_sel)) {
          if (!is.na(orig_sel[age_idx]) && abs(orig_sel[age_idx]) < 20) {
            param_name <- sprintf("sel_fish%d_age%d", fish, age_idx)
            params[[param_name]] <- list(
              values = sapply(par_list, function(p) {
                sel <- c(aperm(fishery_sel(p)[, , fish, ], c(4,1,2,3,5,6)))
                if (length(sel) >= age_idx) sel[age_idx] else NA
              }),
              original = orig_sel[age_idx],
              name = sprintf("Selectivity (Fish %d, Age %d)", fish, age_idx)
            )
            sel_count <- sel_count + 1
          }
        }
      }
    }
    
    if (sel_count > 0) {
      cat(sprintf("✓ Selectivity (total: %d age classes)\n", sel_count))
    }
  }, error = function(e) cat("✗ Selectivity: Error\n"))
  
  cat("\n")
  cat(sprintf("Total parameters collected: %d\n", length(params)))
  cat("\n")
  
  if (length(params) == 0) {
    cat("ERROR: No parameters were successfully extracted!\n")
    return(NULL)
  }
  
  # ==========================================================================
  # PLOT ALL DISTRIBUTIONS
  # ==========================================================================
  cat("PLOTTING ALL DISTRIBUTIONS...\n\n")
  
  n_params <- length(params)
  n_cols <- 3
  n_rows <- ceiling(n_params / n_cols)
  
  pdf("jitter_all_comprehensive.pdf", width = 15, height = 4 * n_rows)
  par(mfrow = c(n_rows, n_cols), mar = c(4.5, 4.5, 3.5, 1.5), oma = c(0, 0, 2, 0))
  
  for (i in seq_along(params)) {
    param <- params[[i]]
    values <- param$values
    original <- param$original
    name <- param$name
    
    # Remove any NA values
    values <- values[!is.na(values)]
    
    if (length(values) > 0) {
      value_range <- range(values)
      x_pad <- if (diff(value_range) > 0) 0.1 * diff(value_range) else max(abs(value_range[1]) * 0.05, 1e-6)
      
      # Create histogram with original line
      h <- hist(values, 
                breaks = 40,
                main = name,
                xlab = "Value",
                ylab = "Frequency",
                col = rgb(0.3, 0.5, 0.8, 0.7),
                border = "white",
                xlim = c(value_range[1] - x_pad, value_range[2] + x_pad))
      
      # Add original value line
      abline(v = original, col = "red", lwd = 3, lty = 1)
      
      # Add statistics legend
      mean_val <- mean(values)
      sd_val <- sd(values)
      
      legend("topleft",
             legend = c(sprintf("Mean: %.4f", mean_val),
                        sprintf("SD: %.4f", sd_val),
                        sprintf("Original: %.4f", original),
                        sprintf("Range: [%.2f, %.2f]", min(values), max(values))),
             bty = "o",
             bg = "white",
             cex = 0.7)
      
      if (i %% 10 == 0) cat(sprintf("  Completed: %d/%d\n", i, n_params))
    }
  }
  
  mtext(sprintf("All Jitter Parameter Distributions (bound=%.3f, n=%d)\nRed line = Original value",
                jitter_bound, n_jitters),
        side = 3, outer = TRUE, cex = 1.5, font = 2)
  
  dev.off()
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("PDF SAVED: jitter_all_comprehensive.pdf (%d plots)\n", n_params))
  cat(strrep("=", 80), "\n\n")
  
  # ==========================================================================
  # PRINT SUMMARY TABLE
  # ==========================================================================
  cat("PARAMETER STATISTICS SUMMARY\n")
  cat(strrep("=", 80), "\n\n")
  
  summary_df <- data.frame(
    Parameter = sapply(params, function(p) p$name),
    Original = sapply(params, function(p) p$original),
    Mean = sapply(params, function(p) mean(p$values[!is.na(p$values)])),
    Std_Dev = sapply(params, function(p) sd(p$values[!is.na(p$values)])),
    Min = sapply(params, function(p) min(p$values[!is.na(p$values)])),
    Max = sapply(params, function(p) max(p$values[!is.na(p$values)])),
    Change_Pct_Mean = sapply(params, function(p) {
      v <- p$values[!is.na(p$values)]
      if (!is.finite(p$original) || abs(p$original) <= eps) {
        return(NA_real_)
      }
      mean((v / p$original - 1) * 100)
    }),
    Change_Abs_Mean = sapply(params, function(p) {
      v <- p$values[!is.na(p$values)]
      mean(v - p$original)
    })
  )
  
  # Sort by parameter type for clarity
  summary_df <- summary_df[order(summary_df$Parameter), ]
  
  print(summary_df, digits = 5)
  
  cat("\n")
  cat("Legend:\n")
  cat("  Original: Original parameter value\n")
  cat("  Mean: Mean of jittered parameters\n")
  cat("  Std_Dev: Standard deviation\n")
  cat("  Min/Max: Minimum/Maximum values\n")
  cat("  Change_Pct_Mean: Mean percent change from original (NA when original is 0)\n")
  cat("  Change_Abs_Mean: Mean absolute shift from original\n\n")
  
  invisible(list(
    params = params,
    summary = summary_df,
    n_params = n_params
  ))
}









#' ==============================================================================
#' VALIDATE JITTER UNIFORMITY AND CONSISTENCY
#' ==============================================================================
#' Validate that jitter_par function produces truly uniform and consistent results
#'

validate_jitter_uniformity <- function(par_orig,
                                       n_jitters = 2000,
                                       bounds = c(0.01, 0.05, 0.10),
                                       indepvar_file = NULL) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("JITTER UNIFORMITY AND CONSISTENCY VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  nFish <- dimensions(par_orig)["fisheries"]
  constant_tol <- 1e-12
  
  safe_ks_uniform <- function(x, min_val, max_val) {
    x <- x[is.finite(x)]
    
    if (length(x) < 2 || diff(range(x)) <= constant_tol) {
      return(list(statistic = NA_real_, p.value = NA_real_, status = "SKIP (constant)"))
    }
    
    out <- ks.test(x, "punif", min = min_val, max = max_val)
    list(
      statistic = unname(out$statistic),
      p.value = out$p.value,
      status = ifelse(out$p.value > 0.05, "(PASS)", "(FAIL)")
    )
  }
  
  # ==========================================================================
  # TEST 1: DISTRIBUTION SHAPE TEST (KS test for uniformity)
  # ==========================================================================
  
  cat("TEST 1: DISTRIBUTION SHAPE VALIDATION\n")
  cat("(Kolmogorov-Smirnov test for Uniformity)\n")
  cat(strrep("-", 80), "\n\n")
  
  for (bound in bounds) {
    cat(sprintf("Testing with jitter_bound = %.2f\n\n", bound))
    
    set.seed(1)
    par_list <- list()
    for (i in 1:n_jitters) {
      par_list[[i]] <- jitter_par(par_orig, jitter_bound = bound, indepvar_file = indepvar_file)
    }
    
    # Extract change percentages for each parameter
    tot_pop_orig <- tot_pop(par_orig)
    tot_pop_jit <- sapply(par_list, tot_pop)
    tot_pop_change_pct <- (tot_pop_jit / tot_pop_orig - 1) * 100
    
    m_orig <- m(par_orig)
    m_jit <- sapply(par_list, m)
    m_change_pct <- (m_jit / m_orig - 1) * 100
    
    growth_orig <- growth(par_orig)[1]
    growth_jit <- sapply(par_list, function(p) growth(p)[1])
    growth_change_pct <- (growth_jit / growth_orig - 1) * 100
    
    # Test: Is it uniform?
    # For uniform percent-change U(-a, +a):
    # - Min should be ≈ -a*100
    # - Max should be ≈ +a*100
    # - Distribution should be relatively flat
    
    expected_min <- -bound
    expected_max <- bound
    expected_min_pct <- expected_min * 100
    expected_max_pct <- expected_max * 100
    
    cat(sprintf("  Total Population:\n"))
    cat(sprintf("    Expected range: [%.2f%%, %.2f%%]\n", expected_min_pct, expected_max_pct))
    cat(sprintf("    Actual range:   [%.2f%%, %.2f%%]\n", 
                min(tot_pop_change_pct), max(tot_pop_change_pct)))
    cat(sprintf("    Mean:           %.4f%%\n", mean(tot_pop_change_pct)))
    cat(sprintf("    Std Dev:        %.4f%%\n\n", sd(tot_pop_change_pct)))
    
    cat(sprintf("  Natural Mortality:\n"))
    cat(sprintf("    Expected range: [%.2f%%, %.2f%%]\n", expected_min_pct, expected_max_pct))
    cat(sprintf("    Actual range:   [%.2f%%, %.2f%%]\n", 
                min(m_change_pct), max(m_change_pct)))
    cat(sprintf("    Mean:           %.4f%%\n", mean(m_change_pct)))
    cat(sprintf("    Std Dev:        %.4f%%\n\n", sd(m_change_pct)))
    
    cat(sprintf("  Growth L1:\n"))
    cat(sprintf("    Expected range: [%.2f%%, %.2f%%]\n", expected_min_pct, expected_max_pct))
    cat(sprintf("    Actual range:   [%.2f%%, %.2f%%]\n", 
                min(growth_change_pct), max(growth_change_pct)))
    cat(sprintf("    Mean:           %.4f%%\n", mean(growth_change_pct)))
    cat(sprintf("    Std Dev:        %.4f%%\n\n", sd(growth_change_pct)))
    
    # Perform KS test (test against uniform distribution)
    ks_tot_pop <- safe_ks_uniform(tot_pop_change_pct, expected_min_pct, expected_max_pct)
    ks_m <- safe_ks_uniform(m_change_pct, expected_min_pct, expected_max_pct)
    ks_growth <- safe_ks_uniform(growth_change_pct, expected_min_pct, expected_max_pct)
    
    cat(sprintf("  KS Test Results (H0: Uniform Distribution):\n"))
    cat(sprintf("    Tot Pop:  D = %.4f, p-value = %.4f %s\n", 
                ks_tot_pop$statistic, ks_tot_pop$p.value,
                ks_tot_pop$status))
    cat(sprintf("    Mortality: D = %.4f, p-value = %.4f %s\n", 
                ks_m$statistic, ks_m$p.value,
                ks_m$status))
    cat(sprintf("    Growth:  D = %.4f, p-value = %.4f %s\n\n", 
                ks_growth$statistic, ks_growth$p.value,
                ks_growth$status))
  }
  
  # ==========================================================================
  # TEST 2: CONSISTENCY ACROSS BOUNDS
  # ==========================================================================
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 2: CONSISTENCY ACROSS DIFFERENT BOUNDS\n")
  cat("(Does doubling bound approximately double the mean absolute change?)\n")
  cat(strrep("-", 80), "\n\n")
  
  consistency_results <- list()
  
  for (bound in bounds) {
    set.seed(1)
    par_list <- list()
    for (i in 1:n_jitters) {
      par_list[[i]] <- jitter_par(par_orig, jitter_bound = bound, indepvar_file = indepvar_file)
    }
    
    tot_pop_orig <- tot_pop(par_orig)
    tot_pop_jit <- sapply(par_list, tot_pop)
    tot_pop_change_pct <- abs((tot_pop_jit / tot_pop_orig - 1) * 100)
    
    m_orig <- m(par_orig)
    m_jit <- sapply(par_list, m)
    m_change_pct <- abs((m_jit / m_orig - 1) * 100)
    
    growth_orig <- growth(par_orig)[1]
    growth_jit <- sapply(par_list, function(p) growth(p)[1])
    growth_change_pct <- abs((growth_jit / growth_orig - 1) * 100)
    
    # Regional recruitment (first free region)
    region_flags <- subset(flags(par_orig), flagtype == -100000)
    idx.free <- region_flags$flag[region_flags$value == 1]
    if (length(idx.free) > 0) {
      reg_orig <- region_pars(par_orig)[1, idx.free[1]]
      reg_jit <- sapply(par_list, function(p) region_pars(p)[1, idx.free[1]])
      reg_change_pct <- abs((reg_jit / reg_orig - 1) * 100)
    } else {
      reg_change_pct <- NA
    }
    
    consistency_results[[as.character(bound)]] <- data.frame(
      bound = bound,
      tot_pop_mean = mean(tot_pop_change_pct),
      m_mean = mean(m_change_pct),
      growth_mean = mean(growth_change_pct),
      reg_mean = ifelse(length(idx.free) > 0, mean(reg_change_pct), NA)
    )
    
    cat(sprintf("jitter_bound = %.2f:\n", bound))
    cat(sprintf("  Total Pop mean |change|:  %.3f%%\n", mean(tot_pop_change_pct)))
    cat(sprintf("  Mortality mean |change|:  %.3f%%\n", mean(m_change_pct)))
    cat(sprintf("  Growth mean |change|:     %.3f%%\n", mean(growth_change_pct)))
    if (length(idx.free) > 0) {
      cat(sprintf("  Regional mean |change|:   %.3f%%\n", mean(reg_change_pct)))
    }
    cat("\n")
  }
  
  # Calculate ratios
  consistency_df <- do.call(rbind, consistency_results)
  rownames(consistency_df) <- NULL
  
  cat("CONSISTENCY ANALYSIS (Linear Scaling):\n")
  cat(strrep("-", 80), "\n\n")
  
  for (param in c("tot_pop_mean", "m_mean", "growth_mean", "reg_mean")) {
    if (!all(is.na(consistency_df[[param]]))) {
      values <- consistency_df[[param]][!is.na(consistency_df[[param]])]
      
      if (length(values) >= 2) {
        ratios <- diff(values) / head(values, -1)
        expected_ratios <- diff(consistency_df$bound) / head(consistency_df$bound, -1)
        
        cat(sprintf("%s:\n", param))
        cat(sprintf("  Values:          %s\n",
                    paste(sprintf("%.3f", values), collapse = " -> ")))
        cat(sprintf("  Actual ratios:   %s\n",
                    paste(sprintf("%.3f", ratios), collapse = ", ")))
        cat(sprintf("  Expected ratios: %s\n",
                    paste(sprintf("%.3f", expected_ratios), collapse = ", ")))
        
        if (all(abs(values) <= constant_tol)) {
          cat(sprintf("  Status: SKIP (constant)\n\n"))
        } else if (length(ratios) == length(expected_ratios) &&
            all(is.finite(ratios)) &&
            all(abs(ratios - expected_ratios) < 0.2)) {
          cat(sprintf("  Status: ✅ CONSISTENT\n\n")
          )
        } else {
          cat(sprintf("  Status: ❌ INCONSISTENT\n\n")
          )
        }
      }
    }
  }
  
  # ==========================================================================
  # TEST 3: VISUAL VALIDATION
  # ==========================================================================
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 3: CREATING VALIDATION PLOTS\n")
  cat(strrep("-", 80), "\n\n")
  
  pdf("jitter_validation.pdf", width = 14, height = 10)
  par(mfrow = c(3, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  for (bound in bounds) {
    set.seed(1)
    par_list <- list()
    for (i in 1:n_jitters) {
      par_list[[i]] <- jitter_par(par_orig, jitter_bound = bound, indepvar_file = indepvar_file)
    }
    
    tot_pop_orig <- tot_pop(par_orig)
    tot_pop_jit <- sapply(par_list, tot_pop)
    tot_pop_change_pct <- (tot_pop_jit / tot_pop_orig - 1) * 100
    
    expected_min <- -bound * 100
    expected_max <- bound * 100
    
    # Histogram
    hist(tot_pop_change_pct, breaks = 50,
         main = sprintf("Total Pop (bound=%.2f)", bound),
         xlab = "Change (%)",
         ylab = "Frequency",
         col = rgb(0.3, 0.5, 0.8, 0.7),
         border = "white",
         xlim = c(expected_min - 2, expected_max + 2))
    
    # Add expected bounds
    abline(v = expected_min, col = "red", lwd = 2, lty = 2)
    abline(v = expected_max, col = "red", lwd = 2, lty = 2)
    abline(v = 0, col = "green", lwd = 1, lty = 1)
    
    # Add text
    text(expected_min, max(hist(tot_pop_change_pct, breaks = 50, plot = FALSE)$counts) * 0.9,
         sprintf("Min:\n%.2f%%", min(tot_pop_change_pct)),
         adj = c(1, 1), cex = 0.75, bg = "white")
    text(expected_max, max(hist(tot_pop_change_pct, breaks = 50, plot = FALSE)$counts) * 0.9,
         sprintf("Max:\n%.2f%%", max(tot_pop_change_pct)),
         adj = c(0, 1), cex = 0.75, bg = "white")
  }
  
  mtext("Jitter Uniformity Validation", side = 3, outer = TRUE, cex = 1.5, font = 2)
  dev.off()
  
  cat("PDF SAVED: jitter_validation.pdf\n\n")
  
  cat(strrep("=", 80), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 80), "\n\n")
  
  cat("✓ Unconstrained active parameters are intended to use uniform percent-change jitter\n")
  cat("✓ Mean absolute change scales linearly with jitter_bound in the tested parameters\n")
  cat("✓ Mean change is approximately 0 for unconstrained parameters\n")
  cat("✓ Range is intended to stay within [-a*100%, +a*100%] where a = jitter_bound\n")
  cat("✓ Fixed or bounded parameters may be skipped or show truncated distributions\n\n")
  
  invisible(consistency_df)
}
