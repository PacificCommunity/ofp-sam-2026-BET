mod_bounds_ui <- function() {
      tabItem(
        tabName = "bounds",
        h2("Parameter Bound Hit Analysis", style = "color: #f39c12;"),
        
        # Overview table
        fluidRow(
          box(
            title = "Overview",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            DTOutput("bounds_overview")
          )
        ),
        
        # Detailed bound hits table
        fluidRow(
          box(
            title = "Detailed Bound Hits",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            collapsible = TRUE,
            selectInput("bound_model", "Select Model:", choices = NULL),
            DTOutput("bounds_type_summary"),
            DTOutput("bounds_detail"),
            downloadButton("download_bounds", "Download CSV", class = "btn-info")
          )
        )
      )
}

mod_bounds_server <- function(input, output, session, rv) {
    # TAB 3: BOUND HITS
    # ===========================================================================

    bound_var_type <- function(var_name) {
      var_name <- as.character(var_name)
      var_name <- trimws(var_name)
      var_name <- sub("\\(.*$", "", var_name)
      var_name <- sub("\\[.*$", "", var_name)
      ifelse(nzchar(var_name), var_name, "Unknown")
    }

    parse_prefix <- function(var_name) {
      v <- tolower(trimws(as.character(var_name)))
      p <- sub("\\(.*$", "", v)
      p <- sub(":.*$", "", p)
      trimws(p)
    }

    parse_var_indices <- function(var_name) {
      idx <- regmatches(var_name, gregexpr("[0-9]+", var_name))[[1]]
      if (length(idx) == 0) return(integer(0))
      suppressWarnings(as.integer(idx))
    }

    movement_pairs_from_matrix <- function(move_matrix) {
      if (is.null(move_matrix) || !is.matrix(move_matrix) || nrow(move_matrix) < 2) {
        return(matrix(integer(0), ncol = 2))
      }

      pairs <- vector("list", 0L)
      for (i in seq_len(nrow(move_matrix) - 1L)) {
        for (j in seq.int(i + 1L, ncol(move_matrix))) {
          value <- move_matrix[i, j]
          if (!is.na(value) && value != 0) {
            pairs[[length(pairs) + 1L]] <- c(i, j)
          }
        }
      }

      if (length(pairs) == 0) return(matrix(integer(0), ncol = 2))
      do.call(rbind, pairs)
    }

    directed_movement_pairs <- function(move_matrix) {
      undirected <- movement_pairs_from_matrix(move_matrix)
      if (nrow(undirected) == 0) return(matrix(integer(0), ncol = 2))

      out <- matrix(NA_integer_, nrow = nrow(undirected) * 2L, ncol = 2L)
      cursor <- 1L
      for (row_idx in seq_len(nrow(undirected))) {
        from <- undirected[row_idx, 1L]
        to <- undirected[row_idx, 2L]
        out[cursor, ] <- c(from, to)
        out[cursor + 1L, ] <- c(to, from)
        cursor <- cursor + 2L
      }
      out
    }

    load_model_move_matrix <- function(model_name) {
      model_dir <- tryCatch(file.path(input$model_dir, model_name), error = function(e) NULL)
      frq_candidates <- character(0)

      info_obj <- tryCatch(rv$Info_list[[model_name]], error = function(e) NULL)
      if (!is.null(info_obj) && !is.null(info_obj$base_dir)) {
        base_dir <- as.character(info_obj$base_dir[[1]])
        frq_file <- if (!is.null(info_obj$frq_file) && nzchar(as.character(info_obj$frq_file[[1]]))) {
          as.character(info_obj$frq_file[[1]])
        } else {
          NA_character_
        }

        base_candidates <- c(
          base_dir,
          file.path(getwd(), base_dir),
          normalizePath(file.path(getwd(), "..", "..", base_dir), winslash = "/", mustWork = FALSE)
        )
        base_candidates <- unique(base_candidates[nzchar(base_candidates)])

        for (base_candidate in base_candidates) {
          if (!dir.exists(base_candidate)) next
          if (!is.na(frq_file)) {
            frq_candidates <- c(frq_candidates, file.path(base_candidate, frq_file))
          }
          frq_candidates <- c(frq_candidates, list.files(base_candidate, pattern = "\\.frq$", full.names = TRUE))
        }
      }

      if (is.null(model_dir) || !dir.exists(model_dir)) {
        model_dir <- NULL
      }

      if (!is.null(model_dir)) {
        frq_candidates <- c(frq_candidates, list.files(model_dir, pattern = "\\.frq$", full.names = TRUE))
      }

      frq_candidates <- unique(frq_candidates[file.exists(frq_candidates)])
      if (length(frq_candidates) == 0) return(NULL)

      tryCatch(read.MFCLFrq(frq_candidates[[1]])@move_matrix, error = function(e) NULL)
    }

    load_model_par_object <- function(model_name) {
      par_obj <- tryCatch(rv$ParOut_list[[model_name]], error = function(e) NULL)
      if (!is.null(par_obj)) return(par_obj)

      model_dir <- tryCatch(file.path(input$model_dir, model_name), error = function(e) NULL)
      par_candidates <- character(0)
      if (!is.null(model_dir) && dir.exists(model_dir)) {
        par_candidates <- c(
          file.path(model_dir, "11.par"),
          list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
        )
      }

      info_obj <- tryCatch(rv$Info_list[[model_name]], error = function(e) NULL)
      if (!is.null(info_obj) && !is.null(info_obj$base_dir)) {
        base_dir <- as.character(info_obj$base_dir[[1]])
        par_file <- if (!is.null(info_obj$par_out) && nzchar(as.character(info_obj$par_out[[1]]))) {
          as.character(info_obj$par_out[[1]])
        } else if (!is.null(info_obj$par_in) && nzchar(as.character(info_obj$par_in[[1]]))) {
          as.character(info_obj$par_in[[1]])
        } else {
          "11.par"
        }

        base_candidates <- c(
          base_dir,
          file.path(getwd(), base_dir),
          normalizePath(file.path(getwd(), "..", "..", base_dir), winslash = "/", mustWork = FALSE)
        )
        base_candidates <- unique(base_candidates[nzchar(base_candidates)])
        for (base_candidate in base_candidates) {
          if (dir.exists(base_candidate)) {
            par_candidates <- c(par_candidates, file.path(base_candidate, par_file))
          }
        }
      }

      par_candidates <- unique(par_candidates[file.exists(par_candidates)])
      if (length(par_candidates) == 0) return(NULL)
      tryCatch(read.MFCLPar(par_candidates[[1]]), error = function(e) NULL)
    }

    load_model_fishery_map <- function(model_name) {
      info_obj <- tryCatch(rv$Info_list[[model_name]], error = function(e) NULL)
      base_dir <- if (!is.null(info_obj) && !is.null(info_obj$base_dir)) as.character(info_obj$base_dir[[1]]) else NA_character_

      candidates <- character(0)
      if (!is.na(base_dir) && nzchar(base_dir)) {
        candidates <- c(
          file.path(base_dir, "fishery_map.R"),
          file.path(getwd(), base_dir, "fishery_map.R"),
          normalizePath(file.path(getwd(), "..", "..", base_dir, "fishery_map.R"), winslash = "/", mustWork = FALSE)
        )
      }

      candidates <- unique(candidates[file.exists(candidates)])
      if (length(candidates) == 0) return(NULL)

      env <- new.env(parent = baseenv())
      tryCatch({
        sys.source(candidates[[1]], envir = env)
        if (exists("fishery_map", envir = env, inherits = FALSE)) {
          get("fishery_map", envir = env, inherits = FALSE)
        } else {
          NULL
        }
      }, error = function(e) NULL)
    }

    movement_label_for_var <- function(var_name, move_matrix) {
      prefix <- parse_prefix(var_name)
      if (!(prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs"))) {
        return(NA_character_)
      }

      directed <- directed_movement_pairs(move_matrix)
      if (nrow(directed) == 0) return(NA_character_)

      idx <- parse_var_indices(var_name)
      if (length(idx) == 0) return(NA_character_)

      movement_col <- if (length(idx) >= 2) {
        idx[[2]]
      } else {
        ((idx[[1]] - 1L) %% nrow(directed)) + 1L
      }
      if (!is.finite(movement_col) || movement_col < 1 || movement_col > nrow(directed)) {
        return(NA_character_)
      }

      movement_period <- if (length(idx) >= 2) idx[[1]] else ceiling(idx[[1]] / nrow(directed))
      from_region <- directed[movement_col, 1L]
      to_region <- directed[movement_col, 2L]
      paste0("R", from_region, " -> R", to_region, " (movement period ", movement_period, ")")
    }

    selectivity_label_for_var <- function(var_name, par_obj, fishery_map = NULL) {
      prefix <- parse_prefix(var_name)
      if (!(prefix %in% c("bs_selcoff_gp", "bs_selcoff"))) return(NA_character_)
      if (is.null(par_obj)) return(NA_character_)

      group_match <- regmatches(var_name, regexpr("(?<=bs_selcoff_gp:)[0-9]+", var_name, perl = TRUE))
      if (length(group_match) == 0 || !nzchar(group_match)) return(NA_character_)
      sel_group <- suppressWarnings(as.integer(group_match))
      if (!is.finite(sel_group)) return(NA_character_)

      n_fish <- tryCatch(as.integer(dimensions(par_obj)["fisheries"]), error = function(e) NA_integer_)
      if (!is.finite(n_fish) || n_fish <= 0) return(paste0("Selectivity group ", sel_group))

      flag24 <- tryCatch(flagval(par_obj, -seq_len(n_fish), 24)$value, error = function(e) rep(NA_integer_, n_fish))
      fish_in_group <- which(flag24 == sel_group)
      if (length(fish_in_group) == 0) return(paste0("Selectivity group ", sel_group, " (no fishery flag 24 match)"))

      fishery_name <- function(fishery_id) {
        if (!is.null(fishery_map) && all(c("fishery", "fishery_name") %in% names(fishery_map))) {
          hit <- fishery_map$fishery == fishery_id
          if (any(hit, na.rm = TRUE)) return(as.character(fishery_map$fishery_name[which(hit)[1]]))
        }
        paste0("Fishery ", fishery_id)
      }

      rep_fish <- fish_in_group[[1]]
      fish_labels <- paste0(fish_in_group, ":", vapply(fish_in_group, fishery_name, character(1)))
      fish_text <- if (length(fish_labels) > 4) {
        paste0(paste(fish_labels[1:4], collapse = "; "), "; +", length(fish_labels) - 4, " more")
      } else {
        paste(fish_labels, collapse = "; ")
      }

      paste0("Sel group ", sel_group, ": ", fish_text)
    }

    # Reactive: process bound hits data
    bounds_data <- reactive({
      req(rv$data_loaded, input$scenarios)
    
      # Process indepvar.rpt for each scenario
      results <- map(input$scenarios, function(model_name) {
        df <- parse_indepvar(rv$IndepOut_list[[model_name]])
        if (is.null(df)) return(NULL)
        move_matrix <- load_model_move_matrix(model_name)
        par_obj <- load_model_par_object(model_name)
        fishery_map <- load_model_fishery_map(model_name)
      
        # Calculate distances to bounds and identify hit type
        df <- df %>%
          mutate(
            Distance_to_lower = abs(Estimate - L_bound),
            Distance_to_upper = abs(Estimate - U_bound),
            Var_Type = bound_var_type(Var_name),
            Movement = vapply(Var_name, movement_label_for_var, character(1), move_matrix = move_matrix),
            Selectivity = vapply(Var_name, selectivity_label_for_var, character(1), par_obj = par_obj, fishery_map = fishery_map),
            Hit_Type = case_when(
              !Hit_Bound ~ "None",
              Distance_to_lower <= Distance_to_upper ~ "Lower",
              TRUE ~ "Upper"
            )
          )
      
        # Filter to only parameters that hit bounds
        bound_hits <- df %>% filter(Hit_Bound)
        movement_rows <- df %>%
          filter(Var_Type %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs"))
        movement_hits <- movement_rows %>% filter(Hit_Bound)
        movement_summary <- data.frame(
          Movement_Params = nrow(movement_rows),
          Movement_Bound_Hits = nrow(movement_hits),
          Movement_Hit_Rate = if (nrow(movement_rows) > 0) {
            sprintf("%.2f%%", nrow(movement_hits) / nrow(movement_rows) * 100)
          } else {
            "NA"
          },
          Unique_Movement_Directions_Hit = if ("Movement" %in% names(movement_hits)) {
            length(unique(na.omit(movement_hits$Movement)))
          } else {
            0L
          },
          stringsAsFactors = FALSE
        )

        list(total_params = nrow(df), bound_hits = bound_hits, movement_summary = movement_summary)
      })
      names(results) <- input$scenarios
      Filter(Negate(is.null), results)
    })
  
    # Render bound hits overview table
    output$bounds_overview <- renderDT({
      req(bounds_data())
    
      # Create summary table
      overview <- data.frame(
        Model = names(bounds_data()),
        Total_Params = sapply(bounds_data(), function(x) x$total_params),
        Bound_Hits = sapply(bounds_data(), function(x) nrow(x$bound_hits)),
        Lower_Hits = sapply(bounds_data(), function(x) sum(x$bound_hits$Hit_Type == "Lower", na.rm = TRUE)),
        Upper_Hits = sapply(bounds_data(), function(x) sum(x$bound_hits$Hit_Type == "Upper", na.rm = TRUE)),
        Movement_Params = sapply(bounds_data(), function(x) x$movement_summary$Movement_Params),
        Movement_Bound_Hits = sapply(bounds_data(), function(x) x$movement_summary$Movement_Bound_Hits),
        Movement_Hit_Rate = sapply(bounds_data(), function(x) x$movement_summary$Movement_Hit_Rate),
        Unique_Movement_Directions_Hit = sapply(bounds_data(), function(x) x$movement_summary$Unique_Movement_Directions_Hit),
        Hit_Rate = sapply(bounds_data(), function(x) 
          sprintf("%.2f%%", nrow(x$bound_hits) / x$total_params * 100))
      )
    
      datatable(overview, 
                options = list(pageLength = 10, dom = 'tip', deferRender = TRUE), 
                rownames = FALSE)
    })

    output$bounds_type_summary <- renderDT({
      req(input$bound_model, bounds_data())

      if (!input$bound_model %in% names(bounds_data())) {
        return(NULL)
      }

      bounds <- bounds_data()[[input$bound_model]]$bound_hits

      if (nrow(bounds) == 0) {
        return(datatable(
          data.frame(Message = "✓ No bound hits detected"),
          options = list(dom = "t", deferRender = TRUE),
          rownames = FALSE
        ))
      }

      summary_tbl <- bounds %>%
        group_by(Var_Type, Hit_Type) %>%
        summarise(
          Count = dplyr::n(),
          Context = {
            context <- Selectivity[!is.na(Selectivity) & nzchar(Selectivity)]
            if (length(context) == 0) NA_character_ else context[[1]]
          },
          .groups = "drop"
        ) %>%
        arrange(Var_Type, match(Hit_Type, c("Lower", "Upper")))

      datatable(
        summary_tbl,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, deferRender = TRUE),
        rownames = FALSE
      ) %>%
        formatStyle(
          "Hit_Type",
          color = styleEqual(
            c("Lower", "Upper"),
            c("#1d4ed8", "#dc2626")
          ),
          fontWeight = "600"
        )
    })
  
    # Render detailed bound hits table
    output$bounds_detail <- renderDT({
      req(input$bound_model, bounds_data())
    
      # Check if data exists for selected model
      if (!input$bound_model %in% names(bounds_data())) {
        return(data.frame(Message = "No data available for this model"))
      }
    
      bounds <- bounds_data()[[input$bound_model]]$bound_hits
    
      # Display message if no bound hits
      if (nrow(bounds) == 0) {
        data.frame(Message = "✓ No bound hits detected")
      } else {
        # Display detailed bound hits
        bounds %>%
          select(Index, Var_Type, Var_name, Movement, Selectivity, Estimate, Hit_Type, L_bound, U_bound) %>%
          datatable(options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE), 
                    rownames = FALSE) %>%
          formatStyle(
            "Hit_Type",
            color = styleEqual(
              c("Lower", "Upper"),
              c("#1d4ed8", "#dc2626")
            ),
            fontWeight = "600"
          )
      }
    })
  
    # Download handler for bound hits CSV
    output$download_bounds <- downloadHandler(
      filename = function() {
        paste0("bound_hits_", input$bound_model, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(input$bound_model, bounds_data())
        bounds <- bounds_data()[[input$bound_model]]$bound_hits
        bounds %>%
          select(Index, Var_Type, Var_name, Movement, Selectivity, Estimate, Hit_Type, L_bound, U_bound) %>%
          write.csv(file, row.names = FALSE)
      }
    )
  
    # ===========================================================================

}
