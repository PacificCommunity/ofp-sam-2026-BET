mod_summary_ui <- function() {
      tabItem(
        tabName = "summary",
        h2("Model Summary", style = "color: #3c8dbc;"),

        fluidRow(
          box(
            title = "Display Options",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            checkboxInput("summary_show_run_description", "Show Run Description", value = FALSE)
          )
        ),
        
        # Overall summary value boxes
        fluidRow(
          valueBoxOutput("n_models", width = 4),
          valueBoxOutput("total_scenarios", width = 4),
          valueBoxOutput("overall_year_range", width = 4)
        ),
        
        # Model-specific information boxes
        fluidRow(
          box(
            title = "Model-Specific Information",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            uiOutput("model_info_boxes")
          )
        ),
        
        # Model configuration table
        fluidRow(
          box(
            title = "Detailed Model Configuration",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            DTOutput("summary_table")
          )
        ),

        fluidRow(
          box(
            title = "indepvar.rpt Parameter Guide by Model",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            selectInput("summary_param_model", "Model:", choices = NULL, selected = NULL),
            DTOutput("summary_param_guide_table")
          )
        ),

        fluidRow(
          box(
            title = "Parameter Group Comparison (Base vs One or More Models)",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            fluidRow(
              column(4, selectInput("summary_compare_model_a", "Model A (Baseline):", choices = NULL, selected = NULL)),
              column(4, pickerInput(
                "summary_compare_model_b", "Model B (Compare, multi-select):",
                choices = NULL, selected = NULL, multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 10)
              )),
              column(4,
                selectInput("summary_compare_view", "Rows:", choices = c("Changed only" = "changed", "All" = "all"), selected = "changed"),
                actionButton("summary_compare_apply", "Apply", class = "btn-primary")
              )
            ),
            tags$p(style = "margin: 6px 0 10px 0; color: #666;", "Overview: each column is a compare model, each cell is Delta Entries (Compare - Base)."),
            DTOutput("summary_param_compare_matrix"),
            tags$hr(style = "margin: 10px 0;"),
            DTOutput("summary_param_compare_table")
          )
        )
      )
}

mod_summary_server <- function(input, output, session, rv) {
    # TAB 1: MODEL SUMMARY
    # ===========================================================================

    get_model_year_label <- function(model_name) {
      year_range <- rv$YearRanges[[model_name]]
      if (is.null(year_range)) return(NA_character_)

      min_year <- suppressWarnings(as.integer(year_range$minYear))
      max_year <- suppressWarnings(as.integer(year_range$maxYear))
      min_year <- if (length(min_year) > 0) min_year[[1]] else NA_integer_
      max_year <- if (length(max_year) > 0) max_year[[1]] else NA_integer_

      if (!is.finite(min_year) || !is.finite(max_year)) return(NA_character_)
      paste(min_year, "-", max_year)
    }

    get_model_summary_details <- function(model_name) {
      par <- rv$ParOut_list[[model_name]]
      if (is.null(par) || !methods::is(par, "MFCLPar")) {
        return(list(
          valid = FALSE,
          model_name = model_name,
          message = "Model summary unavailable because ParOut is missing."
        ))
      }

      dims <- tryCatch(as.list(par@dimensions), error = function(e) NULL)
      if (is.null(dims)) {
        return(list(
          valid = FALSE,
          model_name = model_name,
          message = "Model summary unavailable because model dimensions could not be read."
        ))
      }

      info <- rv$Info_list[[model_name]]
      description <- if (!is.null(info$description) && nzchar(info$description)) info$description else NA_character_
      config_summary <- if (!is.null(info$config_summary) && nzchar(info$config_summary)) info$config_summary else NA_character_
      change_label <- if (!is.null(info$change_token_label) && nzchar(info$change_token_label)) info$change_token_label else "None"
      change_description <- if (!is.null(info$input_change_description) && nzchar(info$input_change_description)) info$input_change_description else NA_character_
      change_source <- if (!is.null(info$change_token_source) && nzchar(info$change_token_source)) info$change_token_source else "none"

      list(
        valid = TRUE,
        model_name = model_name,
        par = par,
        dims = dims,
        description = description,
        config_summary = config_summary,
        change_label = change_label,
        change_description = change_description,
        change_source = change_source,
        year_label = get_model_year_label(model_name)
      )
    }

    get_valid_model_summary_details <- function(model_names) {
      details <- lapply(model_names, get_model_summary_details)
      Filter(function(x) isTRUE(x$valid), details)
    }

    model_display_choices <- function(models) {
      models <- as.character(models)
      choices <- rv$model_choice_labels
      if (!is.null(choices) && length(choices) > 0) {
        out <- choices[unname(choices) %in% models]
        if (length(out) > 0) return(out)
      }
      stats::setNames(models, models)
    }
  
    parse_prefix <- function(var_name) {
      v <- tolower(trimws(as.character(var_name)))
      p <- sub("\\(.*$", "", v)
      p <- sub(":.*$", "", p)
      trimws(p)
    }

    parse_indices_from_names <- function(var_names, prefix) {
      if (is.null(var_names) || length(var_names) == 0) return(integer(0))
      pat <- paste0("^", prefix, ".*?\\((\\d+)")
      idx <- vapply(var_names, function(x) {
        m <- regexec(pat, tolower(as.character(x)))
        g <- regmatches(tolower(as.character(x)), m)[[1]]
        if (length(g) < 2) return(NA_integer_)
        suppressWarnings(as.integer(g[[2]]))
      }, integer(1))
      idx <- idx[is.finite(idx)]
      sort(unique(as.integer(idx)))
    }

    extract_param_family <- function(var_name) {
      v <- tolower(trimws(as.character(var_name)))
      prefix <- parse_prefix(v)
      if (prefix %in% c("bs_selcoff_gp", "selcoff", "ageselcoff", "sel_dev_coffs")) return("Selectivity parameters")
      if (prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs", "region_rec_diff_coffs")) return("Movement parameters")
      if (prefix %in% c("effort_dev_coffs", "grouped_catchability_coffs", "catch_dev_coffs", "q0", "q0_miss", "fm_level_devs")) return("Catchability / effort-deviation parameters")
      if (prefix %in% c("recr", "totpop", "totpop_coff", "region_pars", "region_rec_diffs", "region_rec_diff_coffs", "rec_init_diff")) return("Recruitment")
      if (prefix %in% c("tag_fish_rep", "rep_dev_coffs")) return("Tagging")
      if (prefix %in% c("vb_coff", "var_coff")) return("Growth / size-distribution parameters")
      if (prefix %in% c("sv")) return("Structural / process parameters")
      if (grepl("^age_pars", v)) return("Age parameters")
      "Other / mixed"
    }

    lookup_param_reference <- function(group_name, var_names = NULL) {
      prefix <- parse_prefix(group_name)

      if (prefix %in% c("bs_selcoff_gp", "bs_selcoff", "selcoff", "ageselcoff", "sel_dev_coffs")) {
        return(list(
          Description = "Selectivity coefficient/deviate parameter used for fishery selectivity-at-age (including time-block/season/grouped selectivity variants).",
          Source = "MFCL User's Guide §3.4.2; src/variable.hpp and src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs")) {
        return(list(
          Description = "Movement diffusion coefficient parameter (including age-dependent/orthogonal-polynomial parameterizations).",
          Source = "MFCL User's Guide §4.5.12; src/variable.hpp and src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("region_rec_diffs", "region_rec_diff_coffs")) {
        return(list(
          Description = "Regional recruitment-deviation coefficients controlling time-varying recruitment distribution among regions.",
          Source = "MFCL User's Guide §3.2.1; src/newmau5a.cpp and src/variable.hpp"
        ))
      }
      if (prefix %in% c("tag_fish_rep", "rep_dev_coffs")) {
        return(list(
          Description = "Tag reporting-rate parameter or its deviation term for tagging likelihood formulations.",
          Source = "MFCL User's Guide §4.5.10; src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("recr")) {
        return(list(
          Description = "Recruitment time-series parameter in the core population dynamics model.",
          Source = "MFCL User's Guide §3.2; src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("region_pars")) {
        idx <- parse_indices_from_names(var_names, "region_pars")
        if (length(idx) == 1 && idx[[1]] == 1L) {
          return(list(
            Description = "Region-level recruitment distribution proportions (region_pars row 1).",
            Source = "MFCL User's Guide region parameters section; src/newmau5a.cpp region_pars(1)"
          ))
        }
        return(list(
          Description = "Region-level recruitment distribution parameters.",
          Source = "MFCL User's Guide §3.2.1; src/newmau5a.cpp and src/newmult.cpp"
        ))
      }
      if (prefix %in% c("vb_coff")) {
        return(list(
          Description = "Von Bertalanffy growth coefficients used in age-length conversions and size-based components.",
          Source = "MFCL User's Guide growth parameterization; src/newmaux5.cpp and src/lbselclc.cpp"
        ))
      }
      if (prefix %in% c("var_coff")) {
        return(list(
          Description = "Size-at-age variability coefficients used in length/weight distribution and selectivity-at-length calculations.",
          Source = "MFCL User's Guide growth/variance settings; src/newmaux5.cpp and src/onevar.cpp"
        ))
      }
      if (prefix %in% c("age_pars")) {
        idx <- parse_indices_from_names(var_names, "age_pars")
        if (length(idx) == 1 && idx[[1]] == 5L) {
          return(list(
            Description = "age_pars(5): natural-mortality-at-age functional-form parameters (including Lorenzen option depending on flags).",
            Source = "MFCL User's Guide age_pars definitions; src/newmaux5.cpp"
          ))
        }
        return(list(
          Description = "Age-structured biological parameter block.",
          Source = "MFCL User's Guide biology section; src/newmaux5.cpp"
        ))
      }
      if (prefix %in% c("sv")) {
        idx <- parse_indices_from_names(var_names, "sv")
        if (length(idx) == 1 && idx[[1]] == 21L) {
          return(list(
            Description = "Beverton-Holt stock-recruitment beta scaling parameter (density dependence strength). In code: beta = B0 * (sv(21)+0.001).",
            Source = "MFCL source: vrbioclc.cpp, tx.cpp, do_all_for_empirical_autocorrelated_bh.cpp (sv(21))"
          ))
        }
        if (length(idx) == 1 && idx[[1]] == 29L) {
          return(list(
            Description = "Beverton-Holt stock-recruitment steepness (h) parameter.",
            Source = "MFCL source/manual: vrbioclc.cpp (sv(29)=steepness), MULTIFAN-CL-Users-Guide sv(29)"
          ))
        }
        if (all(c(21L, 29L) %in% idx)) {
          return(list(
            Description = "Core Beverton-Holt SRR parameter set: sv(21)=beta scaling (density dependence strength), sv(29)=steepness (h).",
            Source = "MFCL source: vrbioclc.cpp, tx.cpp; manual sv(29)"
          ))
        }
        idx_label <- if (length(idx) == 0) "unknown" else paste(idx, collapse = ", ")
        return(list(
          Description = paste0("MFCL structural/process scalar vector parameter. Observed sv indices in this model: ", idx_label, "."),
          Source = "MFCL User's Guide sv notes; src/newmaux5.cpp and src/callpen.cpp"
        ))
      }
      if (prefix %in% c("totpop", "totpop_coff")) {
        return(list(
          Description = "Population scaling parameter for initial total population/recruitment scale.",
          Source = "MFCL User's Guide initial population/recruitment; src/newmau5a.cpp (totpop)"
        ))
      }

      list(
        Description = paste0("Estimated MFCL parameter group from indepvar.rpt (group='", prefix, "')."),
        Source = "MFCL indepvar.rpt design + source code parameter vector"
      )
    }

    build_model_param_guide <- function(model_name) {
      indep <- rv$IndepOut_list[[model_name]]
      df <- parse_indepvar(indep)
      if (is.null(df) || nrow(df) == 0) return(NULL)

      df$Param_Group <- vapply(df$Var_name, parse_prefix, character(1))
      tbl <- df %>%
        group_by(Param_Group) %>%
        summarise(
          Family = extract_param_family(first(Param_Group)),
          Entries = dplyr::n(),
          Bound_Hits = sum(Hit_Bound %in% TRUE, na.rm = TRUE),
          Var_Names = list(sort(unique(Var_name))),
          Example_Names = paste(utils::head(sort(unique(Var_name)), 3), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(Bound_Hits), desc(Entries), Param_Group)

      ref_info <- mapply(
        FUN = function(group_name, name_list) lookup_param_reference(group_name, name_list),
        group_name = tbl$Param_Group,
        name_list = tbl$Var_Names,
        SIMPLIFY = FALSE
      )
      tbl$Description <- vapply(ref_info, function(x) x$Description, character(1))
      tbl$Source <- vapply(ref_info, function(x) x$Source, character(1))
      tbl$Var_Names <- NULL

      tbl <- tbl %>%
        rename(
          `Parameter Group` = Param_Group,
          `Bound Hits` = Bound_Hits,
          `Example Names` = Example_Names
        )

      total_row <- tbl[1, , drop = FALSE]
      total_row[,] <- NA
      total_row[1, "Parameter Group"] <- "TOTAL"
      total_row[1, "Family"] <- ""
      total_row[1, "Entries"] <- sum(tbl$Entries, na.rm = TRUE)
      total_row[1, "Bound Hits"] <- sum(tbl$`Bound Hits`, na.rm = TRUE)
      total_row[1, "Example Names"] <- ""
      total_row[1, "Description"] <- "Sum of rows above for this model."
      total_row[1, "Source"] <- ""
      bind_rows(tbl, total_row)
    }

    build_model_param_guide_core <- function(model_name) {
      indep <- rv$IndepOut_list[[model_name]]
      df <- parse_indepvar(indep)
      if (is.null(df) || nrow(df) == 0) return(NULL)

      df$Param_Group <- vapply(df$Var_name, parse_prefix, character(1))
      tbl <- df %>%
        group_by(Param_Group) %>%
        summarise(
          Family = extract_param_family(first(Param_Group)),
          Entries = dplyr::n(),
          Bound_Hits = sum(Hit_Bound %in% TRUE, na.rm = TRUE),
          Var_Names = list(sort(unique(Var_name))),
          .groups = "drop"
        ) %>%
        arrange(desc(Bound_Hits), desc(Entries), Param_Group)

      ref_info <- mapply(
        FUN = function(group_name, name_list) lookup_param_reference(group_name, name_list),
        group_name = tbl$Param_Group,
        name_list = tbl$Var_Names,
        SIMPLIFY = FALSE
      )
      tbl$Description <- vapply(ref_info, function(x) x$Description, character(1))
      tbl$Source <- vapply(ref_info, function(x) x$Source, character(1))
      tbl$Var_Names <- NULL

      tbl %>%
        rename(
          `Parameter Group` = Param_Group,
          `Bound Hits` = Bound_Hits
        )
    }

    # Render model summary table
    output$summary_table <- renderDT({
      req(rv$data_loaded, input$scenarios)

      details <- lapply(input$scenarios, get_model_summary_details)
      valid_details <- Filter(function(x) isTRUE(x$valid), details)
      invalid_details <- Filter(function(x) !isTRUE(x$valid), details)

      params_df <- dplyr::bind_rows(lapply(valid_details, function(detail) {
        out <- data.frame(
          Model = detail$model_name,
          Change_Tokens = detail$change_label,
          Model_Description = detail$description,
          Max_Grad = sprintf("%.6f", as.numeric(detail$par@max_grad)),
          Obj_Fun = sprintf("%.2f", as.numeric(detail$par@obj_fun)),
          N_Pars = as.numeric(detail$par@n_pars),
          Fisheries = detail$dims$fisheries,
          Years = detail$year_label,
          Regions = detail$dims$regions,
          Seasons = detail$dims$seasons,
          stringsAsFactors = FALSE
        )
        if (isTRUE(input$summary_show_run_description)) {
          out$Run_Description <- detail$config_summary
        }
        out
      }))

      if (length(invalid_details) > 0) {
        invalid_rows <- dplyr::bind_rows(lapply(invalid_details, function(detail) {
          out <- data.frame(
            Model = detail$model_name,
            Change_Tokens = NA_character_,
            Model_Description = detail$message,
            Max_Grad = NA_character_,
            Obj_Fun = NA_character_,
            N_Pars = NA_real_,
            Fisheries = NA_real_,
            Years = NA_character_,
            Regions = NA_real_,
            Seasons = NA_real_,
            stringsAsFactors = FALSE
          )
          if (isTRUE(input$summary_show_run_description)) {
            out$Run_Description <- NA_character_
          }
          out
        }))
        params_df <- dplyr::bind_rows(params_df, invalid_rows)
      }

      validate(need(nrow(params_df) > 0, "No selected models have summary information available."))

      # Display as interactive table
      datatable(params_df, 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'tip', deferRender = TRUE),
                rownames = FALSE)
    })
  
    # Value box: number of models selected
    output$n_models <- renderValueBox({
      req(rv$data_loaded)
      valueBox(
        length(input$scenarios), "Models Selected", 
        icon = icon("check-square"),
        color = "blue"
      )
    })
  
    # Value box: total scenarios loaded
    output$total_scenarios <- renderValueBox({
      req(rv$data_loaded)
      valueBox(
        length(rv$ParOut_list), "Total Models Loaded", 
        icon = icon("cube"),
        color = "green"
      )
    })
  
    # Value box: overall year range
    output$overall_year_range <- renderValueBox({
      req(rv$data_loaded)
      valid_details <- get_valid_model_summary_details(input$scenarios)
      all_years <- unlist(lapply(valid_details, function(detail) {
        year_range <- rv$YearRanges[[detail$model_name]]
        c(
          suppressWarnings(as.integer(year_range$minYear)),
          suppressWarnings(as.integer(year_range$maxYear))
        )
      }))
      all_years <- all_years[is.finite(all_years)]
      year_label <- if (length(all_years) > 0) {
        paste(min(all_years), "-", max(all_years))
      } else {
        "Unavailable"
      }
      valueBox(
        year_label, "Overall Year Range", 
        icon = icon("calendar"),
        color = "yellow"
      )
    })
  
    # Render model-specific info boxes
    output$model_info_boxes <- renderUI({
      req(rv$data_loaded, input$scenarios)

      boxes <- lapply(input$scenarios, function(model_name) {
        detail <- get_model_summary_details(model_name)
        if (!isTRUE(detail$valid)) {
          return(column(
            width = 4,
            box(
              title = model_name,
              width = NULL,
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              tags$div(
                style = "padding: 10px; font-size: 13px;",
                detail$message
              )
            )
          ))
        }

        description <- if (!is.na(detail$description) && nzchar(detail$description)) detail$description else "No description available"
        config_summary <- if (!is.na(detail$config_summary) && nzchar(detail$config_summary)) detail$config_summary else "No config summary available"
        change_label <- if (!is.na(detail$change_label) && nzchar(detail$change_label)) detail$change_label else "Base input"
        change_description <- if (!is.na(detail$change_description) && nzchar(detail$change_description)) detail$change_description else ""
        n_index <- length(rv$INDEX_FISHERIES_MAPS[[model_name]])

        column(
          width = 4,
          box(
            title = detail$model_name,
            width = NULL,
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            tags$div(
              style = "padding: 5px;",
              tags$div(
                style = "margin-bottom: 8px; padding: 8px 10px; background: #f4f8fb; border-left: 3px solid #3c8dbc; border-radius: 3px; font-size: 12px;",
                tags$strong("Model Description: "),
                description
              ),
              tags$div(
                style = "margin-bottom: 8px; padding: 8px 10px; background: #f7fff7; border-left: 3px solid #00a65a; border-radius: 3px; font-size: 12px;",
                tags$strong("Change tokens: "),
                change_label,
                if (nzchar(change_description) && !identical(change_description, change_label)) {
                  tags$span(paste0(" - ", change_description))
                },
                tags$span(
                  paste0(" (source: ", detail$change_source, ")"),
                  style = "color:#777;"
                )
              ),
              if (isTRUE(input$summary_show_run_description)) {
                tags$div(
                  style = "margin-bottom: 8px; padding: 8px 10px; background: #f8fafc; border-left: 3px solid #6b7280; border-radius: 3px; font-size: 12px;",
                  tags$strong("Run Description: "),
                  config_summary
                )
              },
              tags$table(
                style = "width: 100%; font-size: 13px;",
                tags$tr(
                  tags$td(tags$strong("🎣 Fisheries:"), style = "width: 60%;"),
                  tags$td(detail$dims$fisheries, style = "text-align: right;")
                ),
                tags$tr(
                  tags$td(tags$strong("📊 Index Fisheries:"), style = "padding-top: 5px;"),
                  tags$td(n_index, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📅 Years:"), style = "padding-top: 5px;"),
                  tags$td(
                    if (!is.na(detail$year_label)) detail$year_label else "Unavailable",
                    style = "text-align: right; padding-top: 5px;"
                  )
                ),
                tags$tr(
                  tags$td(tags$strong("🗺️ Regions:"), style = "padding-top: 5px;"),
                  tags$td(detail$dims$regions, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📆 Seasons:"), style = "padding-top: 5px;"),
                  tags$td(detail$dims$seasons, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📈 Parameters:"), style = "padding-top: 5px;"),
                  tags$td(detail$par@n_pars, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("🎯 Max Gradient:"), style = "padding-top: 5px;"),
                  tags$td(
                    sprintf("%.2e", as.numeric(detail$par@max_grad)),
                    style = "text-align: right; padding-top: 5px;"
                  )
                ),
                tags$tr(
                  tags$td(tags$strong("💰 Obj Function:"), style = "padding-top: 5px;"),
                  tags$td(
                    sprintf("%.2f", as.numeric(detail$par@obj_fun)),
                    style = "text-align: right; padding-top: 5px;"
                  )
                )
              )
            )
          )
        )
      })
    
      # Arrange boxes in rows of 3
      do.call(fluidRow, boxes)
    })

    observe({
      req(rv$data_loaded, input$scenarios)
      models <- input$scenarios
      selected <- input$summary_param_model
      if (is.null(selected) || !(selected %in% models)) selected <- models[[1]]
      updateSelectInput(session, "summary_param_model", choices = model_display_choices(models), selected = selected)
    })

    observe({
      req(rv$data_loaded, input$scenarios)
      models <- input$scenarios
      if (length(models) == 0) return()

      sel_a <- isolate(input$summary_compare_model_a)
      if (is.null(sel_a) || !(sel_a %in% models)) sel_a <- models[[1]]
      updateSelectInput(session, "summary_compare_model_a", choices = model_display_choices(models), selected = sel_a)
    })

    observe({
      req(rv$data_loaded, input$scenarios, input$summary_compare_model_a)
      models <- input$scenarios
      if (length(models) == 0) return()

      choices_b <- setdiff(models, input$summary_compare_model_a)
      if (length(choices_b) == 0) choices_b <- models

      sel_b <- isolate(input$summary_compare_model_b)
      sel_b <- intersect(sel_b, choices_b)
      if (length(sel_b) == 0) sel_b <- choices_b[[1]]

      updatePickerInput(
        session, "summary_compare_model_b",
        choices = model_display_choices(choices_b), selected = sel_b
      )
    })

    output$summary_param_guide_table <- renderDT({
      req(rv$data_loaded, input$scenarios)
      model_name <- input$summary_param_model
      if (is.null(model_name) || !nzchar(model_name) || !(model_name %in% input$scenarios)) {
        model_name <- input$scenarios[[1]]
      }
      tbl <- build_model_param_guide(model_name)
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      datatable(
        tbl,
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip", deferRender = TRUE),
        rownames = FALSE
      )
    })

    compare_param_table_reactive <- eventReactive(input$summary_compare_apply, {
      req(rv$data_loaded, input$scenarios)
      model_a <- input$summary_compare_model_a
      model_bs <- input$summary_compare_model_b

      if (is.null(model_a) || !(model_a %in% input$scenarios)) model_a <- input$scenarios[[1]]
      if (is.null(model_bs) || length(model_bs) == 0) {
        model_bs <- if (length(input$scenarios) >= 2) input$scenarios[[2]] else input$scenarios[[1]]
      } else {
        model_bs <- intersect(model_bs, input$scenarios)
      }
      model_bs <- setdiff(model_bs, model_a)
      if (length(model_bs) == 0) model_bs <- if (length(input$scenarios) >= 2) setdiff(input$scenarios, model_a)[1] else model_a

      a_tbl <- build_model_param_guide_core(model_a)
      if (is.null(a_tbl) && all(vapply(model_bs, function(m) is.null(build_model_param_guide_core(m)), logical(1)))) return(NULL)
      if (is.null(a_tbl)) a_tbl <- data.frame(`Parameter Group` = character(0), Family = character(0), Entries = integer(0), stringsAsFactors = FALSE)
      a_view <- a_tbl %>% select(`Parameter Group`, Family_A = Family, Entries_A = Entries)

      cmp_list <- lapply(model_bs, function(model_b) {
        b_tbl <- build_model_param_guide_core(model_b)
        if (is.null(b_tbl)) b_tbl <- data.frame(`Parameter Group` = character(0), Family = character(0), Entries = integer(0), stringsAsFactors = FALSE)
        b_view <- b_tbl %>% select(`Parameter Group`, Family_B = Family, Entries_B = Entries)

        full_join(a_view, b_view, by = "Parameter Group") %>%
          mutate(
            Entries_A = ifelse(is.na(Entries_A), 0L, as.integer(Entries_A)),
            Entries_B = ifelse(is.na(Entries_B), 0L, as.integer(Entries_B)),
            Family = coalesce(Family_B, Family_A),
            Status = case_when(
              Entries_A == 0L & Entries_B > 0L ~ "Added in Compare",
              Entries_A > 0L & Entries_B == 0L ~ "Removed in Compare",
              TRUE ~ "Common"
            ),
            `Delta Entries (Compare-Base)` = Entries_B - Entries_A,
            `Compare Model` = model_b
          ) %>%
          select(
            `Compare Model`,
            Status,
                `Parameter Group`,
                Family,
                `Base Entries` = Entries_A,
                `Compare Entries` = Entries_B,
                `Delta Entries (Compare-Base)`
              )
      })

      cmp <- dplyr::bind_rows(cmp_list)

      cmp
    }, ignoreInit = FALSE)

    output$summary_param_compare_matrix <- renderDT({
      cmp <- compare_param_table_reactive()
      req(!is.null(cmp), nrow(cmp) > 0)

      if (identical(input$summary_compare_view, "changed")) {
        cmp <- cmp %>% filter(Status != "Common" | `Delta Entries (Compare-Base)` != 0L)
      }

      matrix_tbl <- cmp %>%
        select(`Parameter Group`, Family, `Compare Model`, `Delta Entries (Compare-Base)`) %>%
        tidyr::pivot_wider(
          names_from = `Compare Model`,
          values_from = `Delta Entries (Compare-Base)`,
          values_fill = 0
        ) %>%
        arrange(`Parameter Group`)

      datatable(
        matrix_tbl,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left;",
          paste0("Overview Matrix | Base: ", input$summary_compare_model_a)
        ),
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip", deferRender = TRUE),
        rownames = FALSE
      )
    })

    output$summary_param_compare_table <- renderDT({
      cmp <- compare_param_table_reactive()
      req(!is.null(cmp), nrow(cmp) > 0)
      base_model <- input$summary_compare_model_a
      compared_models <- sort(unique(as.character(cmp$`Compare Model`)))

      if (identical(input$summary_compare_view, "changed")) {
        cmp <- cmp %>% filter(Status != "Common" | `Delta Entries (Compare-Base)` != 0L)
      }

      cmp <- cmp %>%
        mutate(
          StatusRank = case_when(
            Status == "Added in Compare" ~ 1L,
            Status == "Removed in Compare" ~ 2L,
            TRUE ~ 3L
          ),
          DeltaMag = abs(`Delta Entries (Compare-Base)`)
        ) %>%
        arrange(`Compare Model`, StatusRank, desc(DeltaMag), `Parameter Group`) %>%
        select(-StatusRank, -DeltaMag)

      datatable(
        cmp,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left;",
          paste0("Base: ", base_model, " | Compared: ", paste(compared_models, collapse = ", "))
        ),
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip", deferRender = TRUE),
        rownames = FALSE
      )
    })
  
    # ===========================================================================

}
