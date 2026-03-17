source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")
source("R/modules/mod_likelihood.R")
source("R/modules/mod_sections.R")

ui <- dashboardPage(
  skin = "blue",
  
  # ---------------------------------------------------------------------------
  # Header
  # ---------------------------------------------------------------------------
  dashboardHeader(
    title = "MFCL Output Viewer",
    tags$li(
      class = "dropdown",
      style = "padding: 10px 15px; min-width: 360px;",
      div(
        style = "font-size: 11px; color: #e8f3ff; margin-bottom: 4px; white-space: nowrap;",
        "Quick figures: put files in ",
        tags$code("plot/shiny_plot/www/quick_figures", style = "color:#fff; background:rgba(0,0,0,0.25); padding:1px 4px;")
      ),
      pickerInput(
        "quick_fig_select",
        NULL,
        choices = character(0),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "count > 2",
          countSelectedText = "{0} selected"
        ),
        width = "320px"
      ),
      div(
        style = "margin-top: 6px; display: flex; gap: 6px; flex-wrap: wrap;",
        actionButton("quick_fig_refresh", "Refresh", class = "btn-default btn-sm"),
        actionButton("quick_fig_view_btn", "View", class = "btn-info btn-sm")
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Sidebar
  # ---------------------------------------------------------------------------
  dashboardSidebar(
    div(
      style = "margin: 10px 12px 8px 12px; padding: 10px; background: #f7fbff; border: 1px solid #cfe3f2; border-left: 4px solid #3c8dbc; border-radius: 4px; color: #1f2d3d; line-height: 1.35;",
      tags$div(
        style = "margin-bottom: 6px;",
        tags$img(src = "spc-logo.svg", alt = "Pacific Community (SPC) logo", style = "max-width: 100%; height: 34px; display: block;")
      ),
      tags$div("Created by Kyuhan Kim", style = "font-weight: 700; font-size: 12px;"),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "Contact info: ",
        tags$a("kyuhank@spc.int", href = "mailto:kyuhank@spc.int", style = "color: #3c8dbc;")
      ),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "GitHub: ",
        tags$a("github.com/kyuhank", href = "https://github.com/kyuhank", target = "_blank", style = "color: #3c8dbc;")
      )
    ),
    
    # Global model filter (shown after data is loaded)
    conditionalPanel(
      condition = "output.data_loaded == true",
      h5("🎯 Filter Models", style = "padding-left: 15px; font-weight: bold;"),
      pickerInput("scenarios", NULL,
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectAllText = "All",
                    deselectAllText = "None",
                    selectedTextFormat = "count > 2",
                    liveSearch = TRUE
                  )),
      div(
        style = "padding: 6px 15px 0 15px;",
        checkboxInput("live_update_plots", "Live update plots", value = FALSE),
        tags$small("When enabled, plots update immediately as selections change.",
                   style = "display:block; margin-top:-6px; color:#666;")
      )
    ),

    div(style = "display:none;",
        textInput("plot_export_dir", NULL, value = "")
    ),

    shiny::hr(),

    # Navigation menu
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Model Summary", tabName = "summary", icon = NULL),
      menuItem("🏷️ Fishery Names", tabName = "fishery_names", icon = NULL),
      menuItem("⚠️ Bound Hits", tabName = "bounds", icon = NULL),
      menuItem("📈 CPUE Fits", tabName = "cpue", icon = NULL),
      menuItem("📏 Length Frequency", tabName = "lf", icon = NULL),
      menuItem("⚖️ Weight Frequency", tabName = "wf", icon = NULL),
      menuItem("📉 Diagnostics", tabName = "diagnostics", icon = NULL),
      menuItem("⭐ Key Quantities", tabName = "harvest", icon = NULL),
      menuItem("🎣 Tagging Dynamics", tabName = "tagging", icon = NULL),
      menuItem("🚢 Fishery Process", tabName = "fishery_process", icon = NULL),
      menuItem("🧬 Population Biology", tabName = "population_biology", icon = NULL)
    ),
    
    shiny::hr(),
    
    # Data loading section
    h4("📁 Load Model Data", style = "padding-left: 15px; color: #3c8dbc;"),
    
    # Model directory path input
    div(
      style = "margin: 0 15px;",
      textInput("model_dir", "Model Directory:",
                value = normalizePath("..", mustWork = FALSE),
                placeholder = "/path/to/model")
    ),
    
    # Browse + Refresh buttons
    div(
      style = "margin: 0 15px 10px 15px;",
      shinyFiles::shinyDirButton("browse_dir", "Browse...", 
                                 title = "Select Model Directory",
                                 icon = icon("folder-open"),
                                 class = "btn-info btn-sm"),
      actionButton("refresh_dir", "Refresh", 
                   icon = icon("sync"),
                   class = "btn-default btn-sm",
                   style = "margin-left: 8px;")
    ),
    
    helpText("Folder containing model subfolders", 
             style = "margin: 0 15px 10px 15px; font-size: 11px; color: #777;"),
    
    # Display detected models before loading with dropdown selection
    conditionalPanel(
      condition = "output.scenarios_detected == true",
      wellPanel(
        style = "background-color: #f5f5f5; margin: 10px 15px; padding: 12px;",
        h5("📦 Detected Models:", style = "margin-top: 0; color: #3c8dbc; font-weight: bold;"),
        
        # Summary info
        div(
          style = "background: white; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
          textOutput("detected_models_summary")
        ),
        
        # Searchable dropdown for model selection
        tags$style(HTML("
          label[for='models_to_load'] {
            color: #000 !important;
          }
        ")),
        pickerInput(
          "models_to_load",
          label = "Select models to load:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} models selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search models...",
            size = 10,
            dropupAuto = FALSE,
            style = "btn-default"
          )
        ),
        
        tags$small(
          "💡 Tip: Use search box to find specific models",
          style = "color: #666; font-style: italic; display: block; margin-top: 5px;"
        )
      )
    ),
    
    # Load data button
    div(
      style = "margin: 0 50px 10px 15px;",
      actionButton("load_data", "Load Data", 
                   icon = icon("upload"),
                   class = "btn-primary",
                   style = "width: 100%;")
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Body
  # ---------------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Pretty modal styling (used by shinyFiles browse dialog) */
        .modal-header {
          background: #3c8dbc;
          color: #fff;
          border-bottom: 0;
        }
        .modal-header .modal-title {
          font-weight: 600;
          letter-spacing: 0.2px;
        }
        .modal-content {
          border-radius: 8px;
          border: 1px solid #e6e9ef;
          box-shadow: 0 10px 30px rgba(22, 41, 70, 0.15);
        }
        .modal-body {
          background: #f7f9fb;
        }
        .modal-footer {
          border-top: 0;
          background: #f7f9fb;
        }
        .modal-footer .btn {
          border-radius: 6px;
        }
        /* shinyFiles modal prettify */
        .shinyFiles .form-group,
        .shinyfiles .form-group {
          margin-bottom: 10px;
        }
        .shinyFiles .form-control,
        .shinyfiles .form-control {
          border-radius: 6px;
          border: 1px solid #d7dce3;
          box-shadow: none;
        }
        .shinyFiles .btn,
        .shinyfiles .btn {
          border-radius: 6px;
        }
        .shinyFiles select.form-control,
        .shinyfiles select.form-control {
          background-color: #fff;
        }
        .modal-dialog {
          max-width: 860px;
          width: 80%;
        }
        .shinyFiles .well,
        .shinyfiles .well {
          background: #ffffff;
          border: 1px solid #e6e9ef;
          box-shadow: none;
        }
        .shinyFiles label,
        .shinyfiles label {
          font-weight: 600;
          color: #2c3e50;
        }
        .shinyFiles .help-block,
        .shinyfiles .help-block {
          color: #6c7a89;
        }
        .shinyFiles .btn-default,
        .shinyfiles .btn-default {
          background: #ffffff;
          border: 1px solid #d7dce3;
        }
        .shinyFiles .btn-default:hover,
        .shinyfiles .btn-default:hover {
          background: #f2f4f7;
        }
        .shinyFiles .btn-primary,
        .shinyfiles .btn-primary {
          background: #3c8dbc;
          border-color: #367fa9;
        }
        .shinyFiles .btn-primary:hover,
        .shinyfiles .btn-primary:hover {
          background: #367fa9;
        }
        .shinyFiles select,
        .shinyfiles select {
          font-size: 13px;
          padding: 6px 10px;
        }
        .shinyFiles .well .form-group:last-child,
        .shinyfiles .well .form-group:last-child {
          margin-bottom: 0;
        }
      "))
    ),
    
    # Custom CSS styling
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; font-size: 16px; }
        .small-box { cursor: default; }
        .content-wrapper { background-color: #ecf0f5; }
        .btn-primary { background-color: #3c8dbc; border-color: #367fa9; }
        .btn-primary:hover { background-color: #367fa9; }
        .well { padding: 10px; margin-bottom: 10px; }
        
        /* Fix detected models summary text color */
        #detected_models_summary {
          color: #333 !important;
          font-size: 13px;
          line-height: 1.5;
        }
        
        /* Editable table styling */
        .editable-cell {
          cursor: pointer;
          background-color: #ffffcc;
        }
        .editable-cell:hover {
          background-color: #ffff99;
        }

        /* Make selected picker items easier to identify (models and other multi-selects) */
        .bootstrap-select .dropdown-menu li.selected > a,
        .bootstrap-select .dropdown-menu li.selected > a:hover,
        .bootstrap-select .dropdown-menu li.selected > a:focus {
          background-color: #d9ecfa !important;
          color: #123a56 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-menu li.selected .text {
          color: #123a56 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-menu li.selected .check-mark {
          color: #1f78b4 !important;
          opacity: 1 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-toggle .filter-option {
          font-weight: 600;
        }

        body.live-update-on #cpue_apply_filters,
        body.live-update-on #lf_apply_filters,
        body.live-update-on #wf_apply_filters,
        body.live-update-on #lik_apply_filters,
        body.live-update-on #harvest_apply_filters,
        body.live-update-on #tag_apply_filters,
        body.live-update-on #fishery_process_apply_filters,
        body.live-update-on #population_biology_apply_filters,
        body.live-update-on #stock_apply_filters {
          display: none !important;
        }
        .btn.apply-pending {
          background-color: #f39c12 !important;
          border-color: #d58512 !important;
          color: #fff !important;
          box-shadow: 0 0 0 3px rgba(243, 156, 18, 0.2);
        }
        .btn.apply-pending:hover,
        .btn.apply-pending:focus {
          background-color: #e08e0b !important;
          border-color: #c5760a !important;
          color: #fff !important;
        }
        .btn.apply-disabled,
        .btn.apply-disabled:hover,
        .btn.apply-disabled:focus {
          background-color: #c7cdd4 !important;
          border-color: #b5bcc4 !important;
          color: #6b7280 !important;
          box-shadow: none !important;
          cursor: not-allowed !important;
          opacity: 0.95 !important;
        }
        .plot-loading-container {
          position: relative;
        }
        .plot-loading-overlay {
          display: none;
          position: absolute;
          inset: 0;
          z-index: 20;
          align-items: center;
          justify-content: center;
          background: rgba(255, 255, 255, 0.78);
          backdrop-filter: blur(1px);
          pointer-events: none;
        }
        .plot-loading-container.is-loading .plot-loading-overlay {
          display: flex;
        }
        .plot-loading-container > .recalculating {
          opacity: 0.38;
        }
        .plot-loading-container > .recalculating ~ .plot-loading-overlay {
          display: flex;
        }
        .plot-loading-card {
          padding: 10px 14px;
          border-radius: 8px;
          background: rgba(24, 44, 72, 0.92);
          color: #fff;
          box-shadow: 0 8px 18px rgba(0, 0, 0, 0.18);
          font-weight: 700;
          letter-spacing: 0.2px;
        }
        .plot-loading-card .render-spinner {
          display: inline-block;
          width: 14px;
          height: 14px;
          margin-right: 8px;
          border: 2px solid rgba(255, 255, 255, 0.35);
          border-top-color: #fff;
          border-radius: 50%;
          animation: render-spin 0.9s linear infinite;
          vertical-align: -2px;
        }

        #initial-render-overlay {
          display: none;
          position: fixed;
          top: 18px;
          right: 18px;
          z-index: 9999;
          min-width: 260px;
          max-width: 360px;
          padding: 12px 14px;
          background: rgba(24, 44, 72, 0.94);
          color: #fff;
          border-radius: 8px;
          box-shadow: 0 10px 24px rgba(0, 0, 0, 0.22);
          border-left: 4px solid #3c8dbc;
        }

        #initial-render-overlay .render-title {
          font-weight: 700;
          margin-bottom: 4px;
        }

        #initial-render-overlay .render-subtitle {
          font-size: 12px;
          opacity: 0.92;
        }

        #initial-render-overlay .render-spinner {
          display: inline-block;
          width: 14px;
          height: 14px;
          margin-right: 8px;
          border: 2px solid rgba(255, 255, 255, 0.35);
          border-top-color: #fff;
          border-radius: 50%;
          animation: render-spin 0.9s linear infinite;
          vertical-align: -2px;
        }

        body.initial-rendering #initial-render-overlay {
          display: block;
        }

        @keyframes render-spin {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }
      "))
    ),
    tags$head(
      tags$script(HTML("
        (function() {
          var lastCheckedByGroup = {};

          function getGroupElement(el) {
            return el.closest('.dataTables_wrapper, .box, .well, .tab-pane, .sidebar, .modal-content') || document.body;
          }

          function getGroupKey(group) {
            if (group === document.body) return 'body';
            if (group.id) return group.id;
            if (!group.dataset.shiftGroupKey) {
              group.dataset.shiftGroupKey = 'shift-group-' + Math.random().toString(36).slice(2);
            }
            return group.dataset.shiftGroupKey;
          }

          document.addEventListener('click', function(event) {
            var target = event.target;
            if (!(target instanceof HTMLInputElement) || target.type !== 'checkbox' || target.disabled) {
              return;
            }

            var group = getGroupElement(target);
            var groupKey = getGroupKey(group);
            var checkboxes = Array.prototype.filter.call(
              group.querySelectorAll('input[type=\"checkbox\"]'),
              function(box) {
                return !box.disabled && box.offsetParent !== null;
              }
            );

            if (event.shiftKey && lastCheckedByGroup[groupKey]) {
              var start = checkboxes.indexOf(target);
              var end = checkboxes.indexOf(lastCheckedByGroup[groupKey]);

              if (start !== -1 && end !== -1) {
                var checkedState = target.checked;
                checkboxes.slice(Math.min(start, end), Math.max(start, end) + 1).forEach(function(box) {
                  if (box.checked !== checkedState) {
                    box.checked = checkedState;
                    box.dispatchEvent(new Event('change', { bubbles: true }));
                  }
                });
              }
            }

            lastCheckedByGroup[groupKey] = target;
          }, true);
        })();

        (function() {
          var lastPickerIndexById = {};

          function findRelatedSelect(picker) {
            if (!picker || !picker.parentElement) {
              return null;
            }
            return picker.parentElement.querySelector('select');
          }

          document.addEventListener('click', function(event) {
            var optionLink = event.target.closest('.bootstrap-select .dropdown-menu li a');
            if (!optionLink) {
              return;
            }

            var li = optionLink.closest('li');
            var picker = optionLink.closest('.bootstrap-select');
            var select = picker ? findRelatedSelect(picker) : null;

            if (!li || !picker || !select || !select.multiple) {
              return;
            }

            var visibleItems = Array.prototype.filter.call(
              picker.querySelectorAll('.dropdown-menu li'),
              function(item) {
                return !item.classList.contains('divider') &&
                  !item.classList.contains('dropdown-header') &&
                  !item.classList.contains('hidden') &&
                  item.querySelector('a');
              }
            );

            var currentIndex = visibleItems.indexOf(li);
            if (currentIndex === -1) {
              return;
            }

            var selectId = select.id || select.name || 'picker-default';
            var lastIndex = lastPickerIndexById[selectId];
            lastPickerIndexById[selectId] = currentIndex;

            if (!event.shiftKey || typeof lastIndex !== 'number' || lastIndex < 0 || lastIndex >= visibleItems.length) {
              return;
            }

            window.setTimeout(function() {
              var clickedSelected = li.classList.contains('selected');
              var start = Math.min(lastIndex, currentIndex);
              var end = Math.max(lastIndex, currentIndex);

              for (var idx = start; idx <= end; idx++) {
                if (idx === currentIndex) {
                  continue;
                }

                var item = visibleItems[idx];
                var itemAnchor = item.querySelector('a');
                if (!itemAnchor) {
                  continue;
                }

                var isSelected = item.classList.contains('selected');
                if (isSelected !== clickedSelected) {
                  itemAnchor.dispatchEvent(new MouseEvent('click', {
                    bubbles: true,
                    cancelable: true,
                    view: window
                  }));
                }
              }
            }, 0);
          }, true);
        })();

        Shiny.addCustomMessageHandler('triggerApplyButtons', function(ids) {
          if (!Array.isArray(ids)) {
            ids = ids ? [ids] : [];
          }
          ids.forEach(function(id) {
            var el = document.getElementById(id);
            if (el) {
              el.click();
            }
          });
        });

        Shiny.addCustomMessageHandler('setApplyPending', function(payload) {
          if (!payload || !payload.id) return;
          var el = document.getElementById(payload.id);
          if (!el) return;
          var pending = !!payload.pending;
          el.classList.toggle('apply-pending', pending);
          el.classList.toggle('apply-disabled', !pending);
          el.disabled = !pending;
          el.setAttribute('aria-disabled', (!pending).toString());
          el.setAttribute(
            'title',
            pending ? 'Selections changed. Click Apply to refresh this plot.' : 'Selections already applied.'
          );
        });

        Shiny.addCustomMessageHandler('toggleInitialRenderOverlay', function(active) {
          document.body.classList.toggle('initial-rendering', !!active);
        });

        (function() {
          function setLoadingState(outputId, loading) {
            var container = document.querySelector('.plot-loading-container[data-output-id=\"' + outputId + '\"]');
            if (!container) return;
            container.classList.toggle('is-loading', !!loading);
          }

          if (window.jQuery) {
            $(document).on('shiny:outputinvalidated', function(event) {
              if (event && event.name) {
                setLoadingState(event.name, true);
              }
            });

            $(document).on('shiny:value', function(event) {
              if (event && event.name) {
                setLoadingState(event.name, false);
              }
            });

            $(document).on('shiny:error', function(event) {
              if (event && event.name) {
                setLoadingState(event.name, false);
              }
            });
          }
        })();

        (function() {
          function updateLiveUpdateClass() {
            var checkbox = document.getElementById('live_update_plots');
            var enabled = !!(checkbox && checkbox.checked);
            document.body.classList.toggle('live-update-on', enabled);
          }

          document.addEventListener('change', function(event) {
            if (event.target && event.target.id === 'live_update_plots') {
              updateLiveUpdateClass();
            }
          }, true);

          document.addEventListener('shiny:connected', function() {
            setTimeout(updateLiveUpdateClass, 0);
          });

          document.addEventListener('shiny:value', function(event) {
            if (event.name === 'live_update_plots') {
              updateLiveUpdateClass();
            }
          });

          var observer = new MutationObserver(function() {
            updateLiveUpdateClass();
          });

          observer.observe(document.documentElement, {
            childList: true,
            subtree: true
          });
        })();
      "))
    ),
    
    tabItems(
      
      # -----------------------------------------------------------------------
      # TAB 1: MODEL SUMMARY
      # -----------------------------------------------------------------------
      mod_summary_ui(),
      # -----------------------------------------------------------------------
      # TAB 2: FISHERY NAMES EDITOR
      # -----------------------------------------------------------------------
      mod_fishery_names_ui(),
      # -----------------------------------------------------------------------
      # TAB 3: BOUND HITS
      # -----------------------------------------------------------------------
      mod_bounds_ui(),
      # -----------------------------------------------------------------------
      # TAB 4: CPUE FITS
      # -----------------------------------------------------------------------
      mod_cpue_ui(),
      # -----------------------------------------------------------------------
      # TAB 5: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_lf_ui(),
      # -----------------------------------------------------------------------
      # TAB 6: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_wf_ui(),
      # -----------------------------------------------------------------------
      # TAB 7: DIAGNOSTICS
      # -----------------------------------------------------------------------
      mod_likelihood_ui(),
      # -----------------------------------------------------------------------
      # TAB 8: KEY QUANTITIES
      # -----------------------------------------------------------------------
      mod_harvest_ui(),
      # -----------------------------------------------------------------------
      # TAB 9: TAGGING DYNAMICS
      # -----------------------------------------------------------------------
      mod_tagging_ui(),
      # -----------------------------------------------------------------------
      # TAB 10: FISHERY PROCESS DYNAMICS
      # -----------------------------------------------------------------------
      mod_fishery_process_ui(),
      # -----------------------------------------------------------------------
      # TAB 11: POPULATION BIOLOGY
      # -----------------------------------------------------------------------
      mod_population_biology_ui()
    ),
    div(
      id = "initial-render-overlay",
      tags$div(class = "render-title", HTML("<span class='render-spinner'></span>Rendering plots")),
      tags$div(class = "render-subtitle", "Initial plots are being prepared across tabs.")
    )
  )
)

# =============================================================================
