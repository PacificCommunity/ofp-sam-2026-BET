launch_tab_ui <- function() {
  resource_selectize <- function(input_id, label, choices, selected, remove_id, remove_title) {
    fluidRow(
      column(
        10,
        selectizeInput(
          input_id,
          label,
          choices = choices,
          selected = selected,
          options = list(create = TRUE, persist = TRUE)
        )
      ),
      column(
        2,
        tags$label(HTML("&nbsp;")),
        actionButton(
          remove_id,
          NULL,
          icon = icon("trash"),
          title = remove_title,
          class = "btn-default btn-block"
        )
      )
    )
  }

  tabItem(
    tabName = "launch",
    
    fluidRow(
      box(
        title = "Repository Root", status = "primary", solidHeader = TRUE, width = 12,
        div(class = "param-label", "Repo Root Path:"),
        fluidRow(
          column(9,
                 textInput("repo_root", NULL,
                          value = normalizePath(file.path("..", ".."), mustWork = FALSE),
                          placeholder = "/path/to/repo")
          ),
          column(3,
                 actionButton("browse_repo_root", "Browse",
                              class = "btn-default btn-block",
                              icon = icon("folder-open"))
          )
        ),
        div(style = "margin-top: 6px; color: #666; font-size: 12px;",
            "All config/model paths are resolved relative to this root unless absolute.")
      )
    ),
        
    # ---- Config File Loader (inline) ----
    fluidRow(
          box(
            title = "Load Job/Profile Configuration", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(6,
                     actionButton("launch_load_config", "Browse & Load Configuration",
                                  icon = icon("folder-open"),
                                  class = "btn-info btn-lg",
                                  style = "width: 100%; margin-top: 10px; height: 60px; font-size: 16px;")
              ),
              column(6,
                     actionButton("edit_script_rstudio", "Modify Current Script",
                                  icon = icon("edit"),
                                  class = "btn-warning btn-lg",
                                  style = "width: 100%; margin-top: 10px; height: 60px; font-size: 16px;")
              )
            ),
            uiOutput("launch_config_status_ui")
          )
        ),
        
        
        fluidRow(
          box(
            title = "Job Configuration", status = "primary", solidHeader = TRUE, width = 6,
            
            fluidRow(
              column(
                6,
                checkboxInput(
                  "stage_check_only",
                  "Transfer/setup check only (no MFCL)",
                  value = FALSE
                ),
                tags$small(
                  "Checks Condor staging, input recipe build, and fitted-source transfer, then stops before MFCL.",
                  style = "display:block; color:#666; line-height:1.25; margin-top:-8px; margin-bottom:10px;"
                ),
                checkboxInput(
                  "prefer_par_start",
                  "Start model from fitted-source .par when selected",
                  value = TRUE
                ),
                tags$small(
                  "Only trusted fitted-source .par files are used to write the next .par; normal inputs rebuild from ./doitall.sh.",
                  style = "display:block; color:#666; line-height:1.25; margin-top:-8px; margin-bottom:10px;"
                ),
                fluidRow(
                  column(
                    5,
                    checkboxGroupInput("job_types", "Job Types:",
                                       choices = c("Model" = "model",
                                                   "Jitter" = "jitter",
                                                   "Hessian" = "hessian",
                                                   "Retrospective" = "retro",
                                                   "Profile" = "prof",
                                                   "Self-Test" = "selftest"),
                                       selected = NULL)
                  ),
                  column(
                    7,
                    div(
                      class = "job-type-options",
                      conditionalPanel(
                        condition = "input.job_types && input.job_types.indexOf('jitter') !== -1",
                        numericInput(
                          "job_jitter_n",
                          "Jitter runs:",
                          value = 30,
                          min = 1,
                          step = 1
                        )
                      ),
                      conditionalPanel(
                        condition = "input.job_types && input.job_types.indexOf('hessian') !== -1",
                        checkboxInput(
                          "hessian_parallel",
                          "Parallel Hessian",
                          value = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.hessian_parallel",
                          numericInput(
                            "hessian_nsplit",
                            "Hessian parts:",
                            value = 5,
                            min = 1,
                            step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "!input.hessian_parallel",
                          tags$small(
                            "Runs as one Hessian job.",
                            style = "display:block; color:#666; line-height:1.25; margin-top:-6px; margin-bottom:10px;"
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.job_types && input.job_types.indexOf('retro') !== -1",
                        numericInput(
                          "retro_peels_n",
                          "Retrospective peels:",
                          value = 7,
                          min = 1,
                          step = 1
                        )
                      ),
                      conditionalPanel(
                        condition = "input.job_types && input.job_types.indexOf('prof') !== -1",
                        checkboxGroupInput(
                          "profile_components",
                          "Profile Components:",
                          choices = c(
                            "Standard scalar profile" = "standard",
                            "Individual parameter profiles" = "individual",
                            "2D profile" = "prof_2d"
                          ),
                          selected = "standard"
                        ),
                        selectInput(
                          "prof_launch_strategy",
                          "Profile Launch Strategy:",
                          choices = c(
                            "Independent (current)" = "independent",
                            "Sequential from anchor (two chains)" = "seq_anchor_bidir"
                          ),
                          selected = "seq_anchor_bidir"
                        ),
                        conditionalPanel(
                          condition = "input.prof_launch_strategy == 'seq_anchor_bidir'",
                          numericInput("prof_anchor_scalar", "Anchor scalar:", value = 100, min = 1, step = 1)
                        )
                      ),
                      conditionalPanel(
                        condition = "input.job_types && input.job_types.indexOf('selftest') !== -1",
                        numericInput(
                          "selftest_reps_n",
                          "Self-test reps:",
                          value = 20,
                          min = 1,
                          step = 1
                        ),
		                        radioButtons(
	                          "selftest_source_mode",
	                          "Truth source:",
	                          choices = c(
	                            "Existing last .par" = "last_par",
	                            "Run doitall.sh first" = "doitall"
	                          ),
	                          selected = "last_par"
	                        ),
	                        radioButtons(
	                          "selftest_refit_mode",
	                          "Refit mode:",
	                          choices = c(
	                            "Start from last .par" = "last_par",
	                            "Run doitall.sh" = "doitall"
	                          ),
	                          selected = "last_par"
	                        ),
	                        conditionalPanel(
	                          condition = "input.selftest_refit_mode == 'last_par'",
	                        numericInput(
	                          "selftest_refit_fevals",
	                            "Refit fevals from last .par:",
	                          value = 20,
	                          min = 1,
	                          step = 1
	                          )
	                        ),
	                        tags$small(
	                          "Self-test outputs are written under model/<model name>/selftest; native-tag projection length is chosen automatically.",
	                          style = "display:block; color:#666; line-height:1.25; margin-top:-4px; margin-bottom:8px;"
	                        )
	                      )
                    )
                  )
                )
              ),
              column(
                6,
                div(
                  class = "input-recipe-panel",
                  radioButtons(
                    "input_launch_mode",
                    "Input mode:",
                    choices = c(
                      "Run existing input(s)" = "existing",
                      "Build sensitivity input(s) from base" = "sensitivity"
                    ),
                    selected = "existing"
                  ),
                  conditionalPanel(
                    condition = "input.input_launch_mode == 'existing'",
                    selectizeInput(
                      "existing_input_choices",
                      "Existing inputs:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = "Select existing input folder(s)",
                        plugins = list("remove_button"),
                        create = FALSE
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.input_launch_mode == 'sensitivity'",
                    tagList(
                      selectizeInput(
                        "input_recipe_base_input_choice",
                        "Base inputs:",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select base input folder(s)",
                          plugins = list("remove_button"),
                          create = FALSE
                        )
                      ),
                      selectizeInput(
                        "input_recipe_sensitivities",
                        "Sensitivities:",
                        choices = input_sensitivity_choices(),
                        selected = character(0),
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select sensitivities to apply",
                          plugins = list("remove_button"),
                          create = FALSE
                        )
                      ),
                      radioButtons(
                        "input_recipe_expansion",
                        "Expansion:",
                        choices = c(
                          "One-off" = "oneoff",
                          "Factorial" = "factorial"
                        ),
                        selected = "factorial",
                        inline = TRUE
                      ),
                      checkboxInput(
                        "input_recipe_include_base",
                        "Include selected base input(s) in expanded launch set",
                        value = TRUE
                      ),
                      uiOutput("input_recipe_preview_ui")
                    )
                  ),
                  tags$hr(style = "margin: 10px 0;"),
                  checkboxInput(
                    "fitted_model_source_enabled",
                    "Use existing fitted output as source",
                    value = FALSE
                  ),
                  conditionalPanel(
                    condition = "input.fitted_model_source_enabled",
                    tagList(
                      selectizeInput(
                        "fitted_model_source_choice",
                        "Fitted output:",
                        choices = c("Auto-match by launch model name" = "__auto__"),
                        selected = "__auto__",
                        multiple = FALSE,
                        options = list(
                          create = FALSE,
                          placeholder = "Search fitted output with latest .par"
                        )
                      ),
                      uiOutput("fitted_model_source_preview_ui")
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "input.launch_mode == 'condor'",
              textInput("output_dir", "Output Directory:", 
                        value = "quick/test_run")
            ),
            textAreaInput(
              "run_description",
              "Run Description:",
              value = "",
              placeholder = "Describe this launch run (purpose, notes, differences, etc.)",
              rows = 3
            ),

            conditionalPanel(
              condition = "input.launch_mode == 'local_native' || input.launch_mode == 'local_docker'",
              div(
                style = "margin-bottom: 12px; padding: 10px; background: #f7f7f7; border-left: 4px solid #00a65a;",
                strong("Local output location: "),
                "each run writes to the model's configured ",
                tags$code("model_dir"),
                " under this repo."
              )
            ),
            
            selectizeInput(
              "branch",
              "Git Branch:",
              choices = c("main", "develop", "develop_lik"),
              selected = "develop_lik",
              options = list(
                create = TRUE,
                persist = FALSE,
                placeholder = "Select or type a branch"
              )
            ),
            
            div(
              style = "margin-top: 10px; padding: 8px 10px; background: #f4f8fb; border-left: 4px solid #3c8dbc;",
              strong("Estimated Jobs: "),
              textOutput("estimated_jobs_text", inline = TRUE),
              br(),
              tags$small(textOutput("estimated_jobs_breakdown_text", inline = TRUE), style = "color:#666;")
            ),

            div(
              style = "margin-top: 10px; padding: 8px 10px; background: #fffaf0; border-left: 4px solid #f39c12;",
              div(
                style = "display:flex; align-items:center; justify-content:space-between; gap:8px; flex-wrap:wrap;",
                div(
                  strong("Input Readiness: "),
                  textOutput("launch_preflight_summary", inline = TRUE)
                ),
                actionButton("refresh_launch_preflight", "Scan Inputs", class = "btn-default btn-xs", icon = icon("search"))
              ),
              div(style = "margin-top:6px;", DT::DTOutput("launch_preflight_table"))
            ),
            
            actionButton("launch_btn", "Launch Job(s)", 
                         class = "btn-primary btn-launch", 
                         icon = icon("rocket"))
          ),
          
          box(
            title = "Resource Configuration", status = "info", solidHeader = TRUE, width = 6,

            radioButtons(
              "launch_mode",
              "Launch Mode:",
              choices = c(
                "Condor" = "condor",
                "Local Native" = "local_native",
                "Local Docker" = "local_docker"
              ),
              selected = "condor",
              inline = TRUE
            ),
            
            checkboxInput("parallel_launch", "Parallel launch", value = TRUE),
            
            selectInput(
              "launch_parallel_cores",
              "Launch cores:",
              choices = as.character(seq_len(max(1, parallel::detectCores() - 2))),
              selected = as.character(max(1, parallel::detectCores() - 2))
            ),
            
            conditionalPanel(
              condition = "input.launch_mode == 'local_native' || input.launch_mode == 'local_docker'",
              tagList(
                div(
                  style = "margin: 10px 0; padding: 10px; background: #f4f8fb; border-left: 4px solid #3c8dbc;",
                  "Local mode runs the selected job in this repository using the loaded config env."
                ),
                conditionalPanel(
                  condition = "input.launch_mode == 'local_docker'",
                  textInput("local_docker_image", "Docker Image:", 
                            value = "ghcr.io/pacificcommunity/bet-2026:v1.9")
                )
              )
            ),

            conditionalPanel(
              condition = "input.launch_mode == 'condor'",
              tagList(
                resource_selectize(
                  "remote_user",
                  "Remote User:",
                  choices = unique(c(Sys.getenv("USER", "kyuhank"), "kyuhank")),
                  selected = Sys.getenv("USER", "kyuhank"),
                  remove_id = "remove_remote_user_choice",
                  remove_title = "Remove selected remote user from dropdown"
                ),
                
                resource_selectize(
                  "remote_host",
                  "Remote Host:",
                  choices = unique(c(Sys.getenv("NOU_CONDOR", ""))),
                  selected = Sys.getenv("NOU_CONDOR", ""),
                  remove_id = "remove_remote_host_choice",
                  remove_title = "Remove selected remote host from dropdown"
                ),
                
                resource_selectize(
                  "github_username",
                  "GitHub Username:",
                  choices = "kyuhank",
                  selected = "kyuhank",
                  remove_id = "remove_github_username_choice",
                  remove_title = "Remove selected GitHub username from dropdown"
                ),
                
                resource_selectize(
                  "github_org",
                  "GitHub Organization:",
                  choices = "PacificCommunity",
                  selected = "PacificCommunity",
                  remove_id = "remove_github_org_choice",
                  remove_title = "Remove selected GitHub organization from dropdown"
                ),
                
                resource_selectize(
                  "github_repo",
                  "GitHub Repository:",
                  choices = "ofp-sam-2026-bet",
                  selected = "ofp-sam-2026-bet",
                  remove_id = "remove_github_repo_choice",
                  remove_title = "Remove selected GitHub repository from dropdown"
                ),
                
                resource_selectize(
                  "docker_image",
                  "Docker Image:",
                  choices = "ghcr.io/pacificcommunity/bet-2026:v1.9",
                  selected = "ghcr.io/pacificcommunity/bet-2026:v1.9",
                  remove_id = "remove_docker_image_choice",
                  remove_title = "Remove selected Docker image from dropdown"
                ),
                
                checkboxInput("ghcr_login", "GHCR Login (private images)", value = FALSE),
                
                shiny::hr(),
                
                numericInput("condor_cpus", "CPUs:", 
                             value = 2, min = 1, max = 32, step = 1),
                
                numericInput("condor_memory", "Memory (GB):",
                             value = 12, min = 1, max = 128, step = 1),
                
                numericInput("condor_disk", "Disk (GB):",
                             value = 10, min = 1, max = 100, step = 1),

                shiny::hr(),

                selectInput(
                  "condor_run_target",
                  "Run Selected Condor Nodes:",
                  choices = c(
                    "all (no extra filtering)" = "all",
                    "nouofp" = "nouofp",
                    "suvofp" = "suvofp"
                  ),
                  selected = "all"
                )
              )
            )
            
          )
        ),
        
        fluidRow(
          box(
            title = "Launch Log", status = "success", solidHeader = TRUE, width = 12,
            verbatimTextOutput("launch_log")
          )
        )
      )
}
