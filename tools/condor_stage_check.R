## ============================================================
## Condor staging/setup check
##
## This target verifies the Condor-side checkout, transferred payloads,
## on-demand input recipe build, and fitted-source overlay without running
## MFCL. It is intentionally fast so transfer/setup issues can be diagnosed
## before waiting for a long model run.
## ============================================================

source("tools/input_recipe_runner.R")
source("tools/input_change_metadata.R")
source("tools/fitted_model_source.R")

truthy <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- tolower(trimws(as.character(x[[1]])))
  if (!nzchar(txt)) return(default)
  txt %in% c("1", "true", "yes", "y", "on")
}

first <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- trimws(as.character(x[[1]]))
  if (!nzchar(txt) || is.na(txt)) default else txt
}

report <- character(0)
emit <- function(...) {
  line <- paste(..., collapse = "")
  cat(line, "\n", sep = "")
  report <<- c(report, line)
}

emit_section <- function(title) {
  emit("")
  emit("## ", title)
}

format_file_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (!is.finite(bytes)) return("NA")
  units <- c("B", "KB", "MB", "GB")
  idx <- 1L
  while (bytes >= 1024 && idx < length(units)) {
    bytes <- bytes / 1024
    idx <- idx + 1L
  }
  sprintf("%.1f %s", bytes, units[[idx]])
}

describe_dir <- function(path, max_files = 40L) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  emit("path: ", path)
  if (!dir.exists(path)) {
    emit("exists: no")
    return(invisible(NULL))
  }
  emit("exists: yes")
  files <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  emit("items: ", length(files))
  if (length(files) == 0) return(invisible(NULL))
  info <- file.info(files)
  ord <- order(info$isdir, basename(files), decreasing = FALSE)
  files <- files[ord]
  info <- info[ord, , drop = FALSE]
  n_show <- min(length(files), max_files)
  for (idx in seq_len(n_show)) {
    emit(
      "  ",
      if (isTRUE(info$isdir[[idx]])) "[dir] " else "      ",
      basename(files[[idx]]),
      if (!isTRUE(info$isdir[[idx]])) paste0(" (", format_file_size(info$size[[idx]]), ")") else ""
    )
  }
  if (length(files) > n_show) emit("  ... ", length(files) - n_show, " more")
  invisible(NULL)
}

list_matching <- function(path, pattern) {
  if (!dir.exists(path)) return(character(0))
  list.files(path, pattern = pattern, full.names = TRUE, recursive = FALSE)
}

latest_par <- function(path) {
  pars <- list_matching(path, "\\.par([0-9]+)?$")
  if (length(pars) == 0) return(NA_character_)
  stems <- sub("\\.par[0-9]*$", "", basename(pars))
  nums <- suppressWarnings(as.integer(stems))
  exact <- grepl("^[0-9]+\\.par$", basename(pars))
  info <- file.info(pars)
  ord <- order(
    !exact,
    -ifelse(is.finite(nums), nums, -1L),
    -ifelse(is.finite(as.numeric(info$mtime)), as.numeric(info$mtime), -Inf),
    basename(pars)
  )
  pars[ord][[1]]
}

next_par_name <- function(par_path) {
  if (is.na(par_path) || !file.exists(par_path)) return("01.par")
  stem <- sub("\\.par[0-9]*$", "", basename(par_path))
  num <- suppressWarnings(as.integer(stem))
  if (is.finite(num) && grepl("^[0-9]+$", stem)) {
    width <- max(nchar(stem), 2L)
    paste0(sprintf(paste0("%0", width, "d"), num + 1L), ".par")
  } else {
    "01.par"
  }
}

project_root <- getwd()
base_dir <- first(Sys.getenv("base_dir"), "mfcl/inputs/2023_rep")
model_dir <- first(Sys.getenv("model_dir"), file.path("model", basename(base_dir)))

status <- "ok"
warnings <- character(0)
errors <- character(0)

add_warning <- function(x) {
  warnings <<- c(warnings, x)
  emit("WARNING: ", x)
}

add_error <- function(x) {
  errors <<- c(errors, x)
  status <<- "failed"
  emit("ERROR: ", x)
}

emit("Condor stage check")
emit("time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
emit("project_root: ", normalizePath(project_root, winslash = "/", mustWork = FALSE))
emit("base_dir: ", base_dir)
emit("model_dir: ", model_dir)

emit_section("Transferred Files")
describe_dir("..", max_files = 80L)

env_file <- file.path("..", "job_env.txt")
emit_section("Launcher Environment")
emit("job_env.txt: ", if (file.exists(env_file)) normalizePath(env_file, winslash = "/", mustWork = FALSE) else "missing")
env_fields <- c(
  "base_dir",
  "model_dir",
  "launcher_model_name",
  "build_inputs_on_missing",
  "input_recipe_enabled",
  "input_recipe_base_input_dir",
  "input_recipe_output_dir",
  "input_recipe_fixed_params",
  "input_recipe_movement_pairs",
  "input_recipe_sel_nodes",
  "input_recipe_index_cv_half",
  "fitted_model_source_enabled",
  "fitted_model_bundle",
  "fitted_model_source_dir",
  "auto_run_model_before_dependency",
  "auto_fitted_model_dir",
  "prefer_par_start",
  "program_path",
  "mfcl_commands"
)
for (field in env_fields) {
  emit(field, "=", first(Sys.getenv(field), "<unset>"))
}

emit_section("Repository Checkout")
describe_dir(".", max_files = 40L)
git_head <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
git_branch <- tryCatch(system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
emit("git_branch: ", if (length(git_branch) > 0) git_branch[[1]] else "unknown")
emit("git_head: ", if (length(git_head) > 0) git_head[[1]] else "unknown")

emit_section("Input Availability")
base_abs_before <- if (grepl("^/", base_dir)) base_dir else file.path(project_root, base_dir)
emit("base before recipe: ", normalizePath(base_abs_before, winslash = "/", mustWork = FALSE))
emit("base exists before recipe: ", if (dir.exists(base_abs_before)) "yes" else "no")

base_abs <- tryCatch(
  ensure_input_dir_available(base_dir, project_root = project_root),
  error = function(e) {
    add_error(paste0("Input recipe/build failed: ", conditionMessage(e)))
    base_abs_before
  }
)
base_abs <- normalizePath(base_abs, winslash = "/", mustWork = FALSE)
emit("base after recipe: ", base_abs)
emit("base exists after recipe: ", if (dir.exists(base_abs)) "yes" else "no")
if (!dir.exists(base_abs)) {
  add_error("Base input directory is still missing after recipe check.")
} else {
  describe_dir(base_abs, max_files = 80L)
}

frq <- list_matching(base_abs, "\\.frq$")
ini <- list_matching(base_abs, "\\.ini$")
par <- latest_par(base_abs)
doitall <- file.exists(file.path(base_abs, "doitall.sh"))
indepvar <- file.exists(file.path(base_abs, "indepvar.rpt"))
emit("frq count: ", length(frq), if (length(frq) > 0) paste0(" [", paste(basename(frq), collapse = ", "), "]") else "")
emit("ini count: ", length(ini), if (length(ini) > 0) paste0(" [", paste(basename(ini), collapse = ", "), "]") else "")
emit("latest par: ", if (!is.na(par) && file.exists(par)) basename(par) else "missing")
emit("doitall.sh: ", if (doitall) "yes" else "no")
emit("indepvar.rpt in base: ", if (indepvar) "yes" else "no")

if (length(frq) == 0) add_error("No .frq file found in staged/base input.")
if (length(ini) == 0 && (is.na(par) || !file.exists(par))) {
  add_warning("No .ini or .par found; model run would not have a clear start point.")
}

meta <- tryCatch(read_input_change_metadata(base_abs), error = function(e) NULL)
input_tokens <- character(0)
if (is.list(meta) && length(meta) > 0) {
  input_tokens <- if (!is.null(meta$tokens)) as.character(meta$tokens) else character(0)
  input_tokens <- unique(input_tokens[!is.na(input_tokens) & nzchar(trimws(input_tokens))])
  emit("input tokens: ", if (length(meta$tokens) > 0) paste(meta$tokens, collapse = ", ") else "<none>")
  emit("input description: ", first(meta$description, "<none>"))
}

emit_section("Fitted Source Overlay")
fitted_enabled <- truthy(Sys.getenv("fitted_model_source_enabled", "0"))
bundle <- first(Sys.getenv("fitted_model_bundle"))
source_dir <- first(Sys.getenv("fitted_model_source_dir"))
auto_model <- truthy(Sys.getenv("auto_run_model_before_dependency", "0"))
emit("fitted source enabled: ", if (fitted_enabled) "yes" else "no")
emit("fitted bundle: ", if (nzchar(bundle)) bundle else "<none>")
emit("fitted source dir: ", if (nzchar(source_dir)) source_dir else "<none>")
emit("auto prerequisite model: ", if (auto_model) "yes" else "no")

merged_abs <- ""
merged_par <- NA_character_
if (nzchar(bundle) || nzchar(source_dir)) {
  old_auto <- Sys.getenv("auto_run_model_before_dependency", "")
  Sys.setenv(auto_run_model_before_dependency = "0")
  merged_abs <- tryCatch(
    ensure_fitted_model_source(
      base_dir_abs = base_abs,
      base_dir = base_dir,
      model_dir = model_dir,
      project_root = project_root
    ),
    error = function(e) {
      add_error(paste0("Fitted source overlay failed: ", conditionMessage(e)))
      ""
    }
  )
  Sys.setenv(auto_run_model_before_dependency = old_auto)
  if (nzchar(merged_abs)) {
    emit("merged fitted/base input: ", merged_abs)
    describe_dir(merged_abs, max_files = 80L)
    merged_par <- latest_par(merged_abs)
    emit("merged latest par: ", if (!is.na(merged_par) && file.exists(merged_par)) basename(merged_par) else "missing")
    emit("merged indepvar.rpt: ", if (file.exists(file.path(merged_abs, "indepvar.rpt"))) "yes" else "no")
  }
} else if (isTRUE(auto_model)) {
  add_warning("No fitted source bundle was transferred; dependent jobs would run the prerequisite model first. Stage check does not run MFCL.")
} else if (isTRUE(fitted_enabled)) {
  add_warning("Fitted source is enabled but no bundle/source was provided.")
} else {
  emit("fitted source overlay: not requested")
}

emit_section("Would Run")
prefer_par_start <- truthy(Sys.getenv("prefer_par_start", "1"), default = TRUE)
recipe_env_enabled <- truthy(Sys.getenv("input_recipe_enabled", "0"), default = FALSE)
fitted_source_active <- truthy(Sys.getenv("fitted_model_source_enabled", "0"), default = FALSE) ||
  nzchar(first(Sys.getenv("fitted_model_bundle", ""))) ||
  nzchar(first(Sys.getenv("fitted_model_source_dir", "")))
allow_par_start <- isTRUE(prefer_par_start) && isTRUE(fitted_source_active)
mfcl_commands_env <- first(Sys.getenv("mfcl_commands"))
run_par <- if (!is.na(merged_par) && file.exists(merged_par)) merged_par else par
mfcl_commands <- if (!is.na(run_par) && file.exists(run_par) && (!nzchar(mfcl_commands_env) || identical(trimws(mfcl_commands_env), "./doitall.sh")) && isTRUE(allow_par_start)) {
  paste("par-start:", basename(run_par), "->", next_par_name(run_par))
} else if (nzchar(mfcl_commands_env)) {
  mfcl_commands_env
} else if (!is.na(run_par) && file.exists(run_par) && isTRUE(allow_par_start)) {
  paste("par-start:", basename(run_par), "->", next_par_name(run_par))
} else {
  "./doitall.sh"
}
emit("MFCL command mode: ", mfcl_commands)
if (!isTRUE(fitted_source_active) && !is.na(run_par) && file.exists(run_par)) {
  emit("par-start note: disabled unless 'Use existing fitted output as source' is selected")
}
emit("MFCL execution: skipped by setup check")

emit_section("Summary")
emit("status: ", status)
if (length(warnings) > 0) emit("warnings: ", paste(warnings, collapse = " | "))
if (length(errors) > 0) emit("errors: ", paste(errors, collapse = " | "))

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(report, file.path(model_dir, "condor_stage_check.txt"), useBytes = TRUE)
saveRDS(
  list(
    status = status,
    warnings = warnings,
    errors = errors,
    project_root = project_root,
    base_dir = base_dir,
    base_abs = base_abs,
    model_dir = model_dir,
    fitted_merged_base = merged_abs,
    checked_at = Sys.time()
  ),
  file = file.path(model_dir, "condor_stage_check.rds"),
  compress = "xz"
)

emit("wrote: ", file.path(model_dir, "condor_stage_check.txt"))
emit("wrote: ", file.path(model_dir, "condor_stage_check.rds"))

if (identical(status, "failed")) quit(status = 1L)
