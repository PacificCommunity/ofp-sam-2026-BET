apply_model_defaults <- function(models, defaults = list()) {
  stopifnot(is.list(models))

  defaults <- modifyList(
    list(
      program_path = "mfcl/exe/mfclo64_2023",
      base_dir = "mfcl/inputs/2023_rep",
      Reps = "15 25 25 1000 500 500",
      scalers = paste0(seq(140, 50, by = -5), collapse = " "),
      retro_peels = "1 2 3 4 5",
      jitter_seeds = paste0(1:10, collapse = " "),
      jitter_amount = "0.01",
      nsplit = "5"
    ),
    defaults
  )

  Map(function(model, model_name) {
    program_path <- if (!is.null(model$program_path)) model$program_path else defaults$program_path
    base_dir <- if (!is.null(model$base_dir)) model$base_dir else defaults$base_dir
    reps <- if (!is.null(model$Reps)) model$Reps else defaults$Reps
    scalers <- if (!is.null(model$scalers)) model$scalers else defaults$scalers
    retro_peels <- if (!is.null(model$retro_peels)) model$retro_peels else defaults$retro_peels
    jitter_seeds <- if (!is.null(model$jitter_seeds)) model$jitter_seeds else defaults$jitter_seeds
    jitter_amount <- if (!is.null(model$jitter_amount)) model$jitter_amount else defaults$jitter_amount
    nsplit <- if (!is.null(model$nsplit)) model$nsplit else defaults$nsplit

    if (!identical(model$mfcl_commands, "./doitall.sh")) {
      model$mfcl_commands <- paste(program_path, model$mfcl_commands)
    }

    model$model_dir <- file.path("model", model_name)
    model$base_dir <- base_dir
    model$program_path <- program_path
    model$Reps <- reps
    model$scalers <- scalers
    model$retro_peels <- retro_peels
    model$jitter_seeds <- jitter_seeds
    model$jitter_amount <- jitter_amount
    model$nsplit <- nsplit
    model
  }, models, names(models))
}
