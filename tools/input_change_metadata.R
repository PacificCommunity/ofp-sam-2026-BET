input_change_metadata_file <- function(input_dir) {
  file.path(input_dir, "input_change_metadata.rds")
}

normalize_input_change_tokens <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  vals <- unlist(strsplit(paste(as.character(x), collapse = ","), "[,;[:space:]]+", perl = TRUE), use.names = FALSE)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  unique(vals)
}

read_input_change_metadata <- function(input_dir) {
  path <- input_change_metadata_file(input_dir)
  if (!file.exists(path)) {
    return(list(
      version = 1L,
      tokens = character(0),
      operations = list()
    ))
  }

  meta <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(meta)) {
    meta <- list(version = 1L, tokens = character(0), operations = list())
  }
  meta$version <- if (!is.null(meta$version)) meta$version else 1L
  meta$tokens <- normalize_input_change_tokens(meta$tokens)
  if (is.null(meta$operations) || !is.list(meta$operations)) meta$operations <- list()
  meta
}

append_input_change_metadata <- function(input_dir, token, label = NULL, operation = NULL,
                                         source_dir = NULL, details = list()) {
  token <- normalize_input_change_tokens(token)
  if (length(token) == 0) stop("At least one input change token is required.")
  if (!dir.exists(input_dir)) stop("Input directory does not exist: ", input_dir)

  meta <- read_input_change_metadata(input_dir)
  meta$tokens <- unique(c(meta$tokens, token))
  meta$updated_at <- as.character(Sys.time())

  op <- list(
    token = token,
    label = if (!is.null(label) && length(label) > 0) paste(as.character(label), collapse = " ") else paste(token, collapse = " + "),
    operation = if (!is.null(operation) && length(operation) > 0) paste(as.character(operation), collapse = " ") else NA_character_,
    source_dir = if (!is.null(source_dir) && length(source_dir) > 0) paste(as.character(source_dir), collapse = " ") else NA_character_,
    target_dir = input_dir,
    created_at = as.character(Sys.time()),
    details = details
  )
  meta$operations[[length(meta$operations) + 1L]] <- op

  saveRDS(meta, input_change_metadata_file(input_dir), compress = "xz")
  invisible(meta)
}

extract_input_change_tokens <- function(input_dir) {
  read_input_change_metadata(input_dir)$tokens
}
