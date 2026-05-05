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

input_change_first_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- trimws(as.character(x[[1]]))
  if (is.na(txt) || !nzchar(txt)) return(default)
  txt
}

input_change_operation_labels <- function(meta) {
  if (!is.list(meta) || is.null(meta$operations) || !is.list(meta$operations)) {
    return(character(0))
  }
  labels <- vapply(meta$operations, function(op) {
    input_change_first_text(op$label)
  }, character(1))
  unique(labels[nzchar(labels)])
}

input_change_metadata_description <- function(meta) {
  explicit <- input_change_first_text(meta$description)
  if (nzchar(explicit)) return(explicit)

  labels <- input_change_operation_labels(meta)
  if (length(labels) > 0) return(paste(labels, collapse = "; "))

  tokens <- normalize_input_change_tokens(meta$tokens)
  if (length(tokens) > 0) return(paste(tokens, collapse = " + "))

  ""
}

update_input_change_summary_fields <- function(meta) {
  if (!is.list(meta)) meta <- list(version = 1L, tokens = character(0), operations = list())
  meta$tokens <- normalize_input_change_tokens(meta$tokens)
  if (is.null(meta$operations) || !is.list(meta$operations)) meta$operations <- list()

  labels <- input_change_operation_labels(meta)
  meta$labels <- labels
  if (!nzchar(input_change_first_text(meta$label)) && length(meta$tokens) > 0) {
    meta$label <- paste(meta$tokens, collapse = " + ")
  }
  if (!nzchar(input_change_first_text(meta$description)) && length(labels) > 0) {
    meta$description <- paste(labels, collapse = "; ")
  }
  meta
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
  update_input_change_summary_fields(meta)
}

write_input_change_metadata <- function(input_dir, meta) {
  if (!dir.exists(input_dir)) stop("Input directory does not exist: ", input_dir)
  meta <- update_input_change_summary_fields(meta)
  saveRDS(meta, input_change_metadata_file(input_dir), compress = "xz")
  invisible(meta)
}

set_input_change_description <- function(input_dir, description = NULL, label = NULL) {
  meta <- read_input_change_metadata(input_dir)
  if (!is.null(label) && nzchar(input_change_first_text(label))) {
    meta$label <- input_change_first_text(label)
  }
  if (!is.null(description) && nzchar(input_change_first_text(description))) {
    meta$description <- input_change_first_text(description)
  }
  meta$updated_at <- as.character(Sys.time())
  write_input_change_metadata(input_dir, meta)
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
  labels <- input_change_operation_labels(meta)
  if (length(labels) > 0) {
    meta$labels <- labels
    meta$description <- paste(labels, collapse = "; ")
  }

  write_input_change_metadata(input_dir, meta)
}

extract_input_change_tokens <- function(input_dir) {
  read_input_change_metadata(input_dir)$tokens
}

extract_input_change_description <- function(input_dir) {
  input_change_metadata_description(read_input_change_metadata(input_dir))
}
