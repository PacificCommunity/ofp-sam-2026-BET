source("tools/input_change_metadata.R")

env_or_default <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

input_dir <- env_or_default("input_dir")
tokens <- env_or_default("tokens")
label <- env_or_default("label")
operation <- env_or_default("operation", "manual_stamp")
source_dir <- env_or_default("source_dir")

args <- commandArgs(trailingOnly = TRUE)
for (arg in args) {
  kv <- strsplit(arg, "=", fixed = TRUE)[[1]]
  if (length(kv) < 2) next
  key <- kv[[1]]
  value <- paste(kv[-1], collapse = "=")
  if (identical(key, "--input-dir")) input_dir <- value
  if (identical(key, "--tokens")) tokens <- value
  if (identical(key, "--label")) label <- value
  if (identical(key, "--operation")) operation <- value
  if (identical(key, "--source-dir")) source_dir <- value
}

if (!nzchar(input_dir)) stop("input_dir is required.")
if (!nzchar(tokens)) stop("tokens is required, e.g. tokens='fixM,sel4'.")

input_dir_abs <- if (grepl("^/", input_dir)) input_dir else file.path(getwd(), input_dir)
source_dir_abs <- if (nzchar(source_dir)) {
  if (grepl("^/", source_dir)) source_dir else file.path(getwd(), source_dir)
} else {
  NA_character_
}

meta <- append_input_change_metadata(
  input_dir_abs,
  token = tokens,
  label = if (nzchar(label)) label else tokens,
  operation = operation,
  source_dir = source_dir_abs,
  details = list(stamped_by = "tools/stamp_input_change_metadata.R")
)

cat("Stamped input change metadata:\n")
cat("  input_dir:", input_dir_abs, "\n")
cat("  tokens:", paste(meta$tokens, collapse = ", "), "\n")
