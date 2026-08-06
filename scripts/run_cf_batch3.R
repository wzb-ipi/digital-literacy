# One-off runner: setup + causal-forest chunks only (Batch 3 Share/Bookmark refit)
# Usage: Rscript scripts/run_cf_batch3.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
script_path <- if (!is.na(file_arg) && nzchar(file_arg)) {
  normalizePath(file_arg)
} else {
  normalizePath("scripts/run_cf_batch3.R")
}
root <- dirname(dirname(script_path))
setwd(root)
message("Working directory: ", getwd())

extract_chunk <- function(path, label) {
  lines <- readLines(path, warn = FALSE)
  start <- which(grepl(paste0("^#\\|\\s*label:\\s*", label, "\\s*$"), lines))
  if (!length(start)) stop("Chunk label not found: ", label)
  # Walk back to opening ```{r}
  open <- start[1]
  while (open > 1L && !grepl("^```\\{r", lines[open])) open <- open - 1L
  if (!grepl("^```\\{r", lines[open])) stop("Could not find fence for: ", label)
  close <- open + which(lines[(open + 1L):length(lines)] == "```")[1]
  if (is.na(close)) stop("Unclosed chunk: ", label)
  # Body after option lines (#| ...)
  body_start <- open + 1L
  while (body_start < close && grepl("^#\\|", lines[body_start])) {
    body_start <- body_start + 1L
  }
  paste(lines[body_start:(close - 1L)], collapse = "\n")
}

eval_chunk <- function(path, label, envir = .GlobalEnv) {
  message("Evaluating chunk: ", label)
  code <- extract_chunk(path, label)
  eval(parse(text = code), envir = envir)
}

qmd <- "2_analysis.qmd"
needed <- c(
  "analysis-setup-packages",
  "analysis-configuration",
  "analysis-helper-functions",
  "data-imports",
  "appendix-causal-forest-cate-corr",
  "appendix-causal-forest-bookmark-report",
  "appendix-causal-forest-watch-like-by-party"
)

for (lab in needed) eval_chunk(qmd, lab)

message("Done. RUN_FORESTS was: ", RUN_FORESTS)
message("N cells: ", if (!is.null(cf_estimates$n_cells)) cf_estimates$n_cells else nrow(df_cate))
message("Outcomes in cate_unit: ", paste(intersect(cf_outcomes_fit, names(df_cate)), collapse = ", "))
