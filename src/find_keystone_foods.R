#!/usr/bin/env Rscript
# find_keystone_foods.R
# Reads `food_centrality.csv` and `bipartite_food_metrics.csv`,
# standardizes numeric metrics and computes a combined keystone score.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

usage <- function() {
  cat("Usage: Rscript src/find_keystone_foods.R [--centrality=path] [--bipartite=path] [--top=N] [--output=path]\n")
  cat("Defaults: --centrality=food_centrality.csv --bipartite=bipartite_food_metrics.csv --top=10 --output=keystone_foods.csv\n")
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(
    centrality = "food_centrality.csv",
    bipartite = "bipartite_food_metrics.csv",
    top = 10L,
    output = "keystone_foods.csv"
  )
  if (length(args) == 0) return(opts)
  for (a in args) {
    if (a %in% c("-h", "--help")) {
      usage(); quit(status = 0)
    }
    if (grepl("^--centrality=", a)) opts$centrality <- sub("^--centrality=", "", a)
    else if (grepl("^--bipartite=", a)) opts$bipartite <- sub("^--bipartite=", "", a)
    else if (grepl("^--top=", a)) opts$top <- as.integer(sub("^--top=", "", a))
    else if (grepl("^--output=", a)) opts$output <- sub("^--output=", "", a)
    else {
      cat("Unknown arg:", a, "\n")
      usage(); quit(status = 1)
    }
  }
  opts
}

opts <- parse_args()

cat(sprintf("Reading centrality from '%s' and bipartite metrics from '%s'\n", opts$centrality, opts$bipartite))

if (!file.exists(opts$centrality)) stop("Centrality file not found: ", opts$centrality)
if (!file.exists(opts$bipartite)) stop("Bipartite file not found: ", opts$bipartite)

centrality <- read_csv(opts$centrality, show_col_types = FALSE)
bip <- read_csv(opts$bipartite, show_col_types = FALSE)

# Normalize column names for consistent merging
centrality <- centrality %>% rename_with(~ str_trim(.x))
bip <- bip %>% rename_with(~ str_trim(.x))

# Determine the key columns: centrality uses `food`, bipartite uses `node`
if ("food" %in% names(centrality)) {
  central_key <- "food"
} else if ("node" %in% names(centrality)) {
  central_key <- "node"
  centrality <- centrality %>% rename(food = node)
} else stop("Could not find a key column in centrality CSV (expected 'food' or 'node')")

if ("node" %in% names(bip)) {
  bip <- bip %>% rename(food = node)
} else if (!"food" %in% names(bip)) {
  stop("Could not find a key column in bipartite CSV (expected 'node' or 'food')")
}

# Trim and normalize food strings to help matching
centrality <- centrality %>% mutate(food = str_trim(as.character(food)))
bip <- bip %>% mutate(food = str_trim(as.character(food)))

# Merge datasets (full join to include any food present in either file)
merged <- full_join(centrality, bip, by = "food") %>%
  arrange(desc(ifelse(!is.na(degree.x), degree.x, 0)))

# After join, identify numeric columns to include in scoring (exclude the food column)
numeric_cols <- merged %>% select(-food) %>% select(where(is.numeric)) %>% names()

if (length(numeric_cols) == 0) {
  stop("No numeric metric columns found to compute keystone score.")
}

# Compute z-scores for each numeric metric (skip metrics with zero variance)
z_df <- merged %>% select(all_of(numeric_cols)) %>% mutate(across(everything(), ~ {
  if (is.numeric(.x) && sd(.x, na.rm = TRUE) > 0) {
    (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  } else {
    NA_real_
  }
}))

# Combine into an overall keystone score: mean of available z-scores (higher = more keystone)
merged$keystone_score <- apply(z_df, 1, function(row) {
  if (all(is.na(row))) return(NA_real_)
  mean(row[!is.na(row)], na.rm = TRUE)
})

# Rank foods by keystone_score (descending). Keep original metrics for context.
result <- merged %>%
  mutate(keystone_rank = dense_rank(desc(keystone_score))) %>%
  arrange(keystone_rank)

# Output top N
top_n <- opts$top
top_n <- ifelse(is.na(top_n) || top_n <= 0, 10L, top_n)
top_df <- result %>% slice_min(order_by = keystone_rank, n = top_n)

cat(sprintf("Writing %d foods (full table) with keystone scores to '%s'\n", nrow(result), opts$output))
write_csv(result %>% relocate(food, keystone_score, keystone_rank), opts$output)

cat("Top keystone foods:\n")
print(top_df %>% select(food, keystone_score, keystone_rank))

invisible(result)
