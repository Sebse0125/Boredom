library(readr)
library(dplyr)
library(tidyr)

# Setting file directories
input_path <- "data/processed/master/condition_masterfile.csv"
output_dir <- "analysis/files/effort"

all_timepoints_path <- file.path(output_dir, "effort_all_timepoints_long.csv")
trials_path <- file.path(output_dir, "effort_trials_long.csv")
audit_path <- file.path(output_dir, "effort_preparation_audit.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading the condition-level master file
effort_source <- read_csv(
  input_path,
  show_col_types = FALSE
)

# Checking that all required columns exist
required_columns <- c(
  "ID", "Condition", "trial_order", "effort_baseline",
  "effort_01", "effort_02", "effort_03", "effort_04"
)

missing_columns <- setdiff(required_columns, names(effort_source))

if (length(missing_columns) > 0) {
  stop(
    "Required column(s) missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Keeping only variables required for the effort analysis
effort_wide <- effort_source %>%
  select(all_of(required_columns))

# Checking that each participant has only one row per condition
duplicate_rows <- effort_wide %>%
  count(ID, Condition, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  stop("Duplicate ID and Condition combinations were found.")
}

condition_counts <- effort_wide %>%
  distinct(ID, Condition) %>%
  count(ID, name = "n_conditions")

if (any(is.na(effort_wide$ID)) ||
    any(is.na(effort_wide$Condition)) ||
    any(condition_counts$n_conditions != 2L)) {
  stop("Every participant must have exactly one HB row and one LB row.")
}

# Checking condition labels and the valid 0-10 effort range
invalid_conditions <- setdiff(
  unique(na.omit(effort_wide$Condition)),
  c("HB", "LB")
)

if (length(invalid_conditions) > 0) {
  stop(
    "Unexpected Condition value(s): ",
    paste(invalid_conditions, collapse = ", ")
  )
}

effort_columns <- c(
  "effort_baseline", "effort_01", "effort_02",
  "effort_03", "effort_04"
)

invalid_ratings <- effort_wide %>%
  pivot_longer(
    cols = all_of(effort_columns),
    names_to = "source_column",
    values_to = "Effort"
  ) %>%
  filter(!is.na(Effort) & (Effort < 0 | Effort > 10))

# Creating an audit of valid and missing ratings
effort_audit <- effort_wide %>%
  transmute(
    ID,
    Condition,
    baseline_missing = is.na(effort_baseline),
    valid_trial_ratings = rowSums(!is.na(across(all_of(effort_columns[-1])))),
    missing_trial_ratings = 4L - valid_trial_ratings,
    complete_trials = missing_trial_ratings == 0L
  ) %>%
  arrange(ID, Condition)

write_csv(effort_audit, audit_path, na = "")

if (nrow(invalid_ratings) > 0) {
  stop(
    "Effort ratings outside the valid 0-10 range were found. ",
    "Review the source data before continuing."
  )
}

# Creating long data containing baseline and Trials 1-4
# This version is intended for descriptive tables and trajectory figures.
effort_all_timepoints <- effort_wide %>%
  pivot_longer(
    cols = all_of(effort_columns),
    names_to = "Timepoint",
    values_to = "Effort"
  ) %>%
  mutate(
    Timepoint = recode(
      Timepoint,
      effort_baseline = "Baseline",
      effort_01 = "Trial 1",
      effort_02 = "Trial 2",
      effort_03 = "Trial 3",
      effort_04 = "Trial 4"
    ),
    Timepoint_number = recode(
      Timepoint,
      Baseline = 0L,
      `Trial 1` = 1L,
      `Trial 2` = 2L,
      `Trial 3` = 3L,
      `Trial 4` = 4L
    )
  ) %>%
  arrange(ID, Condition, Timepoint_number)

# Creating trial-only data for the primary mixed model
# Baseline remains available as a condition-specific adjustment variable.
effort_trials <- effort_wide %>%
  rename(baseline_effort = effort_baseline) %>%
  pivot_longer(
    cols = starts_with("effort_"),
    names_to = "Trial",
    names_prefix = "effort_",
    values_to = "Effort"
  ) %>%
  mutate(Trial = as.integer(Trial)) %>%
  arrange(ID, Condition, Trial)

# Saving prepared analysis files
write_csv(effort_all_timepoints, all_timepoints_path, na = "")
write_csv(effort_trials, trials_path, na = "")

#writing all-timepoint file and trial-level -trial level for statistical analysis without baseline and all-timepoint for figures with baseline data
message("All-timepoint effort data written: ", all_timepoints_path)
message("Trial-level effort data written: ", trials_path)
message("Preparation audit written: ", audit_path)
