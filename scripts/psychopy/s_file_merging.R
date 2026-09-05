# Merge standardized PsychoPy files into movement-level and participant-level data.
#
# Input:  data/s_files/psychopy/ID_Block_Condition.csv
# Output: data/inter/psychopy/
#
# Raw/standardized source files are read only. The final master is written only
# when filenames, participant pairs, and movement data pass critical validation.

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)

source_dir <- "data/s_files/psychopy"
output_dir <- "data/inter/psychopy"
artifact_threshold_seconds <- 0.01
expected_trials <- sprintf("%02d", 1:4)
movement_domains <- c("arm", "head", "leg", "other")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

validation_path <- file.path(output_dir, "file_validation.csv")
audit_path <- file.path(output_dir, "psychopy_movement_audit.csv")
summary_path <- file.path(output_dir, "psychopy_measurement_summary.csv")
master_path <- file.path(output_dir, "psychopy_master.csv")

collapse_issues <- function(...) {
  issues <- unlist(list(...), use.names = FALSE)
  issues <- unique(issues[!is.na(issues) & issues != ""])
  paste(issues, collapse = "; ")
}

first_nonempty <- function(x) {
  x <- str_trim(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x)) x[1] else NA_character_
}

parse_filename <- function(path) {
  file_name <- basename(path)
  match <- str_match(
    file_name,
    regex(
      "^([[:alpha:]]{2}[0-9]{2}[[:alpha:]]{2}[0-9]{2})_([AB])_(HB|LB)\\.csv$",
      ignore_case = TRUE
    )
  )

  tibble(
    source_file = file_name,
    source_path = path,
    filename_valid = !is.na(match[, 1]),
    ID = str_to_upper(match[, 2]),
    Block = str_to_upper(match[, 3]),
    Condition = str_to_upper(match[, 4])
  )
}

parse_numeric_list <- function(value) {
  if (is.na(value)) {
    return(list(values = numeric(), valid = FALSE, status = "missing time value"))
  }

  value <- str_trim(value)
  if (value == "[]") {
    return(list(values = numeric(), valid = TRUE, status = "OK"))
  }

  if (!str_starts(value, fixed("[")) || !str_ends(value, fixed("]"))) {
    return(list(values = numeric(), valid = FALSE, status = "invalid time-list format"))
  }

  inside <- str_trim(str_sub(value, 2, -2))
  if (inside == "") {
    return(list(values = numeric(), valid = TRUE, status = "OK"))
  }

  pieces <- str_split(inside, fixed(","))[[1]] %>% str_trim()
  values <- suppressWarnings(parse_double(pieces, na = character()))

  if (any(is.na(values))) {
    return(list(values = numeric(), valid = FALSE, status = "non-numeric time entry"))
  }

  list(values = values, valid = TRUE, status = "OK")
}

parse_name_list <- function(value) {
  if (is.na(value)) {
    return(list(
      names = character(), domains = character(),
      valid = FALSE, status = "missing clicked_name value"
    ))
  }

  value <- str_trim(value)
  if (value == "[]") {
    return(list(
      names = character(), domains = character(),
      valid = TRUE, status = "OK"
    ))
  }

  if (!str_starts(value, fixed("[")) || !str_ends(value, fixed("]"))) {
    return(list(
      names = character(), domains = character(),
      valid = FALSE, status = "invalid clicked_name-list format"
    ))
  }

  # PsychoPy stores a comma-separated Python-style string list. Movement
  # component names do not contain commas, so no code evaluation is needed.
  inside <- str_trim(str_sub(value, 2, -2))
  pieces <- str_split(inside, fixed(","))[[1]]
  names <- pieces %>%
    str_trim() %>%
    str_remove_all(fixed("'")) %>%
    str_remove_all(fixed("\"")) %>%
    str_trim()
  names <- names[names != ""]

  if (!length(names)) {
    return(list(
      names = character(), domains = character(),
      valid = FALSE, status = "clicked_name list could not be parsed"
    ))
  }

  domains <- case_when(
    str_detect(names, regex("^Arms?_Block_", ignore_case = TRUE)) ~ "arm",
    str_detect(names, regex("^Head_Block_", ignore_case = TRUE)) ~ "head",
    str_detect(names, regex("^Legs?_Block_", ignore_case = TRUE)) ~ "leg",
    str_detect(names, regex("^Other_Block_", ignore_case = TRUE)) ~ "other",
    TRUE ~ NA_character_
  )

  unknown <- names[is.na(domains)]
  if (length(unknown)) {
    return(list(
      names = names,
      domains = domains,
      valid = FALSE,
      status = paste("unknown movement name(s):", paste(unique(unknown), collapse = " | "))
    ))
  }

  list(names = names, domains = domains, valid = TRUE, status = "OK")
}

find_trial_cell <- function(data, pattern) {
  matching_columns <- names(data)[str_detect(names(data), regex(pattern, ignore_case = TRUE))]

  if (!length(matching_columns)) {
    return(list(
      column = NA_character_, value = NA_character_, valid = FALSE,
      status = "column missing", matching_columns = NA_character_
    ))
  }

  values <- map_chr(matching_columns, ~ first_nonempty(data[[.x]]))
  populated <- which(!is.na(values))

  if (length(populated) == 1) {
    return(list(
      column = matching_columns[populated],
      value = values[populated],
      valid = TRUE,
      status = "OK",
      matching_columns = paste(matching_columns, collapse = " | ")
    ))
  }

  if (length(populated) > 1) {
    return(list(
      column = NA_character_, value = NA_character_, valid = FALSE,
      status = "multiple populated matching columns",
      matching_columns = paste(matching_columns[populated], collapse = " | ")
    ))
  }

  list(
    column = matching_columns[1], value = NA_character_, valid = FALSE,
    status = "matching column contains no value",
    matching_columns = paste(matching_columns, collapse = " | ")
  )
}

empty_trial_result <- function(file_row, trial, issue) {
  tibble(
    source_file = file_row$source_file,
    source_path = file_row$source_path,
    ID = file_row$ID,
    Block = file_row$Block,
    Condition = file_row$Condition,
    trial = trial,
    clicked_name_column = NA_character_,
    time_column = NA_character_,
    internal_column_block = NA_character_,
    block_header_matches_filename = NA,
    raw_clicked_names = NA_character_,
    raw_times = NA_character_,
    raw_name_count = NA_integer_,
    raw_timestamp_count = NA_integer_,
    first_timestamp = NA_real_,
    early_artifact_found = NA,
    removed_movement = NA_character_,
    adjustment_rule = NA_character_,
    adjusted_name_count = NA_integer_,
    adjusted_timestamp_count = NA_integer_,
    unmatched_timestamps = NA_integer_,
    names_without_timestamps = NA_integer_,
    raw_arm = NA_integer_, raw_head = NA_integer_,
    raw_leg = NA_integer_, raw_other = NA_integer_,
    arm = NA_integer_, head = NA_integer_,
    leg = NA_integer_, other = NA_integer_, total = NA_integer_,
    extraction_valid = FALSE,
    audit_status = "ERROR",
    audit_issue = issue
  )
}

extract_trial <- function(data, file_row, trial) {
  name_cell <- find_trial_cell(
    data,
    paste0("^Klicks_Block_[AB]_", trial, "\\.clicked_name$")
  )
  time_cell <- find_trial_cell(
    data,
    paste0("^Klicks_Block_[AB]_", trial, "\\.time$")
  )

  if (!name_cell$valid || !time_cell$valid) {
    return(empty_trial_result(
      file_row,
      trial,
      collapse_issues(
        if (!name_cell$valid) paste("clicked_name:", name_cell$status),
        if (!time_cell$valid) paste("time:", time_cell$status)
      )
    ))
  }

  parsed_names <- parse_name_list(name_cell$value)
  parsed_times <- parse_numeric_list(time_cell$value)

  if (!parsed_names$valid || !parsed_times$valid) {
    return(empty_trial_result(
      file_row,
      trial,
      collapse_issues(
        if (!parsed_names$valid) parsed_names$status,
        if (!parsed_times$valid) parsed_times$status
      )
    ) %>% mutate(
      clicked_name_column = name_cell$column,
      time_column = time_cell$column,
      raw_clicked_names = name_cell$value,
      raw_times = time_cell$value
    ))
  }

  names_raw <- parsed_names$names
  domains_raw <- parsed_names$domains
  times_raw <- parsed_times$values

  first_timestamp <- if (length(times_raw)) times_raw[1] else NA_real_
  early_artifact <- (
    length(times_raw) > 0 &&
      !is.na(first_timestamp) &&
      first_timestamp < artifact_threshold_seconds
  )

  counts_match_before_adjustment <- length(times_raw) == length(names_raw)
  remove_first_name <- early_artifact && counts_match_before_adjustment && length(names_raw) > 0

  times_adjusted <- if (early_artifact) times_raw[-1] else times_raw
  names_adjusted <- if (remove_first_name) names_raw[-1] else names_raw
  domains_adjusted <- if (remove_first_name) domains_raw[-1] else domains_raw

  removed_movement <- if (remove_first_name) domains_raw[1] else NA_character_
  unmatched_timestamps <- max(length(times_adjusted) - length(names_adjusted), 0L)
  names_without_timestamps <- max(length(names_adjusted) - length(times_adjusted), 0L)

  raw_counts <- table(factor(domains_raw, levels = movement_domains))
  adjusted_counts <- table(factor(domains_adjusted, levels = movement_domains))

  internal_block <- str_match(
    name_cell$column,
    regex("^Klicks_Block_([AB])_", ignore_case = TRUE)
  )[, 2] %>% str_to_upper()
  block_matches <- !is.na(file_row$Block) && internal_block == file_row$Block

  adjustment_rule <- case_when(
    early_artifact && remove_first_name ~ "removed early timestamp and paired first movement",
    early_artifact && !remove_first_name ~ "removed early timestamp; retained all movement names",
    TRUE ~ "no early artifact removed"
  )

  warning_issue <- collapse_issues(
    if (!block_matches) "internal A/B header differs from standardized filename",
    if (unmatched_timestamps > 0) {
      paste(unmatched_timestamps, "timestamp(s) cannot be matched to clicked_name")
    },
    if (names_without_timestamps > 0) {
      paste(names_without_timestamps, "clicked_name value(s) lack timestamps")
    },
    if (sum(times_raw < artifact_threshold_seconds, na.rm = TRUE) > 1) {
      "more than one timestamp is below the artifact threshold"
    }
  )

  tibble(
    source_file = file_row$source_file,
    source_path = file_row$source_path,
    ID = file_row$ID,
    Block = file_row$Block,
    Condition = file_row$Condition,
    trial = trial,
    clicked_name_column = name_cell$column,
    time_column = time_cell$column,
    internal_column_block = internal_block,
    block_header_matches_filename = block_matches,
    raw_clicked_names = name_cell$value,
    raw_times = time_cell$value,
    raw_name_count = length(names_raw),
    raw_timestamp_count = length(times_raw),
    first_timestamp = first_timestamp,
    early_artifact_found = early_artifact,
    removed_movement = removed_movement,
    adjustment_rule = adjustment_rule,
    adjusted_name_count = length(names_adjusted),
    adjusted_timestamp_count = length(times_adjusted),
    unmatched_timestamps = unmatched_timestamps,
    names_without_timestamps = names_without_timestamps,
    raw_arm = unname(raw_counts["arm"]),
    raw_head = unname(raw_counts["head"]),
    raw_leg = unname(raw_counts["leg"]),
    raw_other = unname(raw_counts["other"]),
    arm = unname(adjusted_counts["arm"]),
    head = unname(adjusted_counts["head"]),
    leg = unname(adjusted_counts["leg"]),
    other = unname(adjusted_counts["other"]),
    total = length(domains_adjusted),
    extraction_valid = TRUE,
    audit_status = if_else(warning_issue == "", "OK", "WARNING"),
    audit_issue = warning_issue
  )
}

extract_file <- function(file_row) {
  if (!file_row$filename_valid) {
    return(map_dfr(
      expected_trials,
      ~ empty_trial_result(file_row, .x, "invalid standardized filename")
    ))
  }

  data <- tryCatch(
    read_csv(
      file_row$source_path,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ),
    error = function(e) e
  )

  if (inherits(data, "error")) {
    return(map_dfr(
      expected_trials,
      ~ empty_trial_result(
        file_row,
        .x,
        paste("file could not be read:", conditionMessage(data))
      )
    ))
  }

  map_dfr(expected_trials, ~ extract_trial(data, file_row, .x))
}

# -----------------------------------------------------------------------------
# 1. Parse and validate standardized filenames
# -----------------------------------------------------------------------------

files <- list.files(
  source_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

if (!length(files)) {
  stop("No CSV files found in: ", source_dir)
}

file_manifest <- map_dfr(files, parse_filename) %>%
  add_count(ID, Block, Condition, name = "same_measurement_file_count")

participant_design <- file_manifest %>%
  filter(filename_valid) %>%
  group_by(ID) %>%
  summarise(
    ID_file_count = n(),
    A_file_count = sum(Block == "A"),
    B_file_count = sum(Block == "B"),
    HB_file_count = sum(Condition == "HB"),
    LB_file_count = sum(Condition == "LB"),
    design_complete = (
      ID_file_count == 2 &
        A_file_count == 1 & B_file_count == 1 &
        HB_file_count == 1 & LB_file_count == 1
    ),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 2. Extract trial data and apply the <0.01-second artifact rule
# -----------------------------------------------------------------------------

trial_audit <- map_dfr(seq_len(nrow(file_manifest)), function(i) {
  extract_file(file_manifest[i, ])
})

file_extraction <- trial_audit %>%
  group_by(source_file) %>%
  summarise(
    extracted_trial_count = n(),
    valid_trial_count = sum(extraction_valid),
    error_trial_count = sum(audit_status == "ERROR"),
    warning_trial_count = sum(audit_status == "WARNING"),
    extraction_issues = collapse_issues(audit_issue[audit_issue != ""]),
    .groups = "drop"
  )

file_validation <- file_manifest %>%
  left_join(participant_design, by = "ID") %>%
  left_join(file_extraction, by = "source_file") %>%
  rowwise() %>%
  mutate(
    critical_issue = collapse_issues(
      if (!filename_valid) "filename does not match ID_Block_Condition.csv",
      if (filename_valid && same_measurement_file_count > 1) "duplicate ID-Block-Condition file",
      if (filename_valid && !design_complete) {
        "ID does not have exactly one A, one B, one HB, and one LB file"
      },
      if (coalesce(error_trial_count, 4L) > 0) "one or more trials could not be extracted"
    ),
    warning_issue = if (coalesce(warning_trial_count, 0L) > 0) extraction_issues else "",
    validation_status = case_when(
      critical_issue != "" ~ "REVIEW_REQUIRED",
      warning_issue != "" ~ "VALID_WITH_WARNING",
      TRUE ~ "VALID"
    )
  ) %>%
  ungroup() %>%
  arrange(validation_status, ID, Block)

write_csv(file_validation, validation_path)
write_csv(trial_audit, audit_path)

# -----------------------------------------------------------------------------
# 3. Create one row per measurement file (two rows per complete participant)
# -----------------------------------------------------------------------------

measurement_summary <- trial_audit %>%
  select(
    source_file, ID, Block, Condition, trial,
    arm, head, leg, other, total
  ) %>%
  pivot_wider(
    names_from = trial,
    values_from = c(arm, head, leg, other, total),
    names_glue = "{.value}_{trial}"
  ) %>%
  left_join(
    file_validation %>% select(
      source_file, validation_status,
      critical_issue, warning_issue
    ),
    by = "source_file"
  ) %>%
  arrange(ID, Block, Condition)

write_csv(measurement_summary, summary_path)

# Stop after writing review material if a critical problem exists.
critical_files <- file_validation %>% filter(validation_status == "REVIEW_REQUIRED")
if (nrow(critical_files) > 0) {
  stop(
    "Validation found ", nrow(critical_files),
    " file(s) requiring review. Audit outputs were written, but psychopy_master.csv ",
    "was not created. See: ", validation_path
  )
}

# -----------------------------------------------------------------------------
# 4. Create one row per participant with HB/LB columns and condition totals
# -----------------------------------------------------------------------------

trial_value_columns <- names(measurement_summary)[
  str_detect(names(measurement_summary), "^(arm|head|leg|other|total)_[0-9]{2}$")
]

trial_order <- measurement_summary %>%
  group_by(ID) %>%
  summarise(
    condition_A = first_nonempty(Condition[Block == "A"]),
    condition_B = first_nonempty(Condition[Block == "B"]),
    trial_order = paste(condition_A, condition_B, sep = "_"),
    .groups = "drop"
  )

master <- measurement_summary %>%
  select(ID, Condition, all_of(trial_value_columns)) %>%
  pivot_wider(
    names_from = Condition,
    values_from = all_of(trial_value_columns),
    names_glue = "{.value}_{Condition}"
  ) %>%
  rename_with(
    ~ str_replace(.x, "^(arm|head|leg|other|total)_([0-9]{2})_(HB|LB)$", "\\1_\\3_\\2"),
    -ID
  ) %>%
  left_join(trial_order %>% select(ID, trial_order), by = "ID") %>%
  relocate(ID, trial_order)

# Domain totals across trials 01-04 and overall totals within each condition.
for (condition in c("HB", "LB")) {
  for (domain in movement_domains) {
    component_columns <- paste0(domain, "_", condition, "_", expected_trials)
    master[[paste0(domain, "_", condition, "_total")]] <- rowSums(
      master[, component_columns, drop = FALSE],
      na.rm = FALSE
    )
  }

  condition_domain_totals <- paste0(movement_domains, "_", condition, "_total")
  master[[paste0("all_movements_", condition, "_total")]] <- rowSums(
    master[, condition_domain_totals, drop = FALSE],
    na.rm = FALSE
  )
}

master <- master %>% arrange(ID)
write_csv(master, master_path)

cat("Files processed: ", nrow(file_validation), "\n", sep = "")
cat("Participants in master: ", nrow(master), "\n", sep = "")
cat("Files with documented warnings: ",
    sum(file_validation$validation_status == "VALID_WITH_WARNING"), "\n", sep = "")
cat("Outputs written to: ", output_dir, "\n", sep = "")


#==== saving to processed data ====#
#manual inspection deemed file as good, so we now save it to processed files#
processed_dir <- "data/processed/psychopy/master"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  master,
  file.path(processed_dir, "psychopy_master.csv")
)
