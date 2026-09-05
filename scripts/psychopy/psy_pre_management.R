# PsychoPy pre-management: extract, correct, validate, and standardize filenames.
# Raw CSV contents are never edited or deleted.

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

source_dir <- "data/s_files/psychopy_working_files"
clean_dir  <- "data/s_files/psychopy"
audit_dir  <- "data/s_files/psychopy_metadata_audit"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  source_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

generated_files <- c(
  "participant_condition_session_summary.csv",
  "expName_summary.csv",
  "condition_patterns.csv",
  "psychopy_metadata_audit.csv",
  "psychopy_participant_completeness.csv"
)
files <- files[!basename(files) %in% generated_files]

id_pattern <- regex(
  "(?<![[:alnum:]])[[:alpha:]]{2}[0-9]{2}[[:alpha:]]{2}[0-9]{2}(?![[:alnum:]])",
  ignore_case = TRUE
)
condition_pattern <- regex(
  "(?<![[:alpha:]])(HB|LB)(?![[:alpha:]])",
  ignore_case = TRUE
)

first_nonempty_ci <- function(data, requested_name) {
  hit <- which(tolower(names(data)) == tolower(requested_name))
  if (length(hit) == 0) return(list(value = NA_character_, n = 0L))

  values <- str_trim(as.character(data[[hit[1]]]))
  values <- unique(values[!is.na(values) & values != ""])
  list(
    value = if (length(values)) values[1] else NA_character_,
    n = length(values)
  )
}

extract_id <- function(x) {
  str_to_upper(str_extract(replace_na(x, ""), id_pattern))
}

extract_condition <- function(x) {
  str_to_upper(str_match(replace_na(x, ""), condition_pattern)[, 2])
}

extract_expname_block <- function(x) {
  str_to_upper(str_match(
    replace_na(x, ""),
    regex("_([AB])$", ignore_case = TRUE)
  )[, 2])
}

extract_block_token <- function(x) {
  str_to_upper(str_match(
    replace_na(x, ""),
    regex("(?:^|_)([AB])(?:_|$)", ignore_case = TRUE)
  )[, 2])
}

collapse_candidates <- function(x) {
  x <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(x)) paste(x, collapse = " | ") else NA_character_
}

has_conflict <- function(x) {
  length(unique(x[!is.na(x) & x != ""])) > 1
}

raw_metadata <- map_dfr(files, function(file) {
  tryCatch({
    data <- read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )

    participant <- first_nonempty_ci(data, "participant")
    condition   <- first_nonempty_ci(data, "condition")
    session     <- first_nonempty_ci(data, "session")
    exp_name    <- first_nonempty_ci(data, "expName")

    tibble(
      source_file = basename(file),
      source_path = file,
      read_status = "OK",
      participant_raw = participant$value,
      condition_raw = condition$value,
      session_raw = session$value,
      expName_raw = exp_name$value,
      participant_distinct = participant$n,
      condition_distinct = condition$n,
      session_distinct = session$n,
      expName_distinct = exp_name$n
    )
  }, error = function(e) {
    tibble(
      source_file = basename(file),
      source_path = file,
      read_status = paste("READ ERROR:", conditionMessage(e)),
      participant_raw = NA_character_,
      condition_raw = NA_character_,
      session_raw = NA_character_,
      expName_raw = NA_character_,
      participant_distinct = NA_integer_,
      condition_distinct = NA_integer_,
      session_distinct = NA_integer_,
      expName_distinct = NA_integer_
    )
  })
}) %>%
  mutate(
    filename_metadata = str_remove(
      source_file,
      regex("_+Balance_and_VR.*$", ignore_case = TRUE)
    ),
    id_filename = extract_id(source_file),
    id_participant = extract_id(participant_raw),
    id_session = extract_id(session_raw),
    condition_column = extract_condition(condition_raw),
    condition_participant = extract_condition(participant_raw),
    condition_session = extract_condition(session_raw),
    condition_filename = extract_condition(filename_metadata),
    block_expName = extract_expname_block(expName_raw),
    block_participant = extract_block_token(participant_raw),
    block_session = extract_block_token(session_raw),
    block_filename = extract_block_token(filename_metadata)
  ) %>%
  rowwise() %>%
  mutate(
    id_candidates = collapse_candidates(c_across(c(
      id_filename, id_participant, id_session
    ))),
    condition_candidates = collapse_candidates(c_across(c(
      condition_column, condition_participant,
      condition_session, condition_filename
    ))),
    block_candidates = collapse_candidates(c_across(c(
      block_expName, block_participant, block_session, block_filename
    ))),
    id_conflict = has_conflict(c_across(c(
      id_filename, id_participant, id_session
    ))),
    condition_conflict = has_conflict(c_across(c(
      condition_column, condition_participant,
      condition_session, condition_filename
    ))),
    block_conflict = has_conflict(c_across(c(
      block_expName, block_participant, block_session, block_filename
    )))
  ) %>%
  ungroup() %>%
  mutate(
    ID_extracted = coalesce(id_filename, id_participant, id_session),
    Condition_extracted = coalesce(
      condition_column,
      condition_participant,
      condition_session,
      condition_filename
    ),
    Block_extracted = coalesce(
      block_expName,
      block_participant,
      block_session,
      block_filename
    ),
    condition_source = case_when(
      !is.na(condition_column) ~ "Condition column",
      !is.na(condition_participant) ~ "participant column",
      !is.na(condition_session) ~ "session column",
      !is.na(condition_filename) ~ "filename",
      TRUE ~ NA_character_
    ),
    block_source = case_when(
      !is.na(block_expName) ~ "expName column",
      !is.na(block_participant) ~ "participant column",
      !is.na(block_session) ~ "session column",
      !is.na(block_filename) ~ "filename",
      TRUE ~ NA_character_
    )
  )

# Approved manual decisions. Overrides change only the manifest and clean name;
# they do not alter the original CSV contents.
decisions <- tribble(
  ~source_file, ~action, ~ID_override, ~Block_override, ~Condition_override, ~decision_note,
  "LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_09h45.48.708.csv",
  "include", NA, "B", NA,
  "System error: expName says A; date/time and paired file confirm Block B.",

  "CH101AN25_A_HB__Balance_and_VR_A_2024-07-01_11h46.18.751.csv",
  "include", "CH10AN25", NA, NA,
  "Malformed ID corrected to CH10AN25.",

  "JU20ÜC09_A_LB_Balance_and_VR_A_2024-07-08_16h25.25.936.csv",
  "include", "JU20ÜC09", NA, NA,
  "participant column says JU20ÜB09; confirmed correct ID is JU20ÜC09.",

  "CA11UE11_B_LB_Balance_and_VR_B_2024-06-26_15h40.43.273.csv",
  "include", "CA11UR11", NA, NA,
  "Confirmed correct ID is CA11UR11.",

  "AN06AN18_A_LB_Balance_and_VR_A_2026-06-17_10h28.55.133.csv",
  "exclude", NA, NA, NA,
  "Incomplete recording; complete 11h01 recording retained.",

  "CA04TU11_Balance_and_VR_B_2026-05-26_09h03.49.193.csv",
  "exclude", NA, NA, NA,
  "Incomplete recording; complete 09h13 recording retained.",

  "CH10AN25_B_LB__Balance_and_VR_B_2024-07-01_12h51.08.713.csv",
"include", NA_character_, "B", NA_character_,
"participant field says Block A; filename, expName, and paired A/HB file confirm Block B."
)

metadata <- raw_metadata %>%
  left_join(decisions, by = "source_file") %>%
  mutate(
    action = coalesce(action, "include"),
    ID = coalesce(ID_override, ID_extracted),
    Block = coalesce(Block_override, Block_extracted),
    Condition = coalesce(Condition_override, Condition_extracted),
    id_source = case_when(
      !is.na(ID_override) ~ "approved manual override",
      !is.na(id_filename) ~ "filename: strict pattern",
      !is.na(id_participant) ~ "participant: strict pattern",
      !is.na(id_session) ~ "session: strict pattern",
      TRUE ~ NA_character_
    ),
    block_source = if_else(
      !is.na(Block_override),
      "approved manual override",
      block_source
    ),
    condition_source = if_else(
      !is.na(Condition_override),
      "approved manual override",
      condition_source
    ),
    standardized_file = if_else(
      action == "include" & !is.na(ID) & !is.na(Block) & !is.na(Condition),
      paste0(ID, "_", Block, "_", Condition, ".csv"),
      NA_character_
    ),
    issue_resolved = (
      (!is.na(ID_override) & id_conflict) |
        (!is.na(Block_override) & block_conflict) |
        (!is.na(Condition_override) & condition_conflict)
    ),
    unresolved_problem = (
      read_status != "OK" |
        (action == "include" & (is.na(ID) | is.na(Block) | is.na(Condition))) |
        (id_conflict & is.na(ID_override)) |
        (condition_conflict & is.na(Condition_override)) |
        (block_conflict & is.na(Block_override)) |
        participant_distinct > 1 |
        condition_distinct > 1 |
        session_distinct > 1 |
        expName_distinct > 1
    ),
    qc_status = case_when(
      action == "exclude" ~ "EXCLUDED",
      unresolved_problem ~ "REVIEW",
      issue_resolved ~ "RESOLVED",
      TRUE ~ "OK"
    )
  )

included <- metadata %>% filter(action == "include")

participant_check <- included %>%
  group_by(ID) %>%
  summarise(
    number_of_files = n(),
    block_A_files = sum(Block == "A", na.rm = TRUE),
    block_B_files = sum(Block == "B", na.rm = TRUE),
    HB_files = sum(Condition == "HB", na.rm = TRUE),
    LB_files = sum(Condition == "LB", na.rm = TRUE),
    complete = (
      number_of_files == 2 &
        block_A_files == 1 & block_B_files == 1 &
        HB_files == 1 & LB_files == 1
    ),
    .groups = "drop"
  ) %>%
  mutate(status = if_else(complete, "COMPLETE", "REVIEW")) %>%
  arrange(status, ID)

validation_errors <- character()
if (nrow(metadata) != 90) {
  validation_errors <- c(validation_errors, "Expected 90 source files.")
}
if (nrow(included) != 88) {
  validation_errors <- c(validation_errors, "Expected 88 included files.")
}
if (n_distinct(included$ID) != 44) {
  validation_errors <- c(validation_errors, "Expected 44 participant IDs.")
}
if (any(included$unresolved_problem)) {
  validation_errors <- c(validation_errors, "At least one included file has an unresolved problem.")
}
if (any(!participant_check$complete)) {
  validation_errors <- c(
    validation_errors,
    "At least one ID does not have exactly one A, one B, one HB, and one LB file."
  )
}
if (anyDuplicated(included$standardized_file)) {
  validation_errors <- c(validation_errors, "Standardized filenames are not unique.")
}

# Always write the audit tables, even if validation stops the copy operation.
write_csv(
  metadata %>% select(
    source_file, standardized_file, action,
    ID, Block, Condition,
    id_source, block_source, condition_source,
    qc_status, decision_note,
    participant_raw, condition_raw, session_raw, expName_raw,
    id_candidates, block_candidates, condition_candidates,
    source_path
  ),
  file.path(audit_dir, "psychopy_metadata_audit.csv")
)
write_csv(
  participant_check,
  file.path(audit_dir, "psychopy_participant_completeness.csv")
)
write_csv(
  decisions,
  file.path(audit_dir, "psychopy_manual_decisions.csv")
)

if (length(validation_errors)) {
  stop(
    "Validation failed; no files were copied:\n- ",
    paste(validation_errors, collapse = "\n- ")
  )
}


# Copy idempotently. Existing files are accepted only when byte-identical.
destination_paths <- file.path(clean_dir, included$standardized_file)
source_hashes <- unname(tools::md5sum(included$source_path))
destination_exists <- file.exists(destination_paths)

if (any(destination_exists)) {
  destination_hashes <- rep(NA_character_, length(destination_paths))
  destination_hashes[destination_exists] <- unname(
    tools::md5sum(destination_paths[destination_exists])
  )

  conflict <- destination_exists & source_hashes != destination_hashes
  if (any(conflict)) {
    stop(
      "Existing clean files differ from their sources; nothing was overwritten:\n",
      paste(destination_paths[conflict], collapse = "\n")
    )
  }
}

needs_copy <- !destination_exists
copy_ok <- rep(TRUE, length(destination_paths))
copy_ok[needs_copy] <- file.copy(
  from = included$source_path[needs_copy],
  to = destination_paths[needs_copy],
  overwrite = FALSE,
  copy.date = TRUE
)

if (!all(copy_ok)) {
  stop("Some files could not be copied: ",
       paste(destination_paths[!copy_ok], collapse = ", "))
}

cat("Source files audited: ", nrow(metadata), "\n", sep = "")
cat("Files excluded: ", sum(metadata$action == "exclude"), "\n", sep = "")
cat("Clean files available: ", length(destination_paths), "\n", sep = "")
cat("Participant IDs: ", n_distinct(included$ID), "\n", sep = "")
cat("Output folder: ", clean_dir, "\n", sep = "")
