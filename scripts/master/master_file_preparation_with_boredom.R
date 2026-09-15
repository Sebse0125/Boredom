# Prepare standalone final datasets and expanded boredom/attention masters.
#
# Inputs:
#   data/processed/balance/merged/data_merged.csv
#   data/processed/psychopy/master/psychopy_master.csv
#   data/processed/limesurvey/master/limesurvey_master_with_boredom.csv
#
# Outputs:
#   data/processed/master/balance_master.csv
#   data/processed/master/psychopy_final.csv
#   data/processed/master/limesurvey_final_with_boredom.csv
#   data/processed/master/master_with_boredom.csv
#   data/processed/master/condition_masterfile.csv
#   data/processed/master/master_id_mapping_audit_with_boredom.csv
#   data/processed/master/master_merge_audit_with_boredom.csv
#   data/processed/master/master_correction_audit.csv

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

balance_path <- "data/processed/balance/merged/data_merged.csv"
psychopy_path <- "data/processed/psychopy/master/psychopy_master.csv"
limesurvey_path <- paste0(
  "data/processed/limesurvey/master/",
  "limesurvey_master_with_boredom.csv"
)
output_dir <- "data/processed/master"

balance_output_path <- file.path(output_dir, "balance_master.csv")
psychopy_output_path <- file.path(output_dir, "psychopy_final.csv")
limesurvey_output_path <- file.path(
  output_dir,
  "limesurvey_final_with_boredom.csv"
)
master_output_path <- file.path(output_dir, "master_with_boredom.csv")
condition_master_output_path <- file.path(output_dir, "condition_masterfile.csv")
id_audit_path <- file.path(
  output_dir,
  "master_id_mapping_audit_with_boredom.csv"
)
merge_audit_path <- file.path(
  output_dir,
  "master_merge_audit_with_boredom.csv"
)
correction_audit_path <- file.path(output_dir, "master_correction_audit.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_source <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " file not found: ", path)
  }

  read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

find_one_column <- function(data, candidates, label) {
  hits <- names(data)[str_to_lower(names(data)) %in% str_to_lower(candidates)]

  if (length(hits) != 1L) {
    stop(
      "Expected exactly one ", label, " column; found ", length(hits),
      ". Candidates: ", paste(candidates, collapse = " | ")
    )
  }

  hits
}

normalize_id <- function(x) {
  x <- str_to_upper(str_squish(as.character(x)))
  x <- str_replace_all(x, c("Ä" = "A", "Ö" = "O", "Ü" = "U"))
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  x[x == "" | x == "NA"] <- NA_character_
  x
}

standardize_id_column <- function(data, source_name) {
  id_column <- find_one_column(
    data,
    candidates = c("ID", "participant_id", "participant", "VP"),
    label = paste(source_name, "ID")
  )

  data %>%
    rename(ID_original = all_of(id_column)) %>%
    mutate(ID = normalize_id(ID_original), .before = 1)
}

check_unique_ids <- function(data, source_name) {
  problems <- data %>%
    count(ID, name = "n") %>%
    filter(
      is.na(ID) |
        !str_detect(ID, "^[A-Z]{2}[0-9]{2}[A-Z]{2}[0-9]{2}$") |
        n != 1L
    )

  if (nrow(problems)) {
    stop(
      source_name,
      " contains missing or duplicated normalized IDs. No final files were written."
    )
  }

  invisible(data)
}

create_id_audit <- function(data, source_name, balance_ids) {
  map_dfr(seq_len(nrow(data)), function(i) {
    original <- as.character(data$ID_original[i])
    normalized <- data$ID[i]

    if (is.na(normalized)) {
      return(tibble(
        source = source_name, ID_original = original,
        ID_normalized = NA_character_, balance_ID = NA_character_,
        edit_distance = NA_integer_, match_status = "INVALID_ID"
      ))
    }

    if (normalized %in% balance_ids) {
      return(tibble(
        source = source_name, ID_original = original,
        ID_normalized = normalized, balance_ID = normalized,
        edit_distance = 0L,
        match_status = if_else(original == normalized, "EXACT", "NORMALIZED_EXACT")
      ))
    }

    distances <- as.integer(adist(normalized, balance_ids))
    minimum <- min(distances)
    nearest <- balance_ids[distances == minimum]

    tibble(
      source = source_name,
      ID_original = original,
      ID_normalized = normalized,
      balance_ID = if (length(nearest) == 1L) nearest else paste(nearest, collapse = " | "),
      edit_distance = minimum,
      match_status = case_when(
        minimum <= 1L & length(nearest) == 1L ~ "FUZZY_REVIEW_REQUIRED",
        minimum <= 1L ~ "AMBIGUOUS_FUZZY_REVIEW_REQUIRED",
        TRUE ~ "UNMATCHED"
      )
    )
  })
}

balance_raw <- read_source(balance_path, "Balance")
psychopy_raw <- read_source(psychopy_path, "PsychoPy")
limesurvey_raw <- read_source(limesurvey_path, "LimeSurvey")

# Require the complete expanded LimeSurvey schema before any output is written.
msbs_item_ids <- c("01", "03", "09", "10", "22", "23", "24", "28")
required_sbps_columns <- c(
  sprintf("sbps_%02d", 1:8),
  "sbps_n_valid", "sbps_score_sum", "sbps_score_mean"
)
required_msbs_item_columns <- unlist(
  map(c("LB", "HB"), function(condition) {
    unlist(map(c("baseline", "post"), function(timepoint) {
      paste("msbs", condition, timepoint, msbs_item_ids, sep = "_")
    }))
  }),
  use.names = FALSE
)
required_msbs_score_columns <- unlist(
  map(c("LB", "HB"), function(condition) {
    unlist(map(c("baseline", "post"), function(timepoint) {
      paste0(
        "msbs_", condition, "_", timepoint, "_",
        c("n_valid", "score_sum", "score_mean")
      )
    }))
  }),
  use.names = FALSE
)
required_attention_columns <- unlist(
  map(c("LB", "HB"), function(condition) {
    paste0("attention_counter_", condition, "_", sprintf("%02d", 1:4))
  }),
  use.names = FALSE
)
required_expanded_limesurvey_columns <- c(
  required_sbps_columns,
  required_msbs_item_columns,
  required_msbs_score_columns,
  required_attention_columns
)

missing_expanded_columns <- setdiff(
  required_expanded_limesurvey_columns,
  names(limesurvey_raw)
)
if (length(missing_expanded_columns)) {
  stop(
    "Expanded LimeSurvey master is missing required column(s): ",
    paste(missing_expanded_columns, collapse = " | "),
    ". No final master files were written."
  )
}

balance <- standardize_id_column(balance_raw, "Balance")
psychopy <- standardize_id_column(psychopy_raw, "PsychoPy")
limesurvey <- standardize_id_column(limesurvey_raw, "LimeSurvey")

check_unique_ids(balance, "Balance")
check_unique_ids(psychopy, "PsychoPy")
check_unique_ids(limesurvey, "LimeSurvey")

balance_ids <- balance$ID

# Confirmed participants with PsychoPy/LimeSurvey data but no retained balance
# row. They remain in standalone final files but cannot enter the
# balance-authoritative combined master.
confirmed_source_only_ids <- c("CH10AN25", "CH18EU28", "DO13UE03")

id_audit <- bind_rows(
  create_id_audit(balance, "balance", balance_ids),
  create_id_audit(psychopy, "psychopy", balance_ids),
  create_id_audit(limesurvey, "limesurvey", balance_ids)
) %>%
  mutate(
    confirmed_source_only =
      source != "balance" & ID_normalized %in% confirmed_source_only_ids,
    balance_ID = if_else(confirmed_source_only, NA_character_, balance_ID),
    edit_distance = if_else(confirmed_source_only, NA_integer_, edit_distance),
    match_status = if_else(
      confirmed_source_only,
      "CONFIRMED_SOURCE_ONLY_EXCLUDED_FROM_MASTER",
      match_status
    )
  ) %>%
  arrange(source, match_status, ID_normalized)

write_csv(id_audit, id_audit_path, na = "")

unresolved_matches <- id_audit %>%
  filter(
    source != "balance",
    !match_status %in% c(
      "EXACT",
      "NORMALIZED_EXACT",
      "CONFIRMED_SOURCE_ONLY_EXCLUDED_FROM_MASTER"
    )
  )

if (nrow(unresolved_matches)) {
  stop(
    "At least one PsychoPy/LimeSurvey ID requires fuzzy-match review. Review ",
    id_audit_path,
    ". No standalone final or combined master files were written."
  )
}

# Balance is authoritative for final ID spelling after ASCII normalization.
psychopy <- psychopy %>% select(-ID_original)
limesurvey <- limesurvey %>% select(-ID_original)

# Standardize balance demographic units in column names.
height_column <- find_one_column(
  balance,
  c("height", "height_cm", "height (cm)"),
  "balance height"
)
weight_column <- find_one_column(
  balance,
  c("weight", "weight_kg", "weight (kg)"),
  "balance weight"
)

if (height_column != "height_cm") {
  names(balance)[names(balance) == height_column] <- "height_cm"
}
if (weight_column != "weight_kg") {
  names(balance)[names(balance) == weight_column] <- "weight_kg"
}

height_correction_ids <- c("KO24IE08", "MA25UT20", "AN15AV09", "KA09UH17")

missing_correction_ids <- setdiff(height_correction_ids, balance$ID)
if (length(missing_correction_ids)) {
  stop(
    "Height-correction ID(s) not found in balance data: ",
    paste(missing_correction_ids, collapse = " | ")
  )
}

correction_audit <- balance %>%
  filter(ID %in% height_correction_ids) %>%
  transmute(
    ID,
    variable = "height_cm",
    original_value = as.character(height_cm),
    corrected_value = NA_character_,
    reason = "Entered height is impossible; replaced with NA by confirmed decision."
  ) %>%
  arrange(ID)

balance <- balance %>%
  select(-ID_original)
balance$height_cm[balance$ID %in% height_correction_ids] <- NA

# Remove only the requested administrative fields from the standalone
# LimeSurvey final. `any_of()` tolerates an already-removed field.
limesurvey_final <- limesurvey %>%
  select(-any_of(c(
    "response_id", "gender_original", "survey_trial_order",
    "psychopy_trial_order", "order_conflict", "source_file", "source_row"
  )))

psychopy_final <- psychopy
balance_master <- balance

if ("trial_order" %in% names(limesurvey_final) &
    "trial_order" %in% names(psychopy_final)) {
  order_conflicts <- psychopy_final %>%
    select(ID, psychopy_trial_order = trial_order) %>%
    inner_join(
      limesurvey_final %>% select(ID, limesurvey_trial_order = trial_order),
      by = "ID"
    ) %>%
    filter(
      is.na(psychopy_trial_order) |
        is.na(limesurvey_trial_order) |
        psychopy_trial_order != limesurvey_trial_order
    )

  if (nrow(order_conflicts)) {
    stop(
      "PsychoPy/LimeSurvey trial_order disagreement remains. ",
      "No final files were written."
    )
  }
}

write_csv(correction_audit, correction_audit_path, na = "")
write_csv(balance_master, balance_output_path, na = "")
write_csv(psychopy_final, psychopy_output_path, na = "")
write_csv(limesurvey_final, limesurvey_output_path, na = "")

# Authority rules for the combined master only:
# - Balance: ID and demographics.
# - PsychoPy: trial_order and exact Block/block fields.
demographic_names <- c(
  "age", "age_years", "gender", "sex",
  "height", "height_cm", "height (cm)",
  "weight", "weight_kg", "weight (kg)"
)
order_block_names <- c("trial_order", "order", "block")

limesurvey_join <- limesurvey_final %>%
  select(-any_of(c(demographic_names, order_block_names)))

balance_join <- balance_master %>%
  select(-any_of(order_block_names))

if (!"trial_order" %in% names(psychopy_final)) {
  stop("PsychoPy final data must contain trial_order.")
}

# Preserve authoritative balance names. Prefix only remaining colliding
# non-authoritative columns so that no source value is silently overwritten.
psychopy_collisions <- intersect(
  setdiff(names(psychopy_final), "ID"),
  names(balance_join)
)
if (length(psychopy_collisions)) {
  psychopy_final_for_join <- psychopy_final %>%
    rename_with(~ paste0("psychopy_", .x), all_of(psychopy_collisions))
} else {
  psychopy_final_for_join <- psychopy_final
}

names_before_lime <- union(names(balance_join), names(psychopy_final_for_join))
limesurvey_collisions <- intersect(
  setdiff(names(limesurvey_join), "ID"),
  names_before_lime
)
if (length(limesurvey_collisions)) {
  limesurvey_join <- limesurvey_join %>%
    rename_with(~ paste0("limesurvey_", .x), all_of(limesurvey_collisions))
}

master <- balance_join %>%
  left_join(psychopy_final_for_join, by = "ID") %>%
  left_join(limesurvey_join, by = "ID")

missing_expanded_master_columns <- setdiff(
  required_expanded_limesurvey_columns,
  names(master)
)
if (length(missing_expanded_master_columns)) {
  stop(
    "Expanded LimeSurvey columns were lost or renamed unexpectedly during merge: ",
    paste(missing_expanded_master_columns, collapse = " | ")
  )
}

merge_audit <- tibble(ID = balance_ids) %>%
  mutate(
    balance_present = TRUE,
    psychopy_present = ID %in% psychopy_final$ID,
    limesurvey_present = ID %in% limesurvey_final$ID,
    merge_status = case_when(
      psychopy_present & limesurvey_present ~ "COMPLETE_THREE_SOURCE_MATCH",
      !psychopy_present & !limesurvey_present ~ "BALANCE_ONLY",
      !psychopy_present ~ "MISSING_PSYCHOPY",
      !limesurvey_present ~ "MISSING_LIMESURVEY"
    )
  ) %>%
  bind_rows(
    bind_rows(
      psychopy_final %>% filter(!ID %in% balance_ids) %>% distinct(ID) %>%
        mutate(balance_present = FALSE, psychopy_present = TRUE,
               limesurvey_present = ID %in% limesurvey_final$ID,
               merge_status = "PSYCHOPY_ID_NOT_IN_BALANCE"),
      limesurvey_final %>% filter(!ID %in% balance_ids) %>% distinct(ID) %>%
        mutate(balance_present = FALSE,
               psychopy_present = ID %in% psychopy_final$ID,
               limesurvey_present = TRUE,
               merge_status = "LIMESURVEY_ID_NOT_IN_BALANCE")
    )
  ) %>%
  distinct(ID, .keep_all = TRUE) %>%
  mutate(
    merge_status = if_else(
      ID %in% confirmed_source_only_ids,
      "CONFIRMED_SOURCE_ONLY_EXCLUDED_FROM_MASTER",
      merge_status
    )
  ) %>%
  arrange(merge_status, ID)

if (nrow(master) != nrow(balance_master) || n_distinct(master$ID) != nrow(master)) {
  stop("The merge changed the number or uniqueness of authoritative balance IDs.")
}

write_csv(merge_audit, merge_audit_path, na = "")

# Create a condition-level master: one HB and one LB row per participant.
# Participant-level variables are repeated; the HB/LB token is removed from
# measurement column names and stored in the new Condition column.
condition_pattern <- "(^|[ _])(HB|LB)(_|$)"
condition_columns <- names(master)[
  str_detect(names(master), regex(condition_pattern, ignore_case = FALSE))
]
participant_columns <- setdiff(names(master), condition_columns)

remove_condition_token <- function(column_name) {
  column_name %>%
    str_replace(" (HB|LB)_", "_") %>%
    str_replace("_(HB|LB)_", "_") %>%
    str_replace("_(HB|LB)$", "")
}

condition_column_sets <- map(c("HB", "LB"), function(condition) {
  selected <- condition_columns[
    str_detect(
      condition_columns,
      regex(paste0("(^|[ _])", condition, "(_|$)"))
    )
  ]
  set_names(selected, remove_condition_token(selected))
})
names(condition_column_sets) <- c("HB", "LB")

if (!setequal(names(condition_column_sets$HB), names(condition_column_sets$LB))) {
  stop("HB and LB do not contain the same condition-specific variables.")
}

if (anyDuplicated(names(condition_column_sets$HB)) ||
    anyDuplicated(names(condition_column_sets$LB))) {
  stop("Removing HB/LB from column names would create duplicate variables.")
}

master_by_condition <- map_dfr(c("HB", "LB"), function(condition) {
  source_columns <- unname(condition_column_sets[[condition]])
  output_names <- names(condition_column_sets[[condition]])

  condition_data <- master %>% select(all_of(source_columns))
  names(condition_data) <- output_names

  bind_cols(
    master %>% select(all_of(participant_columns)),
    tibble(Condition = condition),
    condition_data
  )
}) %>%
  relocate(Condition, .after = ID) %>%
  mutate(Condition = factor(Condition, levels = c("HB", "LB"))) %>%
  arrange(ID, Condition) %>%
  mutate(Condition = as.character(Condition))

if (nrow(master_by_condition) != 2L * nrow(master) ||
    n_distinct(master_by_condition$ID, master_by_condition$Condition) !=
      nrow(master_by_condition)) {
  stop("Condition-level reshape did not create exactly one HB and one LB row per ID.")
}

expected_condition_limesurvey_columns <- c(
  required_sbps_columns,
  unique(remove_condition_token(c(
    required_msbs_item_columns,
    required_msbs_score_columns,
    required_attention_columns
  )))
)
missing_condition_limesurvey_columns <- setdiff(
  expected_condition_limesurvey_columns,
  names(master_by_condition)
)
if (length(missing_condition_limesurvey_columns)) {
  stop(
    "Condition master is missing expanded LimeSurvey column(s): ",
    paste(missing_condition_limesurvey_columns, collapse = " | ")
  )
}

condition_counts <- master_by_condition %>%
  count(ID, Condition, name = "n")
if (any(condition_counts$n != 1L) ||
    any(!condition_counts$Condition %in% c("HB", "LB"))) {
  stop("Condition master does not contain one unique HB and LB row per ID.")
}

write_csv(master, master_output_path, na = "")
write_csv(master_by_condition, condition_master_output_path, na = "")

message("Balance final written: ", balance_output_path)
message("PsychoPy final written: ", psychopy_output_path)
message("LimeSurvey final written: ", limesurvey_output_path)
message("Combined master written: ", master_output_path)
message("Condition-level master written: ", condition_master_output_path)
message("ID audit written: ", id_audit_path)
message("Merge audit written: ", merge_audit_path)
message("Correction audit written: ", correction_audit_path)
