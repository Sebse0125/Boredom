# Clean and harmonize LimeSurvey participant metadata (checkpoint 1).
#this file was created with several checkpoints and audit files to ensure validity
#"deidentified" with removed researcher names (column) were used to simplyfy file structure and ensure privacy of (former) students
#
# Inputs:
#   data/inter/limesurvey/limesurvey_2024_deidentified.csv
#   data/inter/limesurvey/limesurvey_2026_deidentified.csv
#   data/processed/psychopy/master/psychopy_master.csv
#
# Outputs:
#   data/inter/limesurvey/limesurvey_participant_checkpoint.csv
#   data/inter/limesurvey/limesurvey_participant_audit.csv
#   data/inter/limesurvey/limesurvey_cleaned.csv
#   data/inter/limesurvey/limesurvey_rating_audit.csv
#   data/inter/limesurvey/limesurvey_rating_missingness.csv
#
# The final PsychoPy/LimeSurvey merge is intentionally a later checkpoint.

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

#input files and select directories
input_dir <- "data/inter/limesurvey"
output_dir <- "data/inter/limesurvey"
psychopy_master_path <- "data/processed/psychopy/master/psychopy_master.csv"

survey_paths <- c(
  `2024` = file.path(input_dir, "limesurvey_2024_deidentified.csv"),
  `2026` = file.path(input_dir, "limesurvey_2026_deidentified.csv")
)

#audit and checkpoint files
checkpoint_path <- file.path(output_dir, "limesurvey_participant_checkpoint.csv")
audit_path <- file.path(output_dir, "limesurvey_participant_audit.csv")
cleaned_path <- file.path(output_dir, "limesurvey_cleaned.csv")
rating_audit_path <- file.path(output_dir, "limesurvey_rating_audit.csv")
missingness_path <- file.path(output_dir, "limesurvey_rating_missingness.csv")
final_validation_path <- file.path(output_dir, "limesurvey_final_validation.csv")
processed_dir <- "data/processed/limesurvey/master"
processed_master_path <- file.path(processed_dir, "limesurvey_master.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


required_columns <- list(
  `2024` = c(
    response_id = "id",
    submitted = "submitdate",
    last_page = "lastpage",
    ID = "VP",
    age = "DEMOage",
    gender = "DEMOgender[DEMOgender]",
    weight_kg = "DEMOweight",
    height_cm = "DEMOheight",
    survey_trial_order = "Counterbalance[order]"
  ),
  `2026` = c(
    response_id = "Antwort ID",
    submitted = "Datum Abgeschickt",
    last_page = "Letzte Seite",
    ID = "Versuchspersonencode",
    age = "Wie alt bist du (in Jahren)?",
    gender = "Was ist dein Geschlecht? [Geschlecht]",
    weight_kg = "Bitte gib dein Gewicht in kg an.",
    height_cm = "Bitte gib deine Größe in cm an.",
    survey_trial_order =
      "Which randomisation order is followed? [tick the order of conditions tested]"
  )
)

# Standard names encode measure, measurement position (A/B), and time point.
# A/B are converted to HB/LB only after the authoritative trial order is known.
rating_columns <- list(
  `2024` = c(
    effort_A_baseline = "CUAbas[SQ001_SQ004]",
    boredom_A_baseline = "CUAbas[SQ002_SQ004]",
    pain_A_baseline = "CUAbas[SQ003_SQ004]",
    effort_A_01 = "CUA1[effort_SQ001]",
    boredom_A_01 = "CUA1[boredom_SQ001]",
    pain_A_01 = "CUA1[pain_SQ001]",
    effort_A_02 = "CUA2[effort_SQ001]",
    boredom_A_02 = "CUA2[boredom_SQ001]",
    pain_A_02 = "CUA2[pain_SQ001]",
    effort_A_03 = "CUA3[effort_SQ001]",
    boredom_A_03 = "CUA3[boredom_SQ001]",
    pain_A_03 = "CUA3[pain_SQ001]",
    effort_A_04 = "CUApos[effort_SQ001]",
    boredom_A_04 = "CUApos[boredom_SQ001]",
    pain_A_04 = "CUApos[pain_SQ001]",
    effort_B_baseline = "CUBbas[effort_SQ001]",
    boredom_B_baseline = "CUBbas[boredom_SQ001]",
    pain_B_baseline = "CUBbas[pain_SQ001]",
    effort_B_01 = "CUB1[effort_SQ001]",
    boredom_B_01 = "CUB1[boreodm_SQ001]",
    pain_B_01 = "CUB1[pain_SQ001]",
    effort_B_02 = "CUB2[effort_SQ001]",
    boredom_B_02 = "CUB2[boredom_SQ001]",
    pain_B_02 = "CUB2[pain_SQ001]",
    effort_B_03 = "CUB3[effort_SQ001]",
    boredom_B_03 = "CUB3[boredom_SQ001]",
    pain_B_03 = "CUB3[pain_SQ001]",
    effort_B_04 = "CUBpos[effort_SQ001]",
    boredom_B_04 = "CUBpos[boredom_SQ001]",
    pain_B_04 = "CUBpos[pain_SQ001]"
  ),
  `2026` = c(
    effort_A_baseline = "Check-up questions Baseline [Anstrengung?][rating]",
    boredom_A_baseline = "Check-up questions Baseline [Langeweile?][rating]",
    pain_A_baseline = "Check-up questions Baseline [Schmerz?][rating]",
    effort_A_01 = "Check-up questions A_1 [Anstrengung?][]",
    boredom_A_01 = "Check-up questions A_1 [Langeweile?][]",
    pain_A_01 = "Check-up questions A_1 [Schmerzen?][]",
    effort_A_02 = "Check-up questions A_2 [Anstrengung?][]",
    boredom_A_02 = "Check-up questions A_2 [Langeweile?][]",
    pain_A_02 = "Check-up questions A_2 [Schmerzen?][]",
    effort_A_03 = "Check-up questions A_3 [Anstrengung?][]",
    boredom_A_03 = "Check-up questions A_3 [Langeweile?][]",
    pain_A_03 = "Check-up questions A_3 [Schmerzen?][]",
    effort_A_04 = "Check-up questions A_4 [Anstrengung?][]",
    boredom_A_04 = "Check-up questions A_4 [Langeweile?][]",
    pain_A_04 = "Check-up questions A_4 [Schmerzen?][]",
    effort_B_baseline = "Check-up questions B Baseline [Anstrengung?][]",
    boredom_B_baseline = "Check-up questions B Baseline [Langeweile?][]",
    pain_B_baseline = "Check-up questions B Baseline [Schmerzen?][]",
    effort_B_01 = "Check-up questions B_1 [Anstrengung?][]",
    boredom_B_01 = "Check-up questions B_1 [Langeweile?][]",
    pain_B_01 = "Check-up questions B_1 [Schmerzen?][]",
    effort_B_02 = "Check-up questions B_2 [Anstrengung?][]",
    boredom_B_02 = "Check-up questions B_2 [Langeweile?][]",
    pain_B_02 = "Check-up questions B_2 [Schmerzen?][]",
    effort_B_03 = "Check-up questions B_3 [Anstrengung?][]",
    boredom_B_03 = "Check-up questions B_3 [Langeweile?][]",
    pain_B_03 = "Check-up questions B_3 [Schmerzen?][]",
    effort_B_04 = "Check-up questions B_4 [Anstrengung?][]",
    boredom_B_04 = "Check-up questions B_4 [Langeweile?][]",
    pain_B_04 = "Check-up questions B_4 [Schmerzen?][]"
  )
)

clean_missing <- function(x) {
  x <- str_squish(as.character(x))
  x[x == "" | str_to_upper(x) == "NA"] <- NA_character_
  x
}

clean_number <- function(x) {
  x <- clean_missing(x)
  suppressWarnings(parse_double(str_replace(x, fixed(","), ".")))
}

clean_id <- function(x) {
  x <- clean_missing(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^[:alnum:]ÄÖÜ]", "")

  # Confirmed cross-source corrections.
  recode(
    x,
    "DO11ÜB30" = "DO11UB30",
    "DI16ÜB31" = "DI16UB31",
    "AN" = "AN07IE11",
    .default = x
  )
}

clean_order <- function(x) {
  x <- clean_missing(x)
  x <- str_to_upper(str_replace_all(x, "[^A-Z]", "_"))
  x <- str_replace_all(x, "_+", "_")
  x <- str_remove(x, "^_")
  str_remove(x, "_$")
}

clean_gender <- function(x) {
  x <- str_to_lower(clean_missing(x))
  case_when(
    x == "männlich" ~ "male",
    x == "weiblich" ~ "female",
    x == "divers" ~ "diverse",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

read_survey_metadata <- function(path, year) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path)
  }

  data <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  column_map <- required_columns[[year]]
  rating_map <- rating_columns[[year]]
  expected_columns <- c(unname(column_map), unname(rating_map))
  missing_columns <- setdiff(expected_columns, names(data))

  if (length(missing_columns)) {
    stop(
      "The ", year, " LimeSurvey file is missing required column(s):\n- ",
      paste(missing_columns, collapse = "\n- ")
    )
  }

  metadata <- data %>%
    transmute(
      survey_year = as.integer(year),
      source_file = basename(path),
      source_row = row_number() + 1L,
      response_id = .data[[column_map[["response_id"]]]],
      submitted = clean_missing(.data[[column_map[["submitted"]]]]),
      last_page = clean_number(.data[[column_map[["last_page"]]]]),
      ID_original = clean_missing(.data[[column_map[["ID"]]]]),
      ID = clean_id(.data[[column_map[["ID"]]]]),
      age = clean_number(.data[[column_map[["age"]]]]),
      gender_original = clean_missing(.data[[column_map[["gender"]]]]),
      gender = clean_gender(.data[[column_map[["gender"]]]]),
      weight_kg = clean_number(.data[[column_map[["weight_kg"]]]]),
      height_cm = clean_number(.data[[column_map[["height_cm"]]]]),
      survey_trial_order_original =
        clean_missing(.data[[column_map[["survey_trial_order"]]]]),
      survey_trial_order =
        clean_order(.data[[column_map[["survey_trial_order"]]]])
    )

  ratings <- data[, unname(rating_map), drop = FALSE] %>%
    as_tibble(.name_repair = "minimal")
  names(ratings) <- names(rating_map)
  ratings <- ratings %>% mutate(across(everything(), clean_missing))

  bind_cols(metadata, ratings)
}

survey_metadata <- imap_dfr(
  survey_paths,
  ~ read_survey_metadata(path = .x, year = .y)
)

if (!file.exists(psychopy_master_path)) {
  stop(
    "PsychoPy master not found: ", psychopy_master_path,
    "\nCreate or copy the approved PsychoPy master before running this checkpoint."
  )
}

psychopy_raw <- read_csv(
  psychopy_master_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  progress = FALSE
)

if (!all(c("ID", "trial_order") %in% names(psychopy_raw))) {
  stop("The PsychoPy master must contain ID and trial_order columns.")
}

psychopy_reference <- psychopy_raw %>%
  transmute(
    ID = clean_id(ID),
    psychopy_trial_order = clean_order(trial_order)
  ) %>%
  distinct()

duplicate_psychopy_ids <- psychopy_reference %>%
  count(ID, name = "n") %>%
  filter(is.na(ID) | n != 1L)

if (nrow(duplicate_psychopy_ids)) {
  stop("The PsychoPy reference contains missing or duplicated IDs.")
}

survey_checked <- survey_metadata %>%
  left_join(psychopy_reference, by = "ID") %>%
  mutate(
    ID_changed = !is.na(ID_original) & ID != str_to_upper(ID_original),
    ID_valid = !is.na(ID) &
      str_detect(ID, "^[[:alpha:]]{2}[0-9]{2}[[:alpha:]]{2}[0-9]{2}$"),
    psychopy_match = !is.na(psychopy_trial_order),
    survey_order_valid = survey_trial_order %in% c("HB_LB", "LB_HB"),
    order_conflict = survey_order_valid & psychopy_match &
      survey_trial_order != psychopy_trial_order,

    # PsychoPy is authoritative whenever a matched order is available.
    trial_order = coalesce(psychopy_trial_order, survey_trial_order),

    inclusion_status = case_when(
      !ID_valid ~ "EXCLUDED",
      !psychopy_match ~ "EXCLUDED",
      TRUE ~ "INCLUDED"
    ),
    audit_issue = case_when(
      !ID_valid ~ "missing, test, pilot, or invalid participant ID",
      !psychopy_match ~ "no matching participant in PsychoPy master",
      order_conflict ~ "survey/PsychoPy order conflict; PsychoPy retained",
      ID_changed ~ "participant ID corrected using confirmed mapping",
      TRUE ~ NA_character_
    ),
    audit_note = case_when(
      ID == "JU20ÜC09" & order_conflict ~
        "PsychoPy order LB_HB is authoritative.",
      ID == "DO11UB30" & ID_changed ~ "LimeSurvey DO11ÜB30 corrected to DO11UB30.",
      ID == "DI16UB31" & ID_changed ~ "LimeSurvey DI16ÜB31 corrected to DI16UB31.",
      ID == "AN07IE11" & ID_changed ~ "Incomplete LimeSurvey ID AN corrected to AN07IE11.",
      TRUE ~ NA_character_
    )
  )

write_csv(
  survey_checked %>%
    select(
      survey_year, source_file, source_row, response_id,
      ID_original, ID, ID_changed, ID_valid, psychopy_match,
      submitted, last_page,
      survey_trial_order_original, survey_trial_order,
      psychopy_trial_order, trial_order, survey_order_valid, order_conflict,
      inclusion_status, audit_issue, audit_note
    ),
  audit_path,
  na = ""
)

included <- survey_checked %>%
  filter(inclusion_status == "INCLUDED")

duplicate_survey_ids <- included %>%
  count(ID, name = "n") %>%
  filter(n != 1L)

if (nrow(duplicate_survey_ids)) {
  stop(
    "Included LimeSurvey rows contain duplicate participant IDs. ",
    "Review ", audit_path, "; no checkpoint was written."
  )
}

participant_checkpoint <- included %>%
  select(
    ID, survey_year, response_id,
    age, gender, gender_original, weight_kg, height_cm,
    trial_order, survey_trial_order, psychopy_trial_order,
    order_conflict, source_file, source_row
  ) %>%
  arrange(ID)

write_csv(participant_checkpoint, checkpoint_path, na = "")

# Checkpoint 2: validate ratings and map measurement positions A/B to HB/LB.
standard_rating_names <- names(rating_columns[["2024"]])

rating_long <- survey_checked %>%
  select(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    all_of(standard_rating_names)
  ) %>%
  pivot_longer(
    cols = all_of(standard_rating_names),
    names_to = c("measure", "position", "timepoint"),
    names_pattern = "^(effort|boredom|pain)_([AB])_(baseline|0[1-4])$",
    values_to = "rating_original",
    values_transform = list(rating_original = as.character)
  ) %>%
  mutate(
    rating_original = clean_missing(rating_original),
    rating_numeric = suppressWarnings(parse_double(rating_original, na = character())),
    rating_missing = is.na(rating_original),
    rating_non_numeric = !rating_missing & is.na(rating_numeric),
    rating_out_of_range = !is.na(rating_numeric) &
      (rating_numeric < 0 | rating_numeric > 10),
    rating = if_else(
      rating_missing | rating_non_numeric | rating_out_of_range,
      NA_real_,
      rating_numeric
    ),
    condition = case_when(
      trial_order == "LB_HB" & position == "A" ~ "LB",
      trial_order == "LB_HB" & position == "B" ~ "HB",
      trial_order == "HB_LB" & position == "A" ~ "HB",
      trial_order == "HB_LB" & position == "B" ~ "LB",
      TRUE ~ NA_character_
    ),
    rating_status = case_when(
      inclusion_status == "EXCLUDED" ~ "EXCLUDED_SOURCE_ROW",
      rating_missing ~ "MISSING",
      rating_non_numeric ~ "INVALID_NON_NUMERIC_TO_NA",
      rating_out_of_range ~ "INVALID_OUT_OF_RANGE_TO_NA",
      TRUE ~ "VALID"
    ),
    rating_note = case_when(
      ID == "KA14RE15" & rating_original == "99" ~
        "99 denotes missing data after LimeSurvey site crash; converted to NA.",
      ID == "BG22OE24" & position == "B" & timepoint == "04" & rating_missing ~
        "Final ratings missing because participant became unwell; retained as NA.",
      ID == "DO13UE03" & position == "B" & rating_missing ~
        "Second measurement-position ratings are unavailable; retained as NA.",
      rating_non_numeric ~ "Non-numeric rating converted to NA.",
      rating_out_of_range ~ "Rating outside the valid 0-10 range converted to NA.",
      TRUE ~ NA_character_
    )
  )

write_csv(
  rating_long %>%
    select(
      survey_year, source_file, source_row, response_id,
      ID_original, ID, inclusion_status, trial_order,
      position, condition, timepoint, measure,
      rating_original, rating, rating_status, rating_note
    ) %>%
    arrange(inclusion_status, ID, condition, timepoint, measure),
  rating_audit_path,
  na = ""
)

rating_wide <- rating_long %>%
  filter(inclusion_status == "INCLUDED", !is.na(condition)) %>%
  select(ID, measure, condition, timepoint, rating) %>%
  pivot_wider(
    names_from = c(measure, condition, timepoint),
    values_from = rating,
    names_glue = "{measure}_{condition}_{timepoint}"
  )

expected_rating_names <- unlist(
  map(c("LB", "HB"), function(condition) {
    unlist(map(c("effort", "boredom", "pain"), function(measure) {
      paste(measure, condition, c("baseline", sprintf("%02d", 1:4)), sep = "_")
    }))
  }),
  use.names = FALSE
)

missing_output_columns <- setdiff(expected_rating_names, names(rating_wide))
if (length(missing_output_columns)) {
  rating_wide[missing_output_columns] <- NA_real_
}

cleaned <- participant_checkpoint %>%
  left_join(rating_wide, by = "ID") %>%
  select(
    ID, survey_year, age, gender, weight_kg, height_cm, trial_order,
    all_of(expected_rating_names),
    response_id, gender_original,
    survey_trial_order, psychopy_trial_order, order_conflict,
    source_file, source_row
  ) %>%
  arrange(ID)

write_csv(cleaned, cleaned_path, na = "")

rating_missingness <- rating_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  group_by(ID, condition) %>%
  summarise(
    expected_ratings = n(),
    available_ratings = sum(!is.na(rating)),
    missing_ratings = sum(is.na(rating)),
    invalid_values_converted_to_na =
      sum(rating_non_numeric | rating_out_of_range),
    review_status = if_else(missing_ratings > 0, "REVIEW_MISSING", "COMPLETE"),
    .groups = "drop"
  ) %>%
  arrange(ID, condition)

write_csv(rating_missingness, missingness_path, na = "")

# Final LimeSurvey-only validation. Expected, documented missing ratings are
# accepted; any additional missing rating blocks review and processed output.
rating_validation <- rating_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  mutate(
    expected_missing = case_when(
      ID == "BG22OE24" & condition == "HB" & timepoint == "04" ~ TRUE,
      ID == "KA14RE15" & condition == "LB" ~ TRUE,
      ID == "DO13UE03" & condition == "HB" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(ID) %>%
  summarise(
    rating_cells = n(),
    missing_ratings = sum(is.na(rating)),
    expected_missing_ratings = sum(is.na(rating) & expected_missing),
    unexpected_missing_ratings = sum(is.na(rating) & !expected_missing),
    documented_missing_not_present = sum(!is.na(rating) & expected_missing),
    invalid_cleaned_rating = any(!is.na(rating) & (rating < 0 | rating > 10)),
    .groups = "drop"
  )

survey_id_counts <- cleaned %>% count(ID, name = "survey_rows")

final_validation <- psychopy_reference %>%
  rename(reference_trial_order = psychopy_trial_order) %>%
  left_join(cleaned, by = "ID") %>%
  left_join(survey_id_counts, by = "ID") %>%
  left_join(rating_validation, by = "ID") %>%
  mutate(
    survey_present = !is.na(survey_year),
    survey_rows = coalesce(survey_rows, 0L),
    ID_valid = str_detect(
      ID,
      "^[[:alpha:]]{2}[0-9]{2}[[:alpha:]]{2}[0-9]{2}$"
    ),
    trial_order_valid = trial_order %in% c("HB_LB", "LB_HB"),
    order_matches_psychopy = !is.na(trial_order) &
      trial_order == reference_trial_order,
    rating_structure_valid = coalesce(rating_cells == 30L, FALSE),
    unexpected_missing_ratings = coalesce(unexpected_missing_ratings, 30L),
    invalid_cleaned_rating = coalesce(invalid_cleaned_rating, TRUE),

    # Broad plausibility flags prompt review but do not alter measurements.
    age_plausible = !is.na(age) & age >= 0 & age <= 120,
    weight_plausible = !is.na(weight_kg) & weight_kg >= 20 & weight_kg <= 300,
    height_plausible = !is.na(height_cm) & height_cm >= 100 & height_cm <= 250,
    gender_recorded = !is.na(gender),
    demographic_review =
      !age_plausible | !weight_plausible | !height_plausible | !gender_recorded,

    critical_problem =
      !survey_present |
      survey_rows != 1L |
      !ID_valid |
      !trial_order_valid |
      !order_matches_psychopy |
      !rating_structure_valid |
      unexpected_missing_ratings > 0L |
      invalid_cleaned_rating,
    validation_status = case_when(
      critical_problem ~ "REVIEW_REQUIRED",
      demographic_review | coalesce(documented_missing_not_present, 0L) > 0L ~
        "VALID_WITH_WARNING",
      coalesce(missing_ratings, 0L) > 0L ~ "VALID_DOCUMENTED_MISSING",
      TRUE ~ "VALID"
    )
  ) %>%
  select(
    ID, validation_status, survey_present, survey_rows,
    ID_valid, trial_order, reference_trial_order,
    trial_order_valid, order_matches_psychopy,
    rating_cells, rating_structure_valid,
    missing_ratings, expected_missing_ratings,
    unexpected_missing_ratings, documented_missing_not_present,
    invalid_cleaned_rating,
    age, age_plausible, gender, gender_recorded,
    weight_kg, weight_plausible, height_cm, height_plausible,
    demographic_review
  ) %>%
  arrange(validation_status, ID)

write_csv(final_validation, final_validation_path, na = "")

if (any(final_validation$validation_status == "REVIEW_REQUIRED")) {
  stop(
    "Final LimeSurvey validation failed. Review ", final_validation_path,
    ". The processed master was not written."
  )
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(cleaned, processed_master_path, na = "")

message("Checkpoint written: ", checkpoint_path)
message("Audit written: ", audit_path)
message("Cleaned condition-mapped ratings written: ", cleaned_path)
message("Rating audit written: ", rating_audit_path)
message("Rating missingness summary written: ", missingness_path)
message("Final validation written: ", final_validation_path)
message("Approved LimeSurvey master written: ", processed_master_path)
message("Included participant rows: ", nrow(included))
message("Excluded/test/pilot rows: ", sum(survey_checked$inclusion_status == "EXCLUDED"))
message("Order conflicts retained from PsychoPy: ", sum(survey_checked$order_conflict, na.rm = TRUE))
message(
  "Included invalid values converted to NA: ",
  sum(
    rating_long$inclusion_status == "INCLUDED" &
      (rating_long$rating_non_numeric | rating_long$rating_out_of_range),
    na.rm = TRUE
  )
)
