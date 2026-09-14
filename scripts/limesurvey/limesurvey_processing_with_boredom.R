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
#   data/inter/limesurvey/limesurvey_cleaned_with_boredom.csv
#   data/inter/limesurvey/limesurvey_rating_audit.csv
#   data/inter/limesurvey/limesurvey_rating_missingness.csv
#   data/inter/limesurvey/limesurvey_boredom_item_audit.csv
#   data/inter/limesurvey/limesurvey_boredom_missingness.csv
#   data/inter/limesurvey/limesurvey_attention_counter_audit.csv
#   data/inter/limesurvey/limesurvey_attention_counter_missingness.csv
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
cleaned_path <- file.path(output_dir, "limesurvey_cleaned_with_boredom.csv")
rating_audit_path <- file.path(output_dir, "limesurvey_rating_audit.csv")
missingness_path <- file.path(output_dir, "limesurvey_rating_missingness.csv")
boredom_item_audit_path <- file.path(output_dir, "limesurvey_boredom_item_audit.csv")
boredom_missingness_path <- file.path(output_dir, "limesurvey_boredom_missingness.csv")
attention_audit_path <- file.path(output_dir, "limesurvey_attention_counter_audit.csv")
attention_missingness_path <- file.path(
  output_dir,
  "limesurvey_attention_counter_missingness.csv"
)
final_validation_path <- file.path(
  output_dir,
  "limesurvey_final_validation_with_boredom.csv"
)
processed_dir <- "data/processed/limesurvey/master"
processed_master_path <- file.path(
  processed_dir,
  "limesurvey_master_with_boredom.csv"
)

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

# Cumulative clicker-counter readings recorded after trials 01-04 in each
# measurement position. These are retained as recorded; no differencing is
# performed in this extraction script.
attention_columns <- list(
  `2024` = c(
    attention_A_01 = "countA1",
    attention_A_02 = "countA2",
    attention_A_03 = "countA3",
    attention_A_04 = "countApos",
    attention_B_01 = "countB1",
    attention_B_02 = "countB2",
    attention_B_03 = "countB3",
    attention_B_04 = "countBpos"
  ),
  `2026` = c(
    attention_A_01 = "counts...40",
    attention_A_02 = "counts...44",
    attention_A_03 = "counts...48",
    attention_A_04 = "counts...52",
    attention_B_01 = "counts...77",
    attention_B_02 = "counts...81",
    attention_B_03 = "counts...85",
    attention_B_04 = "counts...89"
  )
)

# Verified 1-based positions in both deidentified LimeSurvey exports. Strict
# header checks below ensure that these positions still contain the intended
# SBPS/MSBS questions before values are extracted.
msbs_item_ids <- c("01", "03", "09", "10", "22", "23", "24", "28")

scale_column_positions <- c(
  set_names(13:20, sprintf("sbps_%02d", 1:8)),
  set_names(21:28, paste0("msbs_A_baseline_", msbs_item_ids)),
  set_names(51:58, paste0("msbs_A_post_", msbs_item_ids)),
  set_names(60:67, paste0("msbs_B_baseline_", msbs_item_ids)),
  set_names(88:95, paste0("msbs_B_post_", msbs_item_ids))
)

expected_2024_scale_headers <- c(
  paste0("SBPS[SBPS", sprintf("%02d", 1:8), "]"),
  paste0("Abas[MSBS", msbs_item_ids, "]"),
  paste0("Apos[MSBS", msbs_item_ids, "]"),
  paste0("MSBSBbas[MSBS", msbs_item_ids, "]"),
  paste0("Bpos[MSBS", msbs_item_ids, "]")
)

validate_and_extract_scale_items <- function(data, year) {
  if (ncol(data) < max(scale_column_positions)) {
    stop("The ", year, " file has too few columns for the verified scale map.")
  }

  selected_headers <- names(data)[unname(scale_column_positions)]

  if (year == "2024" && !identical(selected_headers, expected_2024_scale_headers)) {
    stop("The 2024 SBPS/MSBS headers no longer match the verified scale map.")
  }

  if (year == "2026") {
    sbps_headers <- selected_headers[1:8]
    msbs_groups <- list(
      selected_headers[9:16], selected_headers[17:24],
      selected_headers[25:32], selected_headers[33:40]
    )

    if (!all(str_detect(sbps_headers, fixed("auf dich zutreffen")))) {
      stop("The 2026 SBPS question block is not in the expected columns.")
    }
    if (!all(map_lgl(msbs_groups, ~ all(str_detect(.x, fixed("im Moment")))))) {
      stop("A 2026 MSBS question block is not in the expected columns.")
    }

    # LimeSurvey adds different ...NN suffixes to repeated headers. Removing
    # only that suffix must leave the same eight questions in the same order.
    normalized_groups <- map(msbs_groups, ~ str_remove(.x, "\\.\\.\\.[0-9]+$"))
    if (!all(map_lgl(normalized_groups[-1], ~ identical(.x, normalized_groups[[1]])))) {
      stop("The repeated 2026 MSBS item order differs across assessment times.")
    }
  }

  scale_items <- data[, unname(scale_column_positions), drop = FALSE] %>%
    as_tibble(.name_repair = "minimal")
  names(scale_items) <- names(scale_column_positions)
  scale_items %>% mutate(across(everything(), clean_missing))
}

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
  attention_map <- attention_columns[[year]]
  expected_columns <- c(
    unname(column_map),
    unname(rating_map),
    unname(attention_map)
  )
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

  scale_items <- validate_and_extract_scale_items(data, year)

  attention <- data[, unname(attention_map), drop = FALSE] %>%
    as_tibble(.name_repair = "minimal")
  names(attention) <- names(attention_map)
  attention <- attention %>% mutate(across(everything(), clean_missing))

  bind_cols(metadata, ratings, scale_items, attention)
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

# Checkpoint 2b: extract and score SBPS trait boredom and MSBS state boredom.
standard_scale_names <- names(scale_column_positions)

sbps_long <- survey_checked %>%
  select(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    starts_with("sbps_")
  ) %>%
  pivot_longer(
    cols = starts_with("sbps_"),
    names_to = "item_name",
    values_to = "item_original"
  ) %>%
  transmute(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    scale = "SBPS",
    position = NA_character_,
    condition = NA_character_,
    timepoint = "trait",
    item = str_remove(item_name, "^sbps_"),
    item_original
  )

msbs_long <- survey_checked %>%
  select(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    starts_with("msbs_")
  ) %>%
  pivot_longer(
    cols = starts_with("msbs_"),
    names_to = c("position", "timepoint", "item"),
    names_pattern = "^msbs_([AB])_(baseline|post)_([0-9]{2})$",
    values_to = "item_original"
  ) %>%
  mutate(
    scale = "MSBS",
    condition = case_when(
      trial_order == "LB_HB" & position == "A" ~ "LB",
      trial_order == "LB_HB" & position == "B" ~ "HB",
      trial_order == "HB_LB" & position == "A" ~ "HB",
      trial_order == "HB_LB" & position == "B" ~ "LB",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    scale, position, condition, timepoint, item, item_original
  )

boredom_item_long <- bind_rows(sbps_long, msbs_long) %>%
  mutate(
    item_original = clean_missing(item_original),
    # `parse_number()` correctly converts both "4" and endpoint labels such
    # as "1 - stimme überhaupt nicht zu" to their numeric response.
    item_numeric = suppressWarnings(parse_number(item_original, na = character())),
    item_missing = is.na(item_original),
    item_non_numeric = !item_missing & is.na(item_numeric),
    item_non_integer = !is.na(item_numeric) &
      item_numeric != round(item_numeric),
    item_out_of_range = !is.na(item_numeric) &
      (item_numeric < 1 | item_numeric > 7),
    item_value = if_else(
      item_missing | item_non_numeric | item_non_integer | item_out_of_range,
      NA_real_,
      item_numeric
    ),
    expected_missing =
      ID == "DO13UE03" & scale == "MSBS" &
      condition == "HB" & timepoint == "post",
    item_status = case_when(
      inclusion_status == "EXCLUDED" ~ "EXCLUDED_SOURCE_ROW",
      item_missing & expected_missing ~ "DOCUMENTED_MISSING",
      item_missing ~ "MISSING_REVIEW_REQUIRED",
      item_non_numeric ~ "INVALID_NON_NUMERIC_TO_NA",
      item_non_integer ~ "INVALID_NON_INTEGER_TO_NA",
      item_out_of_range ~ "INVALID_OUT_OF_RANGE_TO_NA",
      TRUE ~ "VALID"
    ),
    item_note = case_when(
      item_missing & expected_missing ~
        "DO13UE03 second-block post MSBS unavailable after measurement failure.",
      item_non_numeric ~ "Non-numeric 1-7 scale response converted to NA.",
      item_non_integer ~ "Non-integer 1-7 scale response converted to NA.",
      item_out_of_range ~ "Response outside the valid 1-7 range converted to NA.",
      TRUE ~ NA_character_
    )
  )

write_csv(
  boredom_item_long %>%
    select(
      survey_year, source_file, source_row, response_id,
      ID_original, ID, inclusion_status, trial_order,
      scale, position, condition, timepoint, item,
      item_original, item_value,
      item_missing, item_non_numeric, item_non_integer, item_out_of_range,
      expected_missing, item_status, item_note
    ) %>%
    arrange(inclusion_status, ID, scale, condition, timepoint, item),
  boredom_item_audit_path,
  na = ""
)

sbps_wide <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED", scale == "SBPS") %>%
  select(ID, item, item_value) %>%
  pivot_wider(
    names_from = item,
    values_from = item_value,
    names_glue = "sbps_{item}"
  )

msbs_wide <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED", scale == "MSBS") %>%
  select(ID, condition, timepoint, item, item_value) %>%
  pivot_wider(
    names_from = c(condition, timepoint, item),
    values_from = item_value,
    names_glue = "msbs_{condition}_{timepoint}_{item}"
  )

score_eight_items <- function(data) {
  data %>%
    summarise(
      n_valid = sum(!is.na(item_value)),
      score_sum = if_else(n_valid == 8L, sum(item_value), NA_real_),
      score_mean = if_else(n_valid == 8L, mean(item_value), NA_real_),
      .groups = "drop"
    )
}

sbps_scores <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED", scale == "SBPS") %>%
  group_by(ID) %>%
  score_eight_items() %>%
  rename(
    sbps_n_valid = n_valid,
    sbps_score_sum = score_sum,
    sbps_score_mean = score_mean
  )

msbs_scores <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED", scale == "MSBS") %>%
  group_by(ID, condition, timepoint) %>%
  score_eight_items() %>%
  pivot_wider(
    names_from = c(condition, timepoint),
    values_from = c(n_valid, score_sum, score_mean),
    names_glue = "msbs_{condition}_{timepoint}_{.value}"
  )

scale_wide <- sbps_wide %>%
  full_join(msbs_wide, by = "ID") %>%
  full_join(sbps_scores, by = "ID") %>%
  full_join(msbs_scores, by = "ID")

expected_sbps_names <- c(
  sprintf("sbps_%02d", 1:8),
  "sbps_n_valid", "sbps_score_sum", "sbps_score_mean"
)
expected_msbs_item_names <- unlist(
  map(c("LB", "HB"), function(condition) {
    unlist(map(c("baseline", "post"), function(timepoint) {
      paste("msbs", condition, timepoint, msbs_item_ids, sep = "_")
    }))
  }),
  use.names = FALSE
)
expected_msbs_score_names <- unlist(
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
expected_boredom_output_names <- c(
  expected_sbps_names,
  expected_msbs_item_names,
  expected_msbs_score_names
)

missing_boredom_output_names <- setdiff(
  expected_boredom_output_names,
  names(scale_wide)
)
if (length(missing_boredom_output_names)) {
  stop(
    "Expected boredom output column(s) were not created: ",
    paste(missing_boredom_output_names, collapse = " | ")
  )
}

boredom_missingness <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  group_by(ID, scale, condition, timepoint) %>%
  summarise(
    expected_items = n(),
    available_items = sum(!is.na(item_value)),
    missing_items = sum(is.na(item_value)),
    documented_missing_items = sum(is.na(item_value) & expected_missing),
    unexpected_missing_items = sum(is.na(item_value) & !expected_missing),
    invalid_values_converted_to_na =
      sum(item_non_numeric | item_non_integer | item_out_of_range),
    score_available = available_items == 8L,
    review_status = case_when(
      unexpected_missing_items > 0L | invalid_values_converted_to_na > 0L ~
        "REVIEW_REQUIRED",
      missing_items > 0L ~ "DOCUMENTED_MISSING",
      TRUE ~ "COMPLETE"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, scale, condition, timepoint)

write_csv(boredom_missingness, boredom_missingness_path, na = "")

# Checkpoint 2c: extract condition-specific cumulative attention-counter values.
standard_attention_names <- names(attention_columns[["2024"]])

attention_long <- survey_checked %>%
  select(
    survey_year, source_file, source_row, response_id,
    ID_original, ID, inclusion_status, trial_order,
    all_of(standard_attention_names)
  ) %>%
  pivot_longer(
    cols = all_of(standard_attention_names),
    names_to = c("position", "trial"),
    names_pattern = "^attention_([AB])_(0[1-4])$",
    values_to = "counter_original"
  ) %>%
  mutate(
    counter_original = clean_missing(counter_original),
    counter_numeric = suppressWarnings(
      parse_double(counter_original, na = character())
    ),
    counter_missing = is.na(counter_original),
    counter_non_numeric = !counter_missing & is.na(counter_numeric),
    counter_non_integer = !is.na(counter_numeric) &
      counter_numeric != round(counter_numeric),
    counter_negative = !is.na(counter_numeric) & counter_numeric < 0,
    condition = case_when(
      trial_order == "LB_HB" & position == "A" ~ "LB",
      trial_order == "LB_HB" & position == "B" ~ "HB",
      trial_order == "HB_LB" & position == "A" ~ "HB",
      trial_order == "HB_LB" & position == "B" ~ "LB",
      TRUE ~ NA_character_
    ),
    documented_99 = ID == "KA14RE15" & counter_numeric == 99,
    documented_missing =
      documented_99 |
      (ID == "DO13UE03" & condition == "HB" & counter_missing),
    counter_value = if_else(
      counter_missing | counter_non_numeric | counter_non_integer |
        counter_negative | documented_99,
      NA_real_,
      counter_numeric
    ),
    counter_status = case_when(
      inclusion_status == "EXCLUDED" ~ "EXCLUDED_SOURCE_ROW",
      documented_99 ~ "DOCUMENTED_99_TO_NA",
      counter_missing & documented_missing ~ "DOCUMENTED_MISSING",
      counter_missing ~ "MISSING_REVIEW_REQUIRED",
      counter_non_numeric ~ "INVALID_NON_NUMERIC_TO_NA",
      counter_non_integer ~ "INVALID_NON_INTEGER_TO_NA",
      counter_negative ~ "INVALID_NEGATIVE_TO_NA",
      TRUE ~ "VALID"
    ),
    counter_note = case_when(
      documented_99 ~
        "KA14RE15 value 99 denotes missing data after survey-site crash.",
      counter_missing & documented_missing ~
        "DO13UE03 HB attention counter unavailable after measurement failure.",
      counter_non_numeric ~ "Non-numeric counter value converted to NA.",
      counter_non_integer ~ "Non-integer counter value converted to NA.",
      counter_negative ~ "Negative counter value converted to NA.",
      TRUE ~ NA_character_
    )
  )

attention_sequence_check <- attention_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  arrange(ID, condition, trial) %>%
  group_by(ID, condition) %>%
  summarise(
    counter_sequence_decreased = {
      available <- counter_value[!is.na(counter_value)]
      length(available) >= 2L && any(diff(available) < 0)
    },
    .groups = "drop"
  )

attention_long <- attention_long %>%
  left_join(attention_sequence_check, by = c("ID", "condition")) %>%
  mutate(
    counter_sequence_decreased = coalesce(counter_sequence_decreased, FALSE),
    counter_note = case_when(
      !is.na(counter_note) ~ counter_note,
      counter_sequence_decreased ~
        "Cumulative counter sequence decreases; retain values and review manually.",
      TRUE ~ NA_character_
    )
  )

write_csv(
  attention_long %>%
    select(
      survey_year, source_file, source_row, response_id,
      ID_original, ID, inclusion_status, trial_order,
      position, condition, trial,
      counter_original, counter_value,
      counter_missing, counter_non_numeric, counter_non_integer,
      counter_negative, documented_missing, counter_sequence_decreased,
      counter_status, counter_note
    ) %>%
    arrange(inclusion_status, ID, condition, trial),
  attention_audit_path,
  na = ""
)

attention_wide <- attention_long %>%
  filter(inclusion_status == "INCLUDED", !is.na(condition)) %>%
  select(ID, condition, trial, counter_value) %>%
  pivot_wider(
    names_from = c(condition, trial),
    values_from = counter_value,
    names_glue = "attention_counter_{condition}_{trial}"
  )

expected_attention_names <- unlist(
  map(c("LB", "HB"), function(condition) {
    paste0("attention_counter_", condition, "_", sprintf("%02d", 1:4))
  }),
  use.names = FALSE
)

missing_attention_output_names <- setdiff(
  expected_attention_names,
  names(attention_wide)
)
if (length(missing_attention_output_names)) {
  stop(
    "Expected attention-counter output column(s) were not created: ",
    paste(missing_attention_output_names, collapse = " | ")
  )
}

attention_missingness <- attention_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  group_by(ID, condition) %>%
  summarise(
    expected_counter_readings = n(),
    available_counter_readings = sum(!is.na(counter_value)),
    missing_counter_readings = sum(is.na(counter_value)),
    documented_missing_readings =
      sum(is.na(counter_value) & documented_missing),
    unexpected_missing_readings =
      sum(is.na(counter_value) & !documented_missing),
    invalid_values_converted_to_na = sum(
      counter_non_numeric | counter_non_integer | counter_negative
    ),
    counter_sequence_decreased = first(counter_sequence_decreased),
    review_status = case_when(
      unexpected_missing_readings > 0L | invalid_values_converted_to_na > 0L ~
        "REVIEW_REQUIRED",
      counter_sequence_decreased ~ "VALID_WITH_SEQUENCE_WARNING",
      missing_counter_readings > 0L ~ "DOCUMENTED_MISSING",
      TRUE ~ "COMPLETE"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, condition)

write_csv(attention_missingness, attention_missingness_path, na = "")

cleaned <- participant_checkpoint %>%
  left_join(rating_wide, by = "ID") %>%
  left_join(scale_wide, by = "ID") %>%
  left_join(attention_wide, by = "ID") %>%
  select(
    ID, survey_year, age, gender, weight_kg, height_cm, trial_order,
    all_of(expected_rating_names),
    all_of(expected_boredom_output_names),
    all_of(expected_attention_names),
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

boredom_validation <- boredom_item_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  group_by(ID) %>%
  summarise(
    boredom_item_cells = n(),
    missing_boredom_items = sum(is.na(item_value)),
    documented_missing_boredom_items =
      sum(is.na(item_value) & expected_missing),
    unexpected_missing_boredom_items =
      sum(is.na(item_value) & !expected_missing),
    invalid_boredom_values_converted_to_na =
      sum(item_non_numeric | item_non_integer | item_out_of_range),
    invalid_cleaned_boredom_item =
      any(!is.na(item_value) & (item_value < 1 | item_value > 7)),
    .groups = "drop"
  )

attention_validation <- attention_long %>%
  filter(inclusion_status == "INCLUDED") %>%
  group_by(ID) %>%
  summarise(
    attention_counter_cells = n(),
    missing_attention_counter_values = sum(is.na(counter_value)),
    documented_missing_attention_values =
      sum(is.na(counter_value) & documented_missing),
    unexpected_missing_attention_values =
      sum(is.na(counter_value) & !documented_missing),
    invalid_attention_values_converted_to_na = sum(
      counter_non_numeric | counter_non_integer | counter_negative
    ),
    invalid_cleaned_attention_value = any(
      !is.na(counter_value) &
        (counter_value < 0 | counter_value != round(counter_value))
    ),
    attention_sequence_warning = any(counter_sequence_decreased),
    .groups = "drop"
  )

survey_id_counts <- cleaned %>% count(ID, name = "survey_rows")

final_validation <- psychopy_reference %>%
  rename(reference_trial_order = psychopy_trial_order) %>%
  left_join(cleaned, by = "ID") %>%
  left_join(survey_id_counts, by = "ID") %>%
  left_join(rating_validation, by = "ID") %>%
  left_join(boredom_validation, by = "ID") %>%
  left_join(attention_validation, by = "ID") %>%
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
    boredom_structure_valid =
      coalesce(boredom_item_cells == 40L, FALSE),
    unexpected_missing_boredom_items =
      coalesce(unexpected_missing_boredom_items, 40L),
    invalid_boredom_values_converted_to_na =
      coalesce(invalid_boredom_values_converted_to_na, 40L),
    invalid_cleaned_boredom_item =
      coalesce(invalid_cleaned_boredom_item, TRUE),
    attention_structure_valid =
      coalesce(attention_counter_cells == 8L, FALSE),
    unexpected_missing_attention_values =
      coalesce(unexpected_missing_attention_values, 8L),
    invalid_attention_values_converted_to_na =
      coalesce(invalid_attention_values_converted_to_na, 8L),
    invalid_cleaned_attention_value =
      coalesce(invalid_cleaned_attention_value, TRUE),

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
      invalid_cleaned_rating |
      !boredom_structure_valid |
      unexpected_missing_boredom_items > 0L |
      invalid_boredom_values_converted_to_na > 0L |
      invalid_cleaned_boredom_item |
      !attention_structure_valid |
      unexpected_missing_attention_values > 0L |
      invalid_attention_values_converted_to_na > 0L |
      invalid_cleaned_attention_value,
    validation_status = case_when(
      critical_problem ~ "REVIEW_REQUIRED",
      demographic_review | coalesce(documented_missing_not_present, 0L) > 0L ~
        "VALID_WITH_WARNING",
      coalesce(attention_sequence_warning, FALSE) ~ "VALID_WITH_WARNING",
      coalesce(missing_ratings, 0L) > 0L |
        coalesce(missing_boredom_items, 0L) > 0L |
        coalesce(missing_attention_counter_values, 0L) > 0L ~
        "VALID_DOCUMENTED_MISSING",
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
    boredom_item_cells, boredom_structure_valid,
    missing_boredom_items, documented_missing_boredom_items,
    unexpected_missing_boredom_items,
    invalid_boredom_values_converted_to_na,
    invalid_cleaned_boredom_item,
    attention_counter_cells, attention_structure_valid,
    missing_attention_counter_values,
    documented_missing_attention_values,
    unexpected_missing_attention_values,
    invalid_attention_values_converted_to_na,
    invalid_cleaned_attention_value,
    attention_sequence_warning,
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
message("Boredom item audit written: ", boredom_item_audit_path)
message("Boredom missingness summary written: ", boredom_missingness_path)
message("Attention counter audit written: ", attention_audit_path)
message("Attention counter missingness written: ", attention_missingness_path)
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
