# =========================================================
# PSYCHOPY STAGE 1
# METADATA STANDARDISATION + INTERMEDIATE FILE CREATION
#
# INPUT:
#   data/raw/psychopy/2024
#   data/raw/psychopy/2026
#
# OUTPUT:
#   data/meta/psychopy/psychopy_metadata.csv
#   data/meta/psychopy/psychopy_metadata_review.csv
#   data/meta/psychopy/psychopy_metadata_excluded.csv
#
#   data/inter/psychopy/ID_Block_Condition.csv
#
# IMPORTANT:
# Raw PsychoPy files are NEVER modified or deleted.
# =========================================================


library(readr)
library(dplyr)
library(stringr)
library(purrr)


# =========================================================
# 1. PATHS
# =========================================================

input_folders <- c(
  "data/raw/psychopy/2024",
  "data/raw/psychopy/2026"
)

metadata_folder <- "data/meta/psychopy"

intermediate_folder <- "data/inter/psychopy"


metadata_file <- file.path(
  metadata_folder,
  "psychopy_metadata.csv"
)

review_file <- file.path(
  metadata_folder,
  "psychopy_metadata_review.csv"
)

exclusion_file <- file.path(
  metadata_folder,
  "psychopy_metadata_excluded.csv"
)


# =========================================================
# 2. CREATE OUTPUT FOLDERS
# =========================================================

dir.create(
  metadata_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  intermediate_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# =========================================================
# 3. FIND ALL RAW PSYCHOPY FILES
# =========================================================

all_psychopy_files <- unlist(
  lapply(
    input_folders,
    function(folder) {
      
      list.files(
        path = folder,
        pattern = "\\.csv$",
        full.names = TRUE
      )
    }
  )
)


if (length(all_psychopy_files) == 0) {
  
  stop(
    "No PsychoPy CSV files were found."
  )
}


print(
  paste(
    "Raw PsychoPy files found:",
    length(all_psychopy_files)
  )
)


# =========================================================
# 4. KNOWN FILE EXCLUSIONS
#
# These are known failed / restarted measurements.
#
# The raw files remain completely untouched.
# They are only excluded from further processing.
#
# Add future exclusions here if necessary.
# =========================================================

excluded_files <- tibble(
  
  source_file = c(
    
    "AN06AN18_A_LB_Balance_and_VR_A_2026-06-17_11h01.32.147.csv",
    
    "CA04TU11_Balance_and_VR_B_2026-05-26_09h03.49.193.csv"
  ),
  
  exclusion_reason = c(
    
    "Failed measurement; measurement was restarted.",
    
    "Failed measurement; measurement was restarted."
  )
)


# ---------------------------------------------------------
# Build exclusion log
# ---------------------------------------------------------

excluded_psychopy_files <- tibble(
  
  source_path = all_psychopy_files,
  
  source_file = basename(
    all_psychopy_files
  )
  
) %>%
  
  inner_join(
    excluded_files,
    by = "source_file"
  ) %>%
  
  mutate(
    
    source_year = str_extract(
      source_path,
      "20[0-9]{2}"
    ),
    
    .before = exclusion_reason
  )


# ---------------------------------------------------------
# Check whether all listed exclusions were actually found
# ---------------------------------------------------------

missing_exclusions <- excluded_files %>%
  
  filter(
    !source_file %in%
      basename(all_psychopy_files)
  )


if (nrow(missing_exclusions) > 0) {
  
  warning(
    paste0(
      nrow(missing_exclusions),
      " file(s) listed in excluded_files were not found."
    )
  )
  
  print(
    missing_exclusions
  )
}


# ---------------------------------------------------------
# Remove known failed files from processing list
# ---------------------------------------------------------

psychopy_files <- all_psychopy_files[
  
  !basename(all_psychopy_files) %in%
    excluded_files$source_file
]


print(
  paste(
    "Known failed/restarted files excluded:",
    nrow(excluded_psychopy_files)
  )
)


print(
  paste(
    "Files remaining for processing:",
    length(psychopy_files)
  )
)


# =========================================================
# 5. HELPER FUNCTION
#
# Examples:
#
# A, A, A -> A
# A, B    -> NA
# nothing -> NA
# =========================================================

get_unique_candidate <- function(candidates) {
  
  candidates <- candidates[
    !is.na(candidates) &
      candidates != ""
  ]
  
  candidates <- unique(candidates)
  
  
  if (length(candidates) == 1) {
    
    return(
      candidates
    )
  }
  
  
  return(
    NA_character_
  )
}


# =========================================================
# 6. EXTRACT METADATA FROM ONE RAW FILE
# =========================================================

extract_metadata <- function(file_path) {
  
  
  # -------------------------------------------------------
  # Read raw file
  # -------------------------------------------------------
  
  psychopy <- read_csv(
    file_path,
    show_col_types = FALSE
  )
  
  
  file_name <- basename(
    file_path
  )
  
  
  # -------------------------------------------------------
  # Initialise
  # -------------------------------------------------------
  
  participant <- NA_character_
  block <- NA_character_
  condition <- NA_character_
  
  participant_source <- NA_character_
  block_source <- NA_character_
  condition_source <- NA_character_
  
  status <- "OK"
  review_reason <- ""
  
  
  # =======================================================
  # PARTICIPANT
  # =======================================================
  
  
  # -------------------------------------------------------
  # Candidate from participant column
  # -------------------------------------------------------
  
  participant_from_column <- NA_character_
  
  
  if ("participant" %in% names(psychopy)) {
    
    x <- as.character(
      psychopy$participant
    )
    
    x <- x[
      !is.na(x) &
        x != ""
    ]
    
    
    if (length(x) > 0) {
      
      participant_from_column <- x[1]
    }
  }
  
  
  # -------------------------------------------------------
  # Extract ID before first underscore
  #
  # Example:
  #
  # HE20ÜB06_A_LB
  #
  # becomes:
  #
  # HE20ÜB06
  # -------------------------------------------------------
  
  participant_from_participant_column <- NA_character_
  
  
  if (!is.na(participant_from_column)) {
    
    participant_from_participant_column <- str_match(
      participant_from_column,
      "^([^_]+)"
    )[, 2]
  }
  
  
  # -------------------------------------------------------
  # Participant candidate from filename
  # -------------------------------------------------------
  
  participant_from_filename <- str_match(
    file_name,
    "^([^_]+)"
  )[, 2]
  
  
  # -------------------------------------------------------
  # Prefer participant column
  # -------------------------------------------------------
  
  if (!is.na(participant_from_participant_column)) {
    
    participant <- participant_from_participant_column
    
    participant_source <- "participant_column"
    
    
  } else if (!is.na(participant_from_filename)) {
    
    participant <- participant_from_filename
    
    participant_source <- "filename"
  }
  
  
  # =======================================================
  # CONDITION: HB / LB
  # =======================================================
  
  
  # -------------------------------------------------------
  # Candidate 1: filename
  # -------------------------------------------------------
  
  condition_filename_matches <- str_extract_all(
    file_name,
    "(?i)(?<=_)(HB|LB)(?=_|\\.)"
  )[[1]]
  
  
  condition_filename_matches <- toupper(
    condition_filename_matches
  )
  
  
  condition_filename <- get_unique_candidate(
    condition_filename_matches
  )
  
  
  # -------------------------------------------------------
  # Candidate 2: session column
  # -------------------------------------------------------
  
  condition_session <- NA_character_
  
  
  if ("session" %in% names(psychopy)) {
    
    session_values <- as.character(
      psychopy$session
    )
    
    session_values <- session_values[
      !is.na(session_values) &
        session_values != ""
    ]
    
    
    if (length(session_values) > 0) {
      
      session_text <- paste(
        session_values,
        collapse = " "
      )
      
      
      session_matches <- str_extract_all(
        session_text,
        "(?i)(?<![A-Z])(HB|LB)(?![A-Z])"
      )[[1]]
      
      
      session_matches <- toupper(
        session_matches
      )
      
      
      condition_session <- get_unique_candidate(
        session_matches
      )
    }
  }
  
  
  # -------------------------------------------------------
  # Candidate 3: Condition column
  # -------------------------------------------------------
  
  condition_condition_column <- NA_character_
  
  
  if ("Condition" %in% names(psychopy)) {
    
    condition_values <- as.character(
      psychopy$Condition
    )
    
    condition_values <- condition_values[
      !is.na(condition_values) &
        condition_values != ""
    ]
    
    
    if (length(condition_values) > 0) {
      
      condition_matches <- str_extract_all(
        
        paste(
          condition_values,
          collapse = " "
        ),
        
        "(?i)(?<![A-Z])(HB|LB)(?![A-Z])"
        
      )[[1]]
      
      
      condition_matches <- toupper(
        condition_matches
      )
      
      
      condition_condition_column <- get_unique_candidate(
        condition_matches
      )
    }
  }
  
  
  # -------------------------------------------------------
  # Candidate 4: participant column
  #
  # Example:
  #
  # HE20ÜB06_A_LB
  # -------------------------------------------------------
  
  condition_participant <- NA_character_
  
  
  if (!is.na(participant_from_column)) {
    
    participant_condition_matches <- str_extract_all(
      participant_from_column,
      "(?i)(?<=_)(HB|LB)(?=_|$)"
    )[[1]]
    
    
    participant_condition_matches <- toupper(
      participant_condition_matches
    )
    
    
    condition_participant <- get_unique_candidate(
      participant_condition_matches
    )
  }
  
  
  # -------------------------------------------------------
  # Resolve Condition
  # -------------------------------------------------------
  
  condition_candidates <- c(
    
    filename = condition_filename,
    
    session = condition_session,
    
    Condition_column = condition_condition_column,
    
    participant = condition_participant
  )
  
  
  available_conditions <- unique(
    
    condition_candidates[
      !is.na(condition_candidates)
    ]
  )
  
  
  if (length(available_conditions) == 0) {
    
    condition <- NA_character_
    
    condition_source <- NA_character_
    
    status <- "REVIEW"
    
    review_reason <- paste(
      review_reason,
      "Condition could not be determined."
    )
    
    
  } else if (length(available_conditions) > 1) {
    
    condition <- NA_character_
    
    condition_source <- NA_character_
    
    status <- "AMBIGUOUS"
    
    
    review_reason <- paste(
      
      review_reason,
      
      paste0(
        "Conflicting Condition values: ",
        paste(
          available_conditions,
          collapse = ", "
        ),
        "."
      )
    )
    
    
  } else {
    
    condition <- available_conditions[1]
    
    
    if (
      !is.na(condition_filename) &&
        condition_filename == condition
    ) {
      
      condition_source <- "filename"
      
      
    } else if (
      !is.na(condition_session) &&
        condition_session == condition
    ) {
      
      condition_source <- "session"
      
      
    } else if (
      !is.na(condition_condition_column) &&
        condition_condition_column == condition
    ) {
      
      condition_source <- "Condition_column"
      
      
    } else if (
      !is.na(condition_participant) &&
        condition_participant == condition
    ) {
      
      condition_source <- "participant_column"
    }
  }
  
  
  # =======================================================
  # BLOCK: A / B
  # =======================================================
  
  
  # -------------------------------------------------------
  # Candidate 1:
  # high-confidence filename structure
  #
  # Example:
  #
  # CH10AN25_B_LB_...
  #
  #          B = Block
  # -------------------------------------------------------
  
  block_filename_primary <- str_match(
    file_name,
    "^[^_]+_([AB])_(HB|LB)(?:_|\\.)"
  )[, 2]
  
  
  if (!is.na(block_filename_primary)) {
    
    block_filename <- toupper(
      block_filename_primary
    )
    
    block_filename_source <- "filename_primary"
    
    
  } else {
    
    
    # -----------------------------------------------------
    # General filename fallback
    # -----------------------------------------------------
    
    block_filename_matches <- str_extract_all(
      file_name,
      "(?i)(?<=_)[AB](?=_|\\.)"
    )[[1]]
    
    
    block_filename_matches <- toupper(
      block_filename_matches
    )
    
    
    block_filename <- get_unique_candidate(
      block_filename_matches
    )
    
    
    block_filename_source <- "filename_general"
  }
  
  
  # -------------------------------------------------------
  # Candidate 2: expName
  #
  # Balance_and_VR_A
  # Balance_and_VR_B
  # -------------------------------------------------------
  
  block_expname <- NA_character_
  
  
  if ("expName" %in% names(psychopy)) {
    
    expname_values <- as.character(
      psychopy$expName
    )
    
    
    expname_values <- expname_values[
      !is.na(expname_values) &
        expname_values != ""
    ]
    
    
    if (length(expname_values) > 0) {
      
      expname_text <- paste(
        expname_values,
        collapse = " "
      )
      
      
      expname_matches <- str_extract_all(
        expname_text,
        "(?i)(?<=_)[AB](?=_|$)"
      )[[1]]
      
      
      expname_matches <- toupper(
        expname_matches
      )
      
      
      block_expname <- get_unique_candidate(
        expname_matches
      )
    }
  }
  
  
  # -------------------------------------------------------
  # Candidate 3:
  # Condition column if its value is exactly A or B
  # -------------------------------------------------------
  
  block_condition_column <- NA_character_
  
  
  if ("Condition" %in% names(psychopy)) {
    
    condition_values <- as.character(
      psychopy$Condition
    )
    
    
    condition_values <- condition_values[
      !is.na(condition_values) &
        condition_values != ""
    ]
    
    
    if (length(condition_values) > 0) {
      
      unique_condition_values <- unique(
        toupper(condition_values)
      )
      
      
      unique_condition_values <- unique_condition_values[
        unique_condition_values %in% c("A", "B")
      ]
      
      
      if (length(unique_condition_values) == 1) {
        
        block_condition_column <- unique_condition_values[1]
      }
    }
  }
  
  
  # -------------------------------------------------------
  # Candidate 4: participant column
  #
  # HE20ÜB06_A_LB
  #          A = Block
  # -------------------------------------------------------
  
  block_participant <- NA_character_
  
  
  if (!is.na(participant_from_column)) {
    
    participant_block_matches <- str_extract_all(
      participant_from_column,
      "(?i)(?<=_)[AB](?=_)"
    )[[1]]
    
    
    participant_block_matches <- toupper(
      participant_block_matches
    )
    
    
    block_participant <- get_unique_candidate(
      participant_block_matches
    )
  }
  
  
  # -------------------------------------------------------
  # Resolve Block
  # -------------------------------------------------------
  
  block_candidates <- c(
    
    filename = block_filename,
    
    expName = block_expname,
    
    Condition_column = block_condition_column,
    
    participant = block_participant
  )
  
  
  available_blocks <- unique(
    
    block_candidates[
      !is.na(block_candidates)
    ]
  )
  
  
  if (length(available_blocks) == 0) {
    
    block <- NA_character_
    
    block_source <- NA_character_
    
    
    if (status == "OK") {
      
      status <- "REVIEW"
    }
    
    
    review_reason <- paste(
      review_reason,
      "Block could not be determined."
    )
    
    
  } else if (length(available_blocks) > 1) {
    
    block <- NA_character_
    
    block_source <- NA_character_
    
    status <- "AMBIGUOUS"
    
    
    review_reason <- paste(
      
      review_reason,
      
      paste0(
        "Conflicting Block values: ",
        paste(
          available_blocks,
          collapse = ", "
        ),
        "."
      )
    )
    
    
  } else {
    
    block <- available_blocks[1]
    
    
    if (
      !is.na(block_filename) &&
        block_filename == block
    ) {
      
      block_source <- block_filename_source
      
      
    } else if (
      !is.na(block_expname) &&
        block_expname == block
    ) {
      
      block_source <- "expName"
      
      
    } else if (
      !is.na(block_condition_column) &&
        block_condition_column == block
    ) {
      
      block_source <- "Condition_column"
      
      
    } else if (
      !is.na(block_participant) &&
        block_participant == block
    ) {
      
      block_source <- "participant_column"
    }
  }
  
  
  # =======================================================
  # FINAL VALIDATION
  # =======================================================
  
  if (is.na(participant)) {
    
    if (status == "OK") {
      
      status <- "REVIEW"
    }
    
    
    review_reason <- paste(
      review_reason,
      "Participant ID could not be determined."
    )
  }
  
  
  if (status == "OK") {
    
    review_reason <- ""
  }
  
  
  # =======================================================
  # RETURN ONE ROW
  # =======================================================
  
  tibble(
    
    source_file = file_name,
    
    source_path = file_path,
    
    source_year = str_extract(
      file_path,
      "20[0-9]{2}"
    ),
    
    participant = participant,
    
    Block = block,
    
    Condition = condition,
    
    participant_source = participant_source,
    
    Block_source = block_source,
    
    Condition_source = condition_source,
    
    status = status,
    
    review_reason = str_squish(
      review_reason
    )
  )
}


# =========================================================
# 7. PROCESS ALL NON-EXCLUDED RAW FILES
# =========================================================

psychopy_metadata <- map_dfr(
  
  psychopy_files,
  
  function(file_path) {
    
    print(
      paste(
        "Processing metadata:",
        basename(file_path)
      )
    )
    
    
    extract_metadata(
      file_path
    )
  }
)


# =========================================================
# 8. MANUAL METADATA CORRECTIONS
#
# Exceptional files can be corrected here.
#
# This happens AFTER automatic extraction.
#
# Raw files are NOT modified.
#
# The correction therefore remains explicit and
# reproducible in this script.
# =========================================================


# ---------------------------------------------------------
# CH10AN25:
#
# Filename starts with:
#
# CH10AN25_B_LB__Balance_and_VR_B_2024-07-01...
#
# Correct Block = B
#
#
# LE11ON12:
#
# Filename starts with:
#
# LE11ON12_B_LB__Balance_and_VR_A_2024-06-24...
#
# Correct Block = B
#
# ---------------------------------------------------------


psychopy_metadata <- psychopy_metadata %>%
  
  mutate(
    
    manual_Block = case_when(
      
      str_starts(
        source_file,
        "CH10AN25_B_LB__Balance_and_VR_B_2024-07-01_"
      ) ~ "B",
      
      str_starts(
        source_file,
        "LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_"
      ) ~ "B",
      
      TRUE ~ NA_character_
    ),
    
    
    manual_correction_reason = case_when(
      
      str_starts(
        source_file,
        "CH10AN25_B_LB__Balance_and_VR_B_2024-07-01_"
      ) ~
        "Block manually set to B based on ID_B_LB pattern in original filename.",
      
      
      str_starts(
        source_file,
        "LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_"
      ) ~
        "Block manually set to B based on ID_B_LB pattern in original filename.",
      
      
      TRUE ~ NA_character_
    ),
    
    
    Block = if_else(
      !is.na(manual_Block),
      manual_Block,
      Block
    ),
    
    
    Block_source = if_else(
      !is.na(manual_Block),
      "manual_correction",
      Block_source
    ),
    
    
    status = if_else(
      !is.na(manual_Block),
      "OK",
      status
    ),
    
    
    review_reason = if_else(
      !is.na(manual_Block),
      "",
      review_reason
    )
    
  ) %>%
  
  select(
    -manual_Block
  )


# =========================================================
# 9. CREATE STANDARDISED INTERMEDIATE FILENAMES
#
# Naming format:
#
# participant_Block_Condition.csv
#
# Example:
#
# AN25BE12_A_LB.csv
# =========================================================

psychopy_metadata <- psychopy_metadata %>%
  
  mutate(
    
    intermediate_file = if_else(
      
      !is.na(participant) &
        !is.na(Block) &
        !is.na(Condition),
      
      paste0(
        participant,
        "_",
        Block,
        "_",
        Condition,
        ".csv"
      ),
      
      NA_character_
    ),
    
    
    intermediate_path = if_else(
      
      !is.na(intermediate_file),
      
      file.path(
        intermediate_folder,
        intermediate_file
      ),
      
      NA_character_
    )
  )


# =========================================================
# 10. CHECK FOR DUPLICATE INTERMEDIATE FILENAMES
#
# This occurs AFTER known failed measurements have already
# been excluded.
#
# Therefore, any remaining duplicate is unexpected and
# should be inspected before files are written.
# =========================================================

duplicate_intermediate_names <- psychopy_metadata %>%
  
  filter(
    !is.na(intermediate_file)
  ) %>%
  
  count(
    intermediate_file
  ) %>%
  
  filter(
    n > 1
  )


if (nrow(duplicate_intermediate_names) > 0) {
  
  print(
    duplicate_intermediate_names
  )
  
  
  stop(
    paste0(
      "Duplicate intermediate filenames detected. ",
      "No intermediate files were written. ",
      "Inspect duplicate_intermediate_names."
    )
  )
}


# =========================================================
# 11. FUNCTION TO CREATE ONE INTERMEDIATE FILE
# =========================================================

create_intermediate_file <- function(
  source_path,
  output_path,
  participant,
  block,
  condition
) {
  
  
  # -------------------------------------------------------
  # Read original raw file
  # -------------------------------------------------------
  
  psychopy <- read_csv(
    source_path,
    show_col_types = FALSE
  )
  
  
  # -------------------------------------------------------
  # Find click columns needed for Stage 2
  #
  # Examples:
  #
  # Klicks_Block_A_01.clicked_name
  # Klicks_Block_A_02.clicked_name
  # Klicks_Block_A_03.clicked_name
  # Klicks_Block_A_04.clicked_name
  #
  # or corresponding Block B columns
  # -------------------------------------------------------
  
  click_columns <- names(psychopy)[
    
    str_detect(
      names(psychopy),
      "^Klicks_Block_[AB]_0[1-4]\\.clicked_name$"
    )
  ]
  
  
  # -------------------------------------------------------
  # Original metadata columns retained for audit purposes
  # -------------------------------------------------------
  
  metadata_columns <- c(
    "participant",
    "session",
    "Condition",
    "date",
    "expName",
    "psychopyVersion"
  )
  
  
  metadata_columns <- metadata_columns[
    metadata_columns %in% names(psychopy)
  ]
  
  
  # -------------------------------------------------------
  # Keep only columns needed downstream
  # -------------------------------------------------------
  
  intermediate_data <- psychopy %>%
    
    select(
      all_of(metadata_columns),
      all_of(click_columns)
    )
  
  
  # -------------------------------------------------------
  # Add standardised metadata
  #
  # Stage 2 should rely on these standardised columns.
  # -------------------------------------------------------
  
  intermediate_data <- intermediate_data %>%
    
    mutate(
      
      participant_standard = participant,
      
      Block_standard = block,
      
      Condition_standard = condition,
      
      .before = 1
    )
  
  
  # -------------------------------------------------------
  # Write intermediate COPY
  #
  # Raw source file remains unchanged.
  # -------------------------------------------------------
  
  write_csv(
    intermediate_data,
    output_path
  )
}


# =========================================================
# 12. SELECT FILES ELIGIBLE FOR INTERMEDIATE CREATION
# =========================================================

metadata_for_intermediate_files <- psychopy_metadata %>%
  
  filter(
    status == "OK",
    !is.na(participant),
    !is.na(Block),
    !is.na(Condition),
    !is.na(intermediate_path)
  )


# =========================================================
# 13. CREATE INTERMEDIATE FILES
# =========================================================

pwalk(
  
  metadata_for_intermediate_files %>%
    
    select(
      source_path,
      intermediate_path,
      participant,
      Block,
      Condition
    ),
  
  
  function(
    source_path,
    intermediate_path,
    participant,
    Block,
    Condition
  ) {
    
    print(
      paste(
        "Creating intermediate file:",
        basename(intermediate_path)
      )
    )
    
    
    create_intermediate_file(
      
      source_path = source_path,
      
      output_path = intermediate_path,
      
      participant = participant,
      
      block = Block,
      
      condition = Condition
    )
  }
)


# =========================================================
# 14. CREATE REVIEW TABLE
#
# Happens AFTER manual metadata corrections.
# =========================================================

psychopy_metadata_review <- psychopy_metadata %>%
  
  filter(
    status != "OK"
  )


# =========================================================
# 15. WRITE AUDIT / METADATA FILES
# =========================================================

write_csv(
  psychopy_metadata,
  metadata_file
)


write_csv(
  psychopy_metadata_review,
  review_file
)


write_csv(
  excluded_psychopy_files,
  exclusion_file
)


# =========================================================
# 16. PRINT SUMMARY
# =========================================================

print("")
print("==============================================")
print("PSYCHOPY STAGE 1 FINISHED")
print("==============================================")


print(
  paste(
    "Total raw files found:",
    length(all_psychopy_files)
  )
)


print(
  paste(
    "Known failed/restarted files excluded:",
    nrow(excluded_psychopy_files)
  )
)


print(
  paste(
    "Files processed:",
    nrow(psychopy_metadata)
  )
)


print(
  paste(
    "Files marked OK:",
    sum(
      psychopy_metadata$status == "OK"
    )
  )
)


print(
  paste(
    "Files requiring review:",
    nrow(psychopy_metadata_review)
  )
)


print(
  paste(
    "Intermediate files created:",
    nrow(metadata_for_intermediate_files)
  )
)


print("")
print("Condition distribution:")

print(
  table(
    psychopy_metadata$Condition,
    useNA = "ifany"
  )
)


print("")
print("Block distribution:")

print(
  table(
    psychopy_metadata$Block,
    useNA = "ifany"
  )
)


print("")
print("Metadata file:")

print(
  metadata_file
)


print("")
print("Review file:")

print(
  review_file
)


print("")
print("Exclusion log:")

print(
  exclusion_file
)


print("")
print("Intermediate folder:")

print(
  intermediate_folder
)


# =========================================================
# 17. OPTIONAL CONSOLE CHECKS
# =========================================================

psychopy_metadata_review

excluded_psychopy_files
