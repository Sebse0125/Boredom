# =========================================================
# PSYCHOPY STAGE 1
# METADATA EXTRACTION / STANDARDISATION
#
# Input:
#   data/raw/psychopy/2024
#   data/raw/psychopy/2026
#
# Output:
#   data/meta/psychopy/psychopy_metadata.csv
#   data/meta/psychopy/psychopy_metadata_review.csv
#
# Raw files are NEVER modified.
# =========================================================


library(readr)
library(dplyr)
library(stringr)
library(purrr)


# ---------------------------------------------------------
# 1. Define paths
# ---------------------------------------------------------

input_folders <- c(
  "data/raw/psychopy/2024",
  "data/raw/psychopy/2026"
)

output_folder <- "data/meta/psychopy"

metadata_file <- file.path(
  output_folder,
  "psychopy_metadata.csv"
)

review_file <- file.path(
  output_folder,
  "psychopy_metadata_review.csv"
)


# ---------------------------------------------------------
# 2. Find all CSV files
# ---------------------------------------------------------

psychopy_files <- unlist(
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


print(
  paste(
    "Found",
    length(psychopy_files),
    "PsychoPy files."
  )
)


if (length(psychopy_files) == 0) {
  
  stop(
    "No CSV files were found."
  )
}


# ---------------------------------------------------------
# 3. Helper function
#
# Return a unique value from a vector.
#
# A, A, A -> A
# A, B    -> NA
# nothing -> NA
# ---------------------------------------------------------

get_unique_candidate <- function(candidates) {
  
  candidates <- candidates[
    !is.na(candidates) &
      candidates != ""
  ]
  
  candidates <- unique(candidates)
  
  if (length(candidates) == 1) {
    return(candidates)
  }
  
  return(NA_character_)
}


# ---------------------------------------------------------
# 4. Process one file
# ---------------------------------------------------------

extract_metadata <- function(file_path) {
  
  
  # -------------------------------------------------------
  # Read file
  # -------------------------------------------------------
  
  psychopy <- read_csv(
    file_path,
    show_col_types = FALSE
  )
  
  
  file_name <- basename(file_path)
  
  
  # -------------------------------------------------------
  # Initialise variables
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
  # Participant from participant column
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
  # HE20ÜB06_A_LB
  #
  # becomes:
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
  # Participant from filename
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
  #
  # Find HB/LB as a standalone underscore-separated
  # component.
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
  #
  # Priority:
  #
  #   1. filename
  #   2. session
  #   3. Condition column
  #   4. participant column
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
      "Condition could not be determined.",
      sep = " "
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
      ),
      sep = " "
    )
    
  } else {
    
    condition <- available_conditions[1]
    
    
    if (!is.na(condition_filename) &&
        condition_filename == condition) {
      
      condition_source <- "filename"
      
    } else if (!is.na(condition_session) &&
               condition_session == condition) {
      
      condition_source <- "session"
      
    } else if (!is.na(condition_condition_column) &&
               condition_condition_column == condition) {
      
      condition_source <- "Condition_column"
      
    } else if (!is.na(condition_participant) &&
               condition_participant == condition) {
      
      condition_source <- "participant_column"
    }
  }
  
  
  # =======================================================
  # BLOCK: A / B
  # =======================================================
  
  
  # -------------------------------------------------------
  # Candidate 1: HIGH-CONFIDENCE filename pattern
  #
  # Look specifically for:
  #
  #   ID_A_HB
  #   ID_A_LB
  #   ID_B_HB
  #   ID_B_LB
  #
  # Examples:
  #
  #   CH10AN25_B_LB__Balance_and_VR_B_2024...
  #           ↑
  #           Block
  #
  #   LE11ON12_B_LB__Balance_and_VR_A_2024...
  #            ↑
  #            Block
  #
  # IMPORTANT:
  # Once this pattern is found, we DO NOT use other
  # A/B occurrences in the filename as competing
  # Block candidates.
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
    # Candidate 1b: general filename search
    #
    # Only used if the high-confidence pattern above
    # cannot be found.
    # -----------------------------------------------------
    
    block_filename_matches <- str_extract_all(
      file_name,
      "(?i)(?<=_)[AB](?=_|\\.)"
    )[[1]]
    
    block_filename_matches <- toupper(
      block_filename_matches
    )
    
    block_filename_general <- get_unique_candidate(
      block_filename_matches
    )
    
    
    block_filename <- block_filename_general
    
    block_filename_source <- "filename_general"
  }
  
  
  # -------------------------------------------------------
  # Candidate 2: expName column
  #
  # Examples:
  #   Balance_and_VR_A
  #   Balance_and_VR_B
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
  # Candidate 3: Condition column
  #
  # Some files use:
  #
  #   Condition = A
  #   Condition = B
  #
  # IMPORTANT:
  # We only consider the entire value A or B here.
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
  # Example:
  #
  #   HE20ÜB06_A_LB
  #           ↑
  #         Block
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
  
  
  # =======================================================
  # RESOLVE BLOCK
  # =======================================================
  
  
  # -------------------------------------------------------
  # HIGH-CONFIDENCE filename result
  #
  # If the primary filename pattern exists, it is treated
  # as ONE candidate only.
  #
  # Other A/B occurrences elsewhere in the filename do
  # NOT enter this comparison.
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
  
  
  # -------------------------------------------------------
  # No Block found
  # -------------------------------------------------------
  
  if (length(available_blocks) == 0) {
    
    block <- NA_character_
    block_source <- NA_character_
    
    if (status == "OK") {
      status <- "REVIEW"
    }
    
    review_reason <- paste(
      review_reason,
      "Block could not be determined.",
      sep = " "
    )
  }
  
  
  # -------------------------------------------------------
  # Conflicting Block information
  # -------------------------------------------------------
  
  else if (length(available_blocks) > 1) {
    
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
      ),
      sep = " "
    )
  }
  
  
  # -------------------------------------------------------
  # Block successfully determined
  # -------------------------------------------------------
  
  else {
    
    block <- available_blocks[1]
    
    
    # -----------------------------------------------------
    # Determine source
    # -----------------------------------------------------
    
    if (!is.na(block_filename) &&
        block_filename == block) {
      
      block_source <- block_filename_source
      
    } else if (!is.na(block_expname) &&
               block_expname == block) {
      
      block_source <- "expName"
      
    } else if (!is.na(block_condition_column) &&
               block_condition_column == block) {
      
      block_source <- "Condition_column"
      
    } else if (!is.na(block_participant) &&
               block_participant == block) {
      
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
      "Participant ID could not be determined.",
      sep = " "
    )
  }
  
  
  # -------------------------------------------------------
  # Clean review reason
  # -------------------------------------------------------
  
  if (status == "OK") {
    review_reason <- ""
  }
  
  
  # =======================================================
  # Return one row
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


# ---------------------------------------------------------
# 5. Process all files
# ---------------------------------------------------------

psychopy_metadata <- map_dfr(
  
  psychopy_files,
  
  function(file_path) {
    
    print(
      paste(
        "Processing:",
        basename(file_path)
      )
    )
    
    extract_metadata(
      file_path
    )
  }
)


# =========================================================
# 6. MANUAL CORRECTIONS
#
# These corrections are applied AFTER automatic extraction.
#
# IMPORTANT:
# - Raw PsychoPy files are NOT changed.
# - Corrections are stored here in the script.
# - Therefore, the corrections are fully reproducible.
# - Match using the exact source filename.
# =========================================================

manual_corrections <- tibble(
  
  source_file = c(
    "CH10AN25_B_LB__Balance_and_VR_B_2024-07-01_12h51.08.713.csv",
    "LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_09h45.48.708.csv"
  ),
  
  Block_manual = c(
    "B",
    "B"
  ),
  
  correction_reason = c(
    "Filename contains ID_B_LB; later B/A occurrences refer to experiment naming.",
    "Filename contains ID_B_LB; later A refers to experiment naming."
  )
)

# ---------------------------------------------------------
# Apply manual Block corrections
# ---------------------------------------------------------

if (nrow(manual_corrections) > 0) {
  
  psychopy_metadata <- psychopy_metadata %>%
    
    left_join(
      manual_corrections,
      by = "source_file"
    ) %>%
    
    mutate(
      
      Block = if_else(
        !is.na(Block_manual),
        Block_manual,
        Block
      ),
      
      Block_source = if_else(
        !is.na(Block_manual),
        "manual_correction",
        Block_source
      ),
      
      status = if_else(
        !is.na(Block_manual),
        "OK",
        status
      ),
      
      review_reason = if_else(
        !is.na(Block_manual),
        "Block manually corrected in Stage 1 script.",
        review_reason
      )
      
    ) %>%
    
    select(
      -Block_manual
    )
}


# ---------------------------------------------------------
# Re-create review table AFTER manual corrections
# ---------------------------------------------------------

psychopy_metadata_review <- psychopy_metadata %>%
  filter(
    status != "OK"
  )




# ---------------------------------------------------------
# 7. Create output directory
# ---------------------------------------------------------

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------
# 8. Write complete metadata file
# ---------------------------------------------------------

write_csv(
  psychopy_metadata,
  metadata_file
)


# ---------------------------------------------------------
# 9. Create review file
# ---------------------------------------------------------

psychopy_metadata_review <- psychopy_metadata %>%
  filter(
    status != "OK"
  )


write_csv(
  psychopy_metadata_review,
  review_file
)


# ---------------------------------------------------------
# 10. Print summary
# ---------------------------------------------------------

print("")
print("==============================================")
print("PSYCHOPY METADATA EXTRACTION FINISHED")
print("==============================================")


print(
  paste(
    "Total files:",
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
    nrow(
      psychopy_metadata_review
    )
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


# ---------------------------------------------------------
# 11. View review data
# ---------------------------------------------------------

psychopy_metadata_review

