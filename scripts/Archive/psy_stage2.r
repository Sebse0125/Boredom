# =========================================================
# PSYCHOPY STAGE 2
# MOVEMENT DATA EXTRACTION
#
# Uses cleaned metadata created in Stage 1.
#
# Input metadata:
#   data/meta/psychopy/psychopy_metadata.csv
#
# Input raw files:
#   paths stored in source_path
#
# Output:
#   data/processed/psychopy/master/psychopy_master.csv
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

metadata_file <- "data/meta/psychopy/psychopy_metadata.csv"

output_file <- "data/processed/psychopy/master/psychopy_master.csv"


# ---------------------------------------------------------
# 2. Read cleaned metadata from Stage 1
# ---------------------------------------------------------

psychopy_metadata <- read_csv(
  metadata_file,
  show_col_types = FALSE
)


print(
  paste(
    "Metadata rows found:",
    nrow(psychopy_metadata)
  )
)


# ---------------------------------------------------------
# 3. Check required metadata columns
# ---------------------------------------------------------

required_metadata_columns <- c(
  "source_file",
  "source_path",
  "source_year",
  "participant",
  "Block",
  "Condition",
  "status"
)


missing_metadata_columns <- setdiff(
  required_metadata_columns,
  names(psychopy_metadata)
)


if (length(missing_metadata_columns) > 0) {
  
  stop(
    paste0(
      "Metadata file is missing required columns: ",
      paste(
        missing_metadata_columns,
        collapse = ", "
      )
    )
  )
}


# ---------------------------------------------------------
# 4. Make sure all metadata rows are approved
# ---------------------------------------------------------

non_ok_metadata <- psychopy_metadata %>%
  filter(
    status != "OK"
  )


if (nrow(non_ok_metadata) > 0) {
  
  stop(
    paste0(
      "Stage 2 stopped because ",
      nrow(non_ok_metadata),
      " metadata row(s) are not marked OK. ",
      "Please resolve these in Stage 1 first."
    )
  )
}


# ---------------------------------------------------------
# 5. Check that Block and Condition are valid
# ---------------------------------------------------------

invalid_metadata <- psychopy_metadata %>%
  filter(
    !Block %in% c("A", "B") |
      !Condition %in% c("HB", "LB")
  )


if (nrow(invalid_metadata) > 0) {
  
  stop(
    paste0(
      "Stage 2 stopped because ",
      nrow(invalid_metadata),
      " row(s) contain invalid Block or Condition values."
    )
  )
}


# =========================================================
# 6. MANUAL RAW-COLUMN OVERRIDES
#
# These are files where the cleaned metadata is correct,
# but the raw PsychoPy column names use the wrong Block
# letter.
#
# Example:
#
# Metadata says:
#   Block = B
#
# But raw columns are:
#   Klicks_Block_A_01.clicked_name
#   ...
#
# Raw files are NOT changed.
# This only tells Stage 2 which raw column labels to read.
# =========================================================

column_block_overrides <- tibble(
  
  source_file = c(
    "LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_09h45.48.708.csv"
  ),
  
  column_block = c(
    "A"
  )
)


# ---------------------------------------------------------
# 7. Helper function
#
# Extract first non-empty value
# ---------------------------------------------------------

first_non_empty <- function(x) {
  
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      x != ""
  ]
  
  if (length(x) == 0) {
    
    return(NA_character_)
    
  } else {
    
    return(x[1])
  }
}


# ---------------------------------------------------------
# 8. Function to process ONE raw PsychoPy file
# ---------------------------------------------------------

process_psychopy_file <- function(
  file_path,
  source_file,
  source_year,
  participant,
  block,
  condition
) {
  
  
  # -------------------------------------------------------
  # Check that raw file exists
  # -------------------------------------------------------
  
  if (!file.exists(file_path)) {
    
    stop(
      paste0(
        "Raw file does not exist: ",
        file_path
      )
    )
  }
  
  
  # -------------------------------------------------------
  # Read raw PsychoPy file
  #
  # READ ONLY:
  # nothing is written back to this file.
  # -------------------------------------------------------
  
  psychopy <- read_csv(
    file_path,
    show_col_types = FALSE
  )
  
  
  # -------------------------------------------------------
  # Determine which Block label is actually used in the
  # raw clicked_name column names.
  #
  # Normally:
  #   column_block = cleaned metadata Block
  #
  # For known faulty files:
  #   column_block = manual override
  # -------------------------------------------------------
  
  column_block <- block
  
  
  override_row <- column_block_overrides %>%
    filter(
      .data$source_file == source_file
    )
  
  
  if (nrow(override_row) == 1) {
    
    column_block <- override_row$column_block[1]
    
    print(
      paste(
        "  Raw-column override:",
        source_file,
        "| metadata Block:",
        block,
        "| raw column Block:",
        column_block
      )
    )
  }
  
  
  if (nrow(override_row) > 1) {
    
    stop(
      paste0(
        "More than one raw-column override exists for: ",
        source_file
      )
    )
  }
  
  
  # -------------------------------------------------------
  # Expected clicked_name columns
  #
  # IMPORTANT:
  # Uses column_block, not necessarily the cleaned Block.
  # -------------------------------------------------------
  
  expected_click_columns <- paste0(
    "Klicks_Block_",
    column_block,
    "_",
    sprintf("%02d", 1:4),
    ".clicked_name"
  )
  
  
  # -------------------------------------------------------
  # Check that all four expected columns exist
  # -------------------------------------------------------
  
  missing_click_columns <- setdiff(
    expected_click_columns,
    names(psychopy)
  )
  
  
  if (length(missing_click_columns) > 0) {
    
    stop(
      paste0(
        "\nFile: ",
        source_file,
        "\nCleaned Block from metadata: ",
        block,
        "\nRaw-column Block being used: ",
        column_block,
        "\nExpected clicked_name columns are missing: ",
        paste(
          missing_click_columns,
          collapse = ", "
        )
      )
    )
  }
  
  
  # -------------------------------------------------------
  # Extract general information
  #
  # Participant, Block and Condition come from Stage 1.
  # -------------------------------------------------------
  
  session_value <- NA_character_
  date_value <- NA_character_
  expname_value <- NA_character_
  psychopy_version_value <- NA_character_
  
  
  if ("session" %in% names(psychopy)) {
    
    session_value <- first_non_empty(
      psychopy$session
    )
  }
  
  
  if ("date" %in% names(psychopy)) {
    
    date_value <- first_non_empty(
      psychopy$date
    )
  }
  
  
  if ("expName" %in% names(psychopy)) {
    
    expname_value <- first_non_empty(
      psychopy$expName
    )
  }
  
  
  if ("psychopyVersion" %in% names(psychopy)) {
    
    psychopy_version_value <- first_non_empty(
      psychopy$psychopyVersion
    )
  }
  
  
  # -------------------------------------------------------
  # Create one-row general information table
  #
  # Block remains the CLEANED metadata Block.
  # -------------------------------------------------------
  
  general_data <- tibble(
    
    source_year = source_year,
    
    source_file = source_file,
    
    participant = participant,
    
    session = session_value,
    
    Condition = condition,
    
    Block = block,
    
    date = date_value,
    
    expName = expname_value,
    
    psychopyVersion = psychopy_version_value
  )
  
  
  # -------------------------------------------------------
  # Extract first non-empty clicked_name value from each
  # relevant trial column
  # -------------------------------------------------------
  
  movement_raw <- psychopy %>%
    
    select(
      all_of(
        expected_click_columns
      )
    ) %>%
    
    summarise(
      across(
        everything(),
        first_non_empty
      )
    )
  
  
  # -------------------------------------------------------
  # Create empty movement result
  # -------------------------------------------------------
  
  movement_result <- tibble()
  
  
  # -------------------------------------------------------
  # Count movements for trials 01-04
  # -------------------------------------------------------
  
  for (i in 1:4) {
    
    trial <- sprintf(
      "%02d",
      i
    )
    
    
    # -----------------------------------------------------
    # clicked_name column for current trial
    #
    # Uses raw column Block label.
    # -----------------------------------------------------
    
    actual_column <- paste0(
      "Klicks_Block_",
      column_block,
      "_",
      trial,
      ".clicked_name"
    )
    
    
    clicks <- movement_raw[[actual_column]]
    
    
    # -----------------------------------------------------
    # Count arms
    #
    # Uses raw Block label because the strings inside the
    # faulty file may also contain the wrong A/B label.
    # -----------------------------------------------------
    
    arms_count <- str_count(
      clicks,
      fixed(
        paste0(
          "Arms_Block_",
          column_block,
          "_",
          trial
        )
      )
    )
    
    
    # -----------------------------------------------------
    # Count legs
    # -----------------------------------------------------
    
    legs_count <- str_count(
      clicks,
      fixed(
        paste0(
          "Legs_Block_",
          column_block,
          "_",
          trial
        )
      )
    )
    
    
    # -----------------------------------------------------
    # Count head
    # -----------------------------------------------------
    
    head_count <- str_count(
      clicks,
      fixed(
        paste0(
          "Head_Block_",
          column_block,
          "_",
          trial
        )
      )
    )
    
    
    # -----------------------------------------------------
    # Count other
    # -----------------------------------------------------
    
    other_count <- str_count(
      clicks,
      fixed(
        paste0(
          "Other_Block_",
          column_block,
          "_",
          trial
        )
      )
    )
    
    
    # -----------------------------------------------------
    # Total clicks
    #
    # NA stays NA if the clicked_name value is missing.
    # -----------------------------------------------------
    
    total_count <-
      arms_count +
      legs_count +
      head_count +
      other_count
    
    
    # -----------------------------------------------------
    # Create trial output columns
    #
    # IMPORTANT:
    # Output column names are based on Condition (HB/LB),
    # not on the faulty raw Block label.
    # -----------------------------------------------------
    
    trial_data <- tibble(
      
      !!paste0(
        "arms_",
        condition,
        "_",
        trial
      ) := arms_count,
      
      !!paste0(
        "legs_",
        condition,
        "_",
        trial
      ) := legs_count,
      
      !!paste0(
        "head_",
        condition,
        "_",
        trial
      ) := head_count,
      
      !!paste0(
        "other_",
        condition,
        "_",
        trial
      ) := other_count,
      
      !!paste0(
        "total_click_",
        condition,
        "_",
        trial
      ) := total_count
    )
    
    
    # -----------------------------------------------------
    # Add current trial to movement result
    # -----------------------------------------------------
    
    if (ncol(movement_result) == 0) {
      
      movement_result <- trial_data
      
    } else {
      
      movement_result <- bind_cols(
        movement_result,
        trial_data
      )
    }
  }
  
  
  # -------------------------------------------------------
  # Combine general information and movement data
  # -------------------------------------------------------
  
  result <- bind_cols(
    general_data,
    movement_result
  )
  
  
  # -------------------------------------------------------
  # Return exactly one row
  # -------------------------------------------------------
  
  return(result)
}


# ---------------------------------------------------------
# 9. Process all files using Stage 1 metadata
# ---------------------------------------------------------

psychopy_results <- pmap(
  
  list(
    psychopy_metadata$source_path,
    psychopy_metadata$source_file,
    psychopy_metadata$source_year,
    psychopy_metadata$participant,
    psychopy_metadata$Block,
    psychopy_metadata$Condition
  ),
  
  function(
    source_path,
    source_file,
    source_year,
    participant,
    Block,
    Condition
  ) {
    
    print(
      paste(
        "Processing:",
        source_file,
        "| Block:",
        Block,
        "| Condition:",
        Condition
      )
    )
    
    
    process_psychopy_file(
      file_path = source_path,
      source_file = source_file,
      source_year = source_year,
      participant = participant,
      block = Block,
      condition = Condition
    )
  }
)


# ---------------------------------------------------------
# 10. Combine all files
#
# HB and LB rows have different movement column names.
# bind_rows() fills non-applicable columns with NA.
# ---------------------------------------------------------

psychopy_master <- bind_rows(
  psychopy_results
)


# ---------------------------------------------------------
# 11. Define final movement column order
# ---------------------------------------------------------

movement_columns <- unlist(
  
  lapply(
    
    c(
      "HB",
      "LB"
    ),
    
    function(condition) {
      
      unlist(
        
        lapply(
          
          sprintf(
            "%02d",
            1:4
          ),
          
          function(trial) {
            
            c(
              paste0(
                "arms_",
                condition,
                "_",
                trial
              ),
              
              paste0(
                "legs_",
                condition,
                "_",
                trial
              ),
              
              paste0(
                "head_",
                condition,
                "_",
                trial
              ),
              
              paste0(
                "other_",
                condition,
                "_",
                trial
              ),
              
              paste0(
                "total_click_",
                condition,
                "_",
                trial
              )
            )
          }
        )
      )
    }
  )
)


# ---------------------------------------------------------
# 12. Put columns into clean final order
# ---------------------------------------------------------

general_columns <- c(
  "source_year",
  "source_file",
  "participant",
  "session",
  "Condition",
  "Block",
  "date",
  "expName",
  "psychopyVersion"
)


movement_columns_existing <- intersect(
  movement_columns,
  names(psychopy_master)
)


psychopy_master <- psychopy_master %>%
  
  select(
    all_of(general_columns),
    all_of(movement_columns_existing)
  )


# ---------------------------------------------------------
# 13. Sort master dataset
# ---------------------------------------------------------

psychopy_master <- psychopy_master %>%
  
  arrange(
    participant,
    source_year,
    Condition,
    Block
  )


# ---------------------------------------------------------
# 14. Safety check:
#     one output row per raw/metadata file
# ---------------------------------------------------------

if (nrow(psychopy_master) != nrow(psychopy_metadata)) {
  
  stop(
    paste0(
      "Unexpected row count. Metadata contains ",
      nrow(psychopy_metadata),
      " rows, but master dataset contains ",
      nrow(psychopy_master),
      " rows."
    )
  )
}


# ---------------------------------------------------------
# 15. Create output directory
# ---------------------------------------------------------

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------
# 16. Write master CSV
# ---------------------------------------------------------

write_csv(
  psychopy_master,
  output_file
)


# ---------------------------------------------------------
# 17. Report result
# ---------------------------------------------------------

print("")
print("==============================================")
print("PSYCHOPY STAGE 2 FINISHED")
print("==============================================")


print(
  paste(
    "Raw files processed:",
    nrow(psychopy_metadata)
  )
)


print(
  paste(
    "Rows in master file:",
    nrow(psychopy_master)
  )
)


print(
  paste(
    "Participants:",
    n_distinct(
      psychopy_master$participant
    )
  )
)


print("")
print("Condition distribution:")

print(
  table(
    psychopy_master$Condition,
    useNA = "ifany"
  )
)


print("")
print("Block distribution:")

print(
  table(
    psychopy_master$Block,
    useNA = "ifany"
  )
)


print("")
print("Master file written to:")

print(
  output_file
)


# ---------------------------------------------------------
# 18. View final master dataset
# ---------------------------------------------------------

psychopy_master