#exchanging Ä, Ü, Ö

#count and check IDs

library(readr)
library(dplyr)
library(stringr)

# Path to your folder
folder <- "data/raw/psychopy/2024"

# Find all CSV files
files <- list.files(
  path = folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read the participant ID from each file
results <- lapply(files, function(file) {
  
  data <- read_csv(file, show_col_types = FALSE)
  
  # Take the participant value from the first row
  participant <- data$participant[1]
  
  # Remove everything from the first "_" onwards
  participant <- str_extract(participant, "^[^_]+")
  
  data.frame(
    file = basename(file),
    participant = participant
  )
}) %>%
  bind_rows()

# Count how many files each participant ID has
id_counts <- results %>%
  count(participant, sort = TRUE)

# Display the counts
print(id_counts)




# ============================================================
# PARTICIPANT ID CHECKING AND CORRECTION
# ============================================================

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ------------------------------------------------------------
# 1. FILE LOCATIONS
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1. FILE LOCATIONS
# ------------------------------------------------------------

# Folder containing the RAW CSV files
raw_folder <- "data/raw/psychopy/2026"

# Your ONE reference file containing all correct participant IDs
reference_file <- "data/raw/balance/2026/balance_output_2026.xlsx"

# NEW folder where corrected copies will eventually be saved
corrected_folder <- "data/cleaned/psychopy/2024"

# IMPORTANT:
# Keep this FALSE while checking the results!
APPLY_CORRECTIONS <- FALSE


# ------------------------------------------------------------
# 2. READ THE CORRECT IDs FROM THE REFERENCE FILE
# ------------------------------------------------------------

reference <- read_excel(
  reference_file
)

valid_ids <- reference %>%
  select(ID) %>%
  mutate(
    ID = as.character(ID),

    # Replace German umlauts
    ID = str_replace_all(ID, c(
      "Ä" = "A",
      "Ü" = "U",
      "Ö" = "O",
      "ä" = "A",
      "ü" = "U",
      "ö" = "O"
    )),

    # Remove whitespace
    ID = str_trim(ID)
  ) %>%
  filter(!is.na(ID), ID != "") %>%
  distinct(ID) %>%
  pull(ID)

# ------------------------------------------------------------
# 3. FIND ALL RAW CSV FILES
# ------------------------------------------------------------

files <- list.files(
  path = raw_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)


# ------------------------------------------------------------
# 4. EXTRACT THE ID FROM EACH RAW FILE
# ------------------------------------------------------------

results <- lapply(files, function(file) {

  data <- read_csv(
    file,
    show_col_types = FALSE
  )

  # Check that participant column exists
  if (!"participant" %in% names(data)) {
    return(data.frame(
      file = basename(file),
      original_ID = NA_character_,
      clean_ID = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # First value in participant column
  participant <- as.character(data$participant[1])

  # Take everything before the first "_"
  clean_id <- str_extract(
    participant,
    "^[^_]+"
  )

  # Replace German umlauts
  clean_id <- str_replace_all(clean_id, c(
    "Ä" = "A",
    "Ü" = "U",
    "Ö" = "O",
    "ä" = "A",
    "ü" = "U",
    "ö" = "O"
  ))

  # Remove whitespace
  clean_id <- str_trim(clean_id)

  data.frame(
    file = basename(file),
    original_ID = participant,
    clean_ID = clean_id,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()


# ------------------------------------------------------------
# 5. FIND EXACT MATCHES
# ------------------------------------------------------------

results <- results %>%
  mutate(
    exact_match = clean_ID %in% valid_ids
  )


# ------------------------------------------------------------
# 6. FUZZY MATCHING FOR INCORRECT IDs
# ------------------------------------------------------------

# This calculates the edit distance between an incorrect ID
# and every valid ID.

find_best_match <- function(id, valid_ids) {

  if (is.na(id) || id == "") {
    return(data.frame(
      proposed_ID = NA_character_,
      distance = NA_integer_
    ))
  }

  distances <- adist(id, valid_ids)

  best <- which.min(distances)

  data.frame(
    proposed_ID = valid_ids[best],
    distance = distances[best]
  )
}


fuzzy_results <- lapply(
  results$clean_ID,
  find_best_match,
  valid_ids = valid_ids
) %>%
  bind_rows()


results <- bind_cols(
  results,
  fuzzy_results
)


# ------------------------------------------------------------
# 7. DECIDE WHETHER TO ACCEPT A CORRECTION
# ------------------------------------------------------------

results <- results %>%
  mutate(

    # IDs are expected to have exactly 8 characters
    correct_length = nchar(clean_ID) == 8,

    # Automatically accept:
    #   - exact matches
    #   - IDs only one character away from a valid ID
    #
    # More distant matches are NOT automatically changed.
    proposed_action = case_when(

      exact_match ~ "KEEP",

      !exact_match & distance == 1 ~ "CORRECT",

      !exact_match & distance > 1 ~ "REVIEW",

      TRUE ~ "REVIEW"
    ),

    final_ID = case_when(

      proposed_action == "KEEP" ~ clean_ID,

      proposed_action == "CORRECT" ~ proposed_ID,

      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 8. CHECK HOW MANY FILES EACH ID HAS
# ------------------------------------------------------------

id_counts <- results %>%
  filter(!is.na(final_ID)) %>%
  count(final_ID, name = "number_of_files") %>%
  arrange(final_ID)


# IDs with something other than exactly 2 files
file_count_problems <- id_counts %>%
  filter(number_of_files != 2)


# ------------------------------------------------------------
# 9. CREATE A REVIEW TABLE
# ------------------------------------------------------------

review <- results %>%
  select(
    file,
    original_ID,
    clean_ID,
    proposed_ID,
    distance,
    correct_length,
    proposed_action,
    final_ID
  ) %>%
  arrange(
    proposed_action,
    final_ID,
    file
  )


# ------------------------------------------------------------
# 10. PRINT RESULTS TO R
# ------------------------------------------------------------

cat("\n============================================\n")
cat("ID CHECK COMPLETE\n")
cat("============================================\n\n")

cat("Total raw files:", nrow(results), "\n")
cat("Exact matches:", sum(results$proposed_action == "KEEP"), "\n")
cat("Automatic corrections:", sum(results$proposed_action == "CORRECT"), "\n")
cat("Files requiring review:", sum(results$proposed_action == "REVIEW"), "\n\n")

cat("============================================\n")
cat("FILES REQUIRING REVIEW\n")
cat("============================================\n\n")

print(
  review %>%
    filter(proposed_action == "REVIEW")
)

cat("\n============================================\n")
cat("PARTICIPANTS WITHOUT EXACTLY 2 FILES\n")
cat("============================================\n\n")

print(file_count_problems)


# ------------------------------------------------------------
# 11. SAVE REVIEW RESULTS
# ------------------------------------------------------------

# This creates a separate Excel file for you to inspect.
# It does NOT modify any raw files.

write_xlsx(
  list(
    all_files = review,
    ID_counts = id_counts,
    count_problems = file_count_problems
  ),
  "participant_ID_review.xlsx"
)


# ============================================================
# STOP HERE!
#
# Open "participant_ID_review.xlsx" and check everything.
#
# ONLY AFTER YOU ARE HAPPY:
#
# Change:
#
# APPLY_CORRECTIONS <- FALSE
#
# to:
#
# APPLY_CORRECTIONS <- TRUE
#
# Then run the script again.
# ============================================================


# ------------------------------------------------------------
# 12. CREATE CORRECTED COPIES
# ------------------------------------------------------------

if (APPLY_CORRECTIONS) {

  # Create NEW folder
  dir.create(
    corrected_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )

  for (i in seq_along(files)) {

    file <- files[i]

    # Skip files that need manual review
    if (results$proposed_action[i] == "REVIEW") {
      next
    }

    # Read original file again
    data <- read_csv(
      file,
      show_col_types = FALSE
    )

    # Correct participant column
    data$participant <- results$final_ID[i]

    # Save as a NEW file
    write_csv(
      data,
      file.path(
        corrected_folder,
        basename(file)
      )
    )
  }

  cat("\n============================================\n")
  cat("CORRECTED FILES SAVED\n")
  cat("============================================\n")
  cat("Location:", corrected_folder, "\n")
  cat("Original raw files were NOT changed.\n")
}