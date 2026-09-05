# =========================================================

# PSYCHOPY STAGE 2

# MOVEMENT DATA EXTRACTION + MASTER FILE

#

# Input:

# data/raw/psychopy/2024

# data/raw/psychopy/2026

#

# data/meta/psychopy/psychopy_metadata.csv

#

# Output:

# data/processed/psychopy/master/psychopy_master.csv

#

# IMPORTANT:

# - Raw PsychoPy files are NEVER modified.

# - Stage 1 metadata is NEVER modified.

# - Known raw-file naming errors are handled through

# the exception table below.

# =========================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# =========================================================

# 1. DEFINE PATHS

# =========================================================

metadata_file <-
"data/meta/psychopy/psychopy_metadata.csv"

output_folder <-
"data/processed/psychopy/master"

output_file <-
file.path(
output_folder,
"psychopy_master.csv"
)

# =========================================================

# 2. READ STAGE 1 METADATA

# =========================================================

psychopy_metadata <- read_csv(
metadata_file,
show_col_types = FALSE
)

print(
paste(
"Metadata records:",
nrow(psychopy_metadata)
)
)

# =========================================================

# 3. KNOWN RAW-FILE EXCEPTIONS

# =========================================================

#

# These corrections refer ONLY to mistakes in the naming

# of columns INSIDE individual raw PsychoPy files.

#

# They do NOT change the actual experimental Block.

#

# Example:

#

# LE11ON12:

#

# true measurement Block = B

# raw click columns      = A

#

# Therefore:

#

# Block          = B

# raw_click_block = A

#

# =========================================================

raw_block_corrections <- tibble(

source_file = c(
"LE11ON12_B_LB__Balance_and_VR_A_2024-06-24_09h45.48.708.csv"
),

raw_click_block = c(
"A"
),

correction_reason = c(
"Measurement is Block B, but PsychoPy clicked_name columns were incorrectly labelled as Block A."

)
)

# =========================================================

# 4. ADD RAW CLICK BLOCK TO METADATA

# =========================================================

#

# Normally:

#

# raw_click_block = Block

#

# Only known exceptions override this.

# =========================================================

psychopy_metadata <- psychopy_metadata %>%

left_join(
raw_block_corrections %>%
  select(
    source_file,
    raw_click_block,
    correction_reason
  ),

by = "source_file"
) %>%

mutate(
raw_click_block = if_else(
  is.na(raw_click_block),
  Block,
  raw_click_block
)
)

# =========================================================

# 5. CHECK REQUIRED METADATA COLUMNS

# =========================================================

required_metadata_columns <- c(

"source_file",
"source_path",
"source_year",
"participant",
"Block",
"Condition"

)

missing_metadata_columns <- setdiff(

required_metadata_columns,
names(psychopy_metadata)

)

if (length(missing_metadata_columns) > 0) {

stop(
paste0(
  "The metadata file is missing these columns: ",
  
  paste(
    missing_metadata_columns,
    collapse = ", "
  )
  
)

)
}

# =========================================================

# 6. CHECK FOR UNRESOLVED METADATA

# =========================================================

unresolved_metadata <- psychopy_metadata %>%

filter(
is.na(participant) |
  is.na(Block) |
  is.na(Condition)

)

if (nrow(unresolved_metadata) > 0) {

print("")
print("==============================================")
print("UNRESOLVED METADATA")
print("==============================================")

print(
unresolved_metadata %>%
select(
source_file,
participant,
Block,
Condition,
status,
review_reason
)
)

stop(
"Stage 2 stopped because some files have incomplete metadata."
)
}

# =========================================================

# 7. CHECK VALID BLOCK AND CONDITION VALUES

# =========================================================

invalid_block <- psychopy_metadata %>%

filter(
!Block %in% c("A", "B")
)

if (nrow(invalid_block) > 0) {

print(invalid_block)

stop(
"Invalid Block value found in Stage 1 metadata."
)
}

invalid_condition <- psychopy_metadata %>%

filter(
!Condition %in% c("HB", "LB")
)

if (nrow(invalid_condition) > 0) {

print(invalid_condition)

stop(
"Invalid Condition value found in Stage 1 metadata."
)
}

# =========================================================

# 8. CHECK RAW CLICK BLOCK VALUES

# =========================================================

invalid_raw_click_block <- psychopy_metadata %>%

filter(
!raw_click_block %in% c("A", "B")
)

if (nrow(invalid_raw_click_block) > 0) {

print(invalid_raw_click_block)

stop(
"Invalid raw_click_block value found."
)
}

# =========================================================

# 9. FUNCTION TO EXTRACT MOVEMENTS FROM ONE FILE

# =========================================================

extract_movements <- function(
file_path,
raw_click_block,
condition,
measurement_block
) {

# -------------------------------------------------------

# Read raw PsychoPy file

# -------------------------------------------------------

psychopy <- read_csv(
file_path,
show_col_types = FALSE
)

# -------------------------------------------------------

# Find the four clicked_name columns

#

# IMPORTANT:

#

# raw_click_block is used here.

#

# This means the function can handle:

#

# true Block B

# but raw columns labelled Block A.

# -------------------------------------------------------

click_pattern <- paste0(
"^Klicks_Block_",
raw_click_block,
"_0[1-4]\\.clicked_name$"
)

click_columns <- names(psychopy)[
str_detect(
  names(psychopy),
  click_pattern
)
]

# -------------------------------------------------------

# Check that exactly four columns were found

# -------------------------------------------------------

if (length(click_columns) != 4) {
stop(
  paste0(
    
    "File: ",
    basename(file_path),
    "\n",
    
    "Measurement Block: ",
    measurement_block,
    "\n",
    
    "Raw click Block expected: ",
    raw_click_block,
    "\n",
    
    "Expected 4 clicked_name columns, but found ",
    length(click_columns),
    "."
    
  )
)
}

# -------------------------------------------------------

# Sort columns into trial order

# -------------------------------------------------------

click_trials <- str_extract(
click_columns,
"0[1-4](?=\\.clicked_name)"
)

click_columns <- click_columns[
order(
  as.integer(click_trials)
)
]

# =======================================================

# Extract first non-empty value from each click column

# =======================================================

movement_data <- psychopy %>%
select(
  all_of(click_columns)
) %>%

summarise(
  
  across(
    
    everything(),
    
    ~ {
      
      x <- as.character(.x)
      
      x <- x[
        !is.na(x) &
          x != ""
      ]
      
      if (length(x) == 0) {
        
        NA_character_
        
      } else {
        
        x[1]
        
      }
      
    }
    
  )
  
)
# =======================================================

# COUNT MOVEMENTS FOR EACH TRIAL

# =======================================================

for (i in 1:4) {
trial <- sprintf(
  "%02d",
  i
)

# -----------------------------------------------------
# Actual raw clicked_name column
# -----------------------------------------------------

actual_column <- paste0(
  
  "Klicks_Block_",
  raw_click_block,
  "_",
  trial,
  ".clicked_name"
  
)


clicks <- movement_data[[actual_column]]


# -----------------------------------------------------
# Arms
# -----------------------------------------------------

movement_data[[paste0(
  
  "arms_",
  condition,
  "_",
  measurement_block,
  "_",
  trial
  
)]] <- str_count(
  
  clicks,
  
  paste0(
    "Arms_Block_",
    raw_click_block,
    "_",
    trial
  )
  
)


# -----------------------------------------------------
# Legs
# -----------------------------------------------------

movement_data[[paste0(
  
  "legs_",
  condition,
  "_",
  measurement_block,
  "_",
  trial
  
)]] <- str_count(
  
  clicks,
  
  paste0(
    "Legs_Block_",
    raw_click_block,
    "_",
    trial
  )
  
)


# -----------------------------------------------------
# Head
# -----------------------------------------------------

movement_data[[paste0(
  
  "head_",
  condition,
  "_",
  measurement_block,
  "_",
  trial
  
)]] <- str_count(
  
  clicks,
  
  paste0(
    "Head_Block_",
    raw_click_block,
    "_",
    trial
  )
  
)


# -----------------------------------------------------
# Other
# -----------------------------------------------------

movement_data[[paste0(
  
  "other_",
  condition,
  "_",
  measurement_block,
  "_",
  trial
  
)]] <- str_count(
  
  clicks,
  
  paste0(
    "Other_Block_",
    raw_click_block,
    "_",
    trial
  )
  
)


# -----------------------------------------------------
# Total
# -----------------------------------------------------

movement_data[[paste0(
  
  "total_click_",
  condition,
  "_",
  measurement_block,
  "_",
  trial
  
)]] <-
  
  movement_data[[paste0(
    "arms_",
    condition,
    "_",
    measurement_block,
    "_",
    trial
  )]] +
  
  movement_data[[paste0(
    "legs_",
    condition,
    "_",
    measurement_block,
    "_",
    trial
  )]] +
  
  movement_data[[paste0(
    "head_",
    condition,
    "_",
    measurement_block,
    "_",
    trial
  )]] +
  
  movement_data[[paste0(
    "other_",
    condition,
    "_",
    measurement_block,
    "_",
    trial
  )]]
}

# =======================================================

# Keep movement columns in trial order

# =======================================================

movement_columns <- unlist(
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
        measurement_block,
        "_",
        trial
      ),
      
      paste0(
        "legs_",
        condition,
        "_",
        measurement_block,
        "_",
        trial
      ),
      
      paste0(
        "head_",
        condition,
        "_",
        measurement_block,
        "_",
        trial
      ),
      
      paste0(
        "other_",
        condition,
        "_",
        measurement_block,
        "_",
        trial
      ),
      
      paste0(
        "total_click_",
        condition,
        "_",
        measurement_block,
        "_",
        trial
      )
      
    )
    
  }
  
)
)

movement_data %>%
select(
  all_of(movement_columns)
)
}

# =========================================================

# 10. PROCESS ALL FILES

# =========================================================

print("")
print("==============================================")
print("STARTING MOVEMENT EXTRACTION")
print("==============================================")

file_level_data <- map_dfr(

1:nrow(psychopy_metadata),

function(i) {
metadata <- psychopy_metadata[i, ]


print("")
print(
  paste(
    "[",
    i,
    "/",
    nrow(psychopy_metadata),
    "]",
    metadata$source_file
  )
)


# -----------------------------------------------------
# Report raw-file exception if one exists
# -----------------------------------------------------

if (!is.na(metadata$correction_reason)) {
  
  print(
    paste(
      "  NOTE:",
      metadata$correction_reason
    )
  )
  
}


# -----------------------------------------------------
# Extract movements
#
# IMPORTANT:
#
# raw_click_block:
#   tells us how the raw columns are named
#
# Block:
#   tells us what the measurement actually represents
# -----------------------------------------------------
#small correction
  correction <- raw_block_corrections %>%
  filter(source_file == metadata$source_file)

if (nrow(correction) == 1) {
  raw_click_block <- correction$raw_click_block[[1]]
} else {
  raw_click_block <- metadata$Block
}
  
movement_data <- extract_movements(
  
  file_path = metadata$source_path,
  
  raw_click_block = metadata$raw_click_block,
  
  condition = metadata$Condition,
  
  measurement_block = metadata$Block
  
)


# -----------------------------------------------------
# Add metadata to movement row
# -----------------------------------------------------

bind_cols(
  
  metadata %>%
    
    select(
      
      source_file,
      source_path,
      source_year,
      participant,
      Block,
      Condition
      
    ),
  
  movement_data
  
)
}

)

# =========================================================

# 11. CHECK THAT EVERY FILE PRODUCED ONE ROW

# =========================================================

if (
nrow(file_level_data) !=
nrow(psychopy_metadata)
) {

stop(
paste0(
  "The number of extracted rows (",
  nrow(file_level_data),
  
  ") does not match the number of metadata rows (",
  nrow(psychopy_metadata),
  
  ")."
  
)

)
}

# =========================================================

# 12. CREATE MEASUREMENT IDENTIFIER

# =========================================================

#

# This uses the TRUE measurement Block,

# NOT raw_click_block.

#

# Therefore:

#

# HB + A = HB_A

# HB + B = HB_B

# LB + A = LB_A

# LB + B = LB_B

# =========================================================

file_level_data <- file_level_data %>%

mutate(
measurement = paste(
  Condition,
  Block,
  sep = "_"
)
)

# =========================================================

# 13. CHECK FOR DUPLICATE PARTICIPANT / MEASUREMENT PAIRS

# =========================================================

duplicates <- file_level_data %>%

count(
participant,
measurement
) %>%

filter(
n > 1
)

if (nrow(duplicates) > 0) {

print("")
print("==============================================")
print("DUPLICATE PARTICIPANT / MEASUREMENT PAIRS")
print("==============================================")

print(
duplicates
)

stop(
"At least one participant has more than one file for the same Condition + Block."
)
}

# =========================================================

# 14. PREPARE PARTICIPANT-LEVEL METADATA

# =========================================================

participant_metadata <- file_level_data %>%

group_by(
participant
) %>%

summarise(
source_year = paste(
  sort(unique(source_year)),
  collapse = ";"
),

.groups = "drop"
)

# =========================================================

# 15. EXTRACT MOVEMENT VARIABLES

# =========================================================

movement_long <- file_level_data %>%

select(
participant,
measurement,

starts_with("arms_"),
starts_with("legs_"),
starts_with("head_"),
starts_with("other_"),
starts_with("total_click_")
) %>%

pivot_longer(
cols = -c(
  participant,
  measurement
),

names_to = "variable",

values_to = "value"

)

# =========================================================

# 16. REMOVE DUPLICATE CONDITION PART

# =========================================================

#

# At this point variables look like:

#

# arms_HB_A_01

#

# and measurement is:

#

# HB_A

#

# We remove the existing HB_A part and add it back

# systematically.

# =========================================================

movement_long <- movement_long %>%

mutate(
variable = str_replace(
  
  variable,
  
  "^(arms|legs|head|other|total_click)_(HB|LB)_(A|B)_(\\d{2})$",
  
  "\\1_\\4"
  
),

variable = paste(
  
  variable,
  measurement,
  sep = "_"
  
),

variable = str_replace(
  
  variable,
  
  "^(arms|legs|head|other|total_click)_(\\d{2})_(HB|LB)_(A|B)$",
  
  "\\1_\\3_\\4_\\2"
  
)

)

# =========================================================

# 17. CONVERT TO ONE ROW PER PARTICIPANT

# =========================================================

movement_wide <- movement_long %>%

pivot_wider(

names_from = variable,

values_from = value
)

# =========================================================

# 18. CREATE MEASUREMENT-PRESENCE INDICATORS

# =========================================================

measurement_presence <- file_level_data %>%

distinct(
participant,
measurement
) %>%

mutate(
present = 1
) %>%

pivot_wider(
names_from = measurement,

values_from = present,

names_prefix = "measurement_",

values_fill = 0
)

# =========================================================

# 19. COMBINE EVERYTHING

# =========================================================

psychopy_master <- participant_metadata %>%

left_join(
movement_wide,
by = "participant"
) %>%

left_join(
measurement_presence,
by = "participant"
)

# =========================================================

# 20. SORT BY PARTICIPANT

# =========================================================

psychopy_master <- psychopy_master %>%

arrange(
participant
)

# =========================================================

# 21. CREATE OUTPUT DIRECTORY

# =========================================================

dir.create(

output_folder,

recursive = TRUE,

showWarnings = FALSE

)

# =========================================================

# 22. WRITE MASTER FILE

# =========================================================

write_csv(

psychopy_master,

output_file

)

# =========================================================

# 23. FINAL SUMMARY

# =========================================================

print("")
print("==============================================")
print("STAGE 2 FINISHED")
print("==============================================")

print(
paste(
"Raw files processed:",
nrow(file_level_data)
)
)

print(
paste(
"Unique participants:",
n_distinct(
psychopy_master$participant
)
)
)

print(
paste(
"Final rows:",
nrow(psychopy_master)
)
)

print(
paste(
"Final columns:",
ncol(psychopy_master)
)
)

print("")
print("Measurements found:")

print(
file_level_data %>%

count(
  Condition,
  Block
) %>%

arrange(
  Condition,
  Block
)

)

print("")
print("Master file written to:")

print(
output_file
)

# =========================================================

# 24. VIEW FINAL RESULT

# =========================================================

psychopy_master
