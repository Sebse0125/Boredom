# =========================================================
# PSYCHOPY MOVEMENT DATA EXTRACTION
# ONE FILE AT A TIME
# =========================================================

library(readr)
library(dplyr)
library(stringr)


# ---------------------------------------------------------
# 1. Read the PsychoPy file
# ---------------------------------------------------------

psychopy <- read_csv(
  "data/raw/psychopy/2026/DO11UB30_A_LB_Balance_and_VR_A_2026-06-15_15h29.06.514.csv",
  show_col_types = FALSE
)


# ---------------------------------------------------------
# 2. Find the four clicked_name columns
# ---------------------------------------------------------

click_columns <- names(psychopy)[
  str_detect(
    names(psychopy),
    "^Klicks_Block_[AB]_0[1-4]\\.clicked_name$"
  )
]


# Show which columns were found
print(click_columns)


# ---------------------------------------------------------
# 3. Check that exactly four columns were found
# ---------------------------------------------------------

if (length(click_columns) != 4) {
  
  stop(
    paste0(
      "Expected 4 clicked_name columns, but found ",
      length(click_columns),
      "."
    )
  )
}


# ---------------------------------------------------------
# 4. Determine A/B internally
# ---------------------------------------------------------

order_condition <- str_extract(
  click_columns[1],
  "(?<=Klicks_Block_)[AB]"
)

print(paste("PsychoPy block:", order_condition))


# ---------------------------------------------------------
# 5. Get the actual experimental condition
# ---------------------------------------------------------

actual_condition <- psychopy %>%
  filter(!is.na(Condition), Condition != "") %>%
  pull(Condition) %>%
  as.character() %>%
  first()


# Check the condition
print(paste("Experimental condition:", actual_condition))


# ---------------------------------------------------------
# 6. Check that the condition is HB or LB
# ---------------------------------------------------------

if (!actual_condition %in% c("HB", "LB")) {
  
  stop(
    paste0(
      "Unexpected value in 'Condition' column: ",
      actual_condition
    )
  )
}


# ---------------------------------------------------------
# 7. Extract participant/measurement information
# ---------------------------------------------------------

general_columns <- c(
  "participant",
  "session",
  "Condition",
  "date",
  "expName",
  "psychopyVersion"
)


general_data <- psychopy %>%
  
  select(all_of(general_columns)) %>%
  
  summarise(
    across(
      everything(),
      ~ {
        
        x <- as.character(.x)
        
        x <- x[!is.na(x) & x != ""]
        
        if (length(x) == 0) {
          NA_character_
        } else {
          x[1]
        }
      }
    )
  )


# ---------------------------------------------------------
# 8. Extract the first non-empty value from each
#    clicked_name column
# ---------------------------------------------------------

movement_data <- psychopy %>%
  
  select(all_of(click_columns)) %>%
  
  summarise(
    across(
      everything(),
      ~ {
        
        x <- as.character(.x)
        
        x <- x[!is.na(x) & x != ""]
        
        if (length(x) == 0) {
          NA_character_
        } else {
          x[1]
        }
      }
    )
  )


# ---------------------------------------------------------
# 9. Count movements for each trial
# ---------------------------------------------------------

for (i in 1:4) {
  
  # PsychoPy trial number: 01, 02, 03, 04
  trial <- sprintf("%02d", i)
  
  
  # Name of the clicked_name column
  actual_column <- paste0(
    "Klicks_Block_",
    order_condition,
    "_",
    trial,
    ".clicked_name"
  )
  
  
  # Extract clicks for this trial
  clicks <- movement_data[[actual_column]]
  
  
  # -------------------------------------------------------
  # Count arm movements
  # -------------------------------------------------------
  
  movement_data[[paste0(
    "arms_", actual_condition, "_", trial
  )]] <- str_count(
    clicks,
    paste0(
      "Arms_Block_",
      order_condition,
      "_",
      trial
    )
  )
  
  
  # -------------------------------------------------------
  # Count leg movements
  # -------------------------------------------------------
  
  movement_data[[paste0(
    "legs_", actual_condition, "_", trial
  )]] <- str_count(
    clicks,
    paste0(
      "Legs_Block_",
      order_condition,
      "_",
      trial
    )
  )
  
  
  # -------------------------------------------------------
  # Count head movements
  # -------------------------------------------------------
  
  movement_data[[paste0(
    "head_", actual_condition, "_", trial
  )]] <- str_count(
    clicks,
    paste0(
      "Head_Block_",
      order_condition,
      "_",
      trial
    )
  )
  
  
  # -------------------------------------------------------
  # Count other movements
  # -------------------------------------------------------
  
  movement_data[[paste0(
    "other_", actual_condition, "_", trial
  )]] <- str_count(
    clicks,
    paste0(
      "Other_Block_",
      order_condition,
      "_",
      trial
    )
  )
  
  
  # -------------------------------------------------------
  # Calculate total movements
  # -------------------------------------------------------
  
  movement_data[[paste0(
    "total_click_",
    actual_condition,
    "_",
    trial
  )]] <-
    
    movement_data[[paste0(
      "arms_", actual_condition, "_", trial
    )]] +
    
    movement_data[[paste0(
      "legs_", actual_condition, "_", trial
    )]] +
    
    movement_data[[paste0(
      "head_", actual_condition, "_", trial
    )]] +
    
    movement_data[[paste0(
      "other_", actual_condition, "_", trial
    )]]
}


# ---------------------------------------------------------
# 10. Create movement column names in trial order
# ---------------------------------------------------------

movement_columns <- unlist(
  
  lapply(
    
    sprintf("%02d", 1:4),
    
    function(trial) {
      
      c(
        paste0("arms_", actual_condition, "_", trial),
        paste0("legs_", actual_condition, "_", trial),
        paste0("head_", actual_condition, "_", trial),
        paste0("other_", actual_condition, "_", trial),
        paste0("total_click_", actual_condition, "_", trial)
      )
      
    }
  )
)


# ---------------------------------------------------------
# 11. Keep movement columns in trial order
# ---------------------------------------------------------

movement_data <- movement_data %>%
  select(all_of(movement_columns))


# ---------------------------------------------------------
# 12. Combine participant information and movement data
# ---------------------------------------------------------

movement_data <- bind_cols(
  general_data,
  movement_data
)


# ---------------------------------------------------------
# 13. View final result
# ---------------------------------------------------------

movement_data


length(list.files("data/raw/psychopy/2024"))
length(list.files("data/processed/psychopy/2024/"))

length(list.files("data/raw/psychopy/2026"))
length(list.files("data/processed/psychopy/2026"))
