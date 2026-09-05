#==== adjusting IDs ====#
library(readr)
library(dplyr)
library(stringr)

standardize_id <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all(c(
      "Ä" = "A",
      "Ö" = "O",
      "Ü" = "U"
    ))
}

# Read raw files
data_2024 <- read_csv2("data/raw/balance/2024/Balance_output_2024.csv")
data_2026 <- read_csv2("data/raw/balance/2026/balance_output_2026.csv")

# Standardize IDs
data_2024 <- data_2024 %>%
  mutate(ID = standardize_id(ID))

data_2026 <- data_2026 %>%
  mutate(ID = standardize_id(ID))

# Save processed versions
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write_csv(data_2024, "data/processed/balance/balance_2024.csv")
write_csv(data_2026, "data/processed/balance/balance_2026.csv")

#==== merging ====#

# 1. Check for columns only in one dataset
setdiff(names(data_2024), names(data_2026))
setdiff(names(data_2026), names(data_2024))

# 2. Check whether they contain the same set of columns
setequal(names(data_2024), names(data_2026))

# 3. Check the structure / data types
sapply(data_2024, class)
sapply(data_2026, class)

# 4. Check number of rows
nrow(data_2024)
nrow(data_2026)

# 5. Add year vairable to distinguish later -> ### will probably be deleted later
data_2024 <- data_2024 %>%
  mutate(year = 2024)

data_2026 <- data_2026 %>%
  mutate(year = 2026)

data_all <- bind_rows(data_2024, data_2026)

# 6. merge the files
data_all <- bind_rows(data_2024, data_2026)

# 7. Create output folder if necessary
dir.create("data/processed/balance/merged", recursive = TRUE, showWarnings = FALSE)

# 8. Save combined dataset
write_csv(data_all, "data/processed/balance/merged/data_merged.csv")
