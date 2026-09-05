library(readr)
library(dplyr)
library(stringr)


# ---------------------------------------------------------
# 1. Read the LimeSurvey file
# ---------------------------------------------------------

limesurvey2026 <- read_csv(
  "data/raw/limesurvey/2026/LimeSurvey_2026.csv",
  show_col_types = FALSE
)

write_csv2(
  limesurvey2026,
  "data/raw/limesurvey/2026/limesurvey_2026.csv"
)

limesurvey2024 <- read_csv(
  "data/raw/limesurvey/2024/LimeSurvey_2024.csv",
  show_col_types = FALSE
)

write_csv2(
  limesurvey2024,
  "data/raw/limesurvey/2024/limesurvey_2024.csv"
)

# 1. read file
# 2. clear testing rows
# 3. check IDs
# 4. choose important columns