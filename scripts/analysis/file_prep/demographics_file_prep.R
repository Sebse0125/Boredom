library(readr)
library(dplyr)

dir.create("analysis/files", recursive = TRUE, showWarnings = FALSE)

master <- read_csv(
  "data/processed/master/master_with_boredom.csv",
  show_col_types = FALSE
)

limesurvey <- read_csv(
  "data/processed/master/limesurvey_final_with_boredom.csv",
  show_col_types = FALSE
)

demographics <- master %>%
  select(ID, height_cm, weight_kg) %>%
  left_join(
    limesurvey %>% select(ID, age, gender),
    by = "ID"
  ) %>%
  rename(
    `height (cm)` = height_cm,
    `weight (kg)` = weight_kg
  )

write_csv(
  demographics,
  "analysis/files/demographics.csv",
  na = ""
)
