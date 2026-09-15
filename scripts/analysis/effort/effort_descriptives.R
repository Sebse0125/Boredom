library(readr)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Setting file directories
input_path <- "analysis/files/effort/effort_all_timepoints_long.csv"
output_dir <- "figures/effort"

table_csv_path <- file.path(output_dir, "effort_descriptive_table.csv")
table_docx_path <- file.path(output_dir, "effort_descriptive_table_APA.docx")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading the prepared effort data
effort_data <- read_csv(input_path, show_col_types = FALSE)

# Checking variables needed for the descriptive table
required_columns <- c(
  "ID", "Condition", "Timepoint", "Timepoint_number", "Effort"
)

missing_columns <- setdiff(required_columns, names(effort_data))

if (length(missing_columns) > 0) {
  stop(
    "Required column(s) missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Calculating descriptives separately for every condition and timepoint
effort_summary_long <- effort_data %>%
  group_by(Timepoint_number, Timepoint, Condition) %>%
  summarise(
    n_total = n(),
    n_valid = sum(!is.na(Effort)),
    Missing = sum(is.na(Effort)),
    Mean = if_else(n_valid > 0, mean(Effort, na.rm = TRUE), NA_real_),
    SD = if_else(n_valid > 1, sd(Effort, na.rm = TRUE), NA_real_),
    Minimum = if_else(n_valid > 0, min(Effort, na.rm = TRUE), NA_real_),
    Maximum = if_else(n_valid > 0, max(Effort, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Creating a clear HB-versus-LB table
effort_summary <- effort_summary_long %>%
  select(
    Timepoint_number, Timepoint, Condition,
    Mean, SD, Minimum, Maximum, n_valid, Missing
  ) %>%
  pivot_wider(
    names_from = Condition,
    values_from = c(Mean, SD, Minimum, Maximum, n_valid, Missing),
    names_glue = "{Condition}_{.value}"
  ) %>%
  arrange(Timepoint_number) %>%
  transmute(
    Timepoint,
    `HB M (SD)` = if_else(
      is.na(HB_Mean),
      "—",
      sprintf("%.2f (%.2f)", HB_Mean, HB_SD)
    ),
    `HB Range` = if_else(
      is.na(HB_Minimum),
      "—",
      sprintf("%.0f–%.0f", HB_Minimum, HB_Maximum)
    ),
    `HB n` = HB_n_valid,
    `HB Missing` = HB_Missing,
    `LB M (SD)` = if_else(
      is.na(LB_Mean),
      "—",
      sprintf("%.2f (%.2f)", LB_Mean, LB_SD)
    ),
    `LB Range` = if_else(
      is.na(LB_Minimum),
      "—",
      sprintf("%.0f–%.0f", LB_Minimum, LB_Maximum)
    ),
    `LB n` = LB_n_valid,
    `LB Missing` = LB_Missing
  )

# Saving a plain CSV version for checking and reuse
write_csv(effort_summary, table_csv_path, na = "")

# Formatting the table for an APA-style Word document
apa_table <- flextable(effort_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(j = "Timepoint", align = "left", part = "all") %>%
  align(j = setdiff(names(effort_summary), "Timepoint"),
        align = "center", part = "all") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  autofit()

apa_document <- read_docx() %>%
  body_add_fpar(
    fpar(ftext("Table 1", prop = fp_text(bold = TRUE, font.family = "Aptos")))
  ) %>%
  body_add_fpar(
    fpar(ftext(
      "Descriptive Statistics for Effort Ratings by Condition and Timepoint",
      prop = fp_text(italic = TRUE, font.family = "Aptos")
    ))
  ) %>%
  body_add_flextable(apa_table) %>%
  body_add_fpar(
    fpar(ftext(
      paste0(
        "Note. Effort was rated from 0 to 10. HB = high-boredom condition; ",
        "LB = low-boredom condition; M = mean; SD = standard deviation. ",
        "Sample sizes vary because missing ratings were not imputed."
      ),
      prop = fp_text(font.family = "Aptos")
    ))
  )

print(apa_document, target = table_docx_path)

message("Descriptive table data written: ", table_csv_path)
message("APA-style Word table written: ", table_docx_path)
