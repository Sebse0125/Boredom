library(readr)
library(dplyr)
library(stringr)

output_folder <- "data/inter/limesurvey"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

remove_columns <- function(input_file, output_file, columns_to_remove) {

  data <- read_delim(
  input_file,
  delim = ";",
  show_col_types = FALSE,
  name_repair = "minimal"
)


  missing_columns <- setdiff(columns_to_remove, names(data))

  if (length(missing_columns) > 0) {
    possible_matches <- names(data)[
      str_detect(
        names(data),
        regex("tester|researcher", ignore_case = TRUE)
      )
    ]

    stop(
      "These requested columns were not found:\n",
      paste(missing_columns, collapse = "\n"),
      "\n\nPossible researcher columns in the file:\n",
      paste(possible_matches, collapse = "\n")
    )
  }

  cleaned_data <- data %>%
    select(-all_of(columns_to_remove))

  # Final safeguard
  if (any(columns_to_remove %in% names(cleaned_data))) {
    stop("One or more researcher columns were not removed.")
  }

  write_csv(cleaned_data, output_file)

  message("Created: ", output_file)
}

#2024
remove_columns(
  input_file = "data/raw/limesurvey/2024/limesurvey_2024.csv",
  output_file = "data/inter/limesurvey/limesurvey_2024_deidentified.csv",
  columns_to_remove = c(
    "tester[Researcher1_SQ001]",
    "tester[Researcher2_SQ001]"
  )
)

#2026
remove_columns(
  input_file = "data/raw/limesurvey/2026/limesurvey_2026.csv",
  output_file = "data/inter/limesurvey/limesurvey_2026_deidentified.csv",
  columns_to_remove = c(
    paste0(
      "Who are the testers? please type the full name --&gt; Tina Baus  Researcher 1: Psychopy + VR Steam  Researcher 2: instructor  [Researcher 1][]"
    ),
    paste0(
      "Who are the testers? please type the full name --&gt; Tina Baus  Researcher 1: Psychopy + VR Steam  Researcher 2: instructor  [Researcher 2][]"
    )
  )
)
