#==== this file was used for psychopy raw data checks only ====#
#### here we ran checks and assumtion checks on contents of files in order to find out best workflows to standardise files and merge data into larger frame ####


# Replace these paths with your actual folder paths
source_folders <- c(
  "data/raw/psychopy/2024",
  "data/raw/psychopy/2026"
)

working_folder <- "data/s_files/psychopy_working_files"

# Create the destination folder if needed
dir.create(working_folder, recursive = TRUE, showWarnings = FALSE)

# Find all files in both source folders and their subfolders
files <- unlist(
  lapply(
    source_folders,
    list.files,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  ),
  use.names = FALSE
)

# Check whether different source files have the same filename
duplicate_names <- unique(basename(files)[duplicated(basename(files))])

if (length(duplicate_names) > 0) {
  stop(
    "Copy stopped because duplicate filenames were found:\n",
    paste(duplicate_names, collapse = "\n")
  )
}

# Copy files without overwriting anything already in the working folder
destination_paths <- file.path(working_folder, basename(files))

copied <- file.copy(
  from = files,
  to = destination_paths,
  overwrite = FALSE,
  copy.date = TRUE
)

# Show a summary
cat("Files found:", length(files), "\n")
cat("Files copied:", sum(copied), "\n")
cat("Files not copied:", sum(!copied), "\n")


#==== standardizing file names ====#
#---- helper to consistently find Condition, Block and ID in files ----#

#repeat the same for condition
library(readr)
library(dplyr)
library(purrr)

working_folder <- "data/s_files/psychopy_working_files"
output_name <- "participant_condition_session_summary.csv"

files <- list.files(
  working_folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Prevent the output file from being read as an input file
files <- files[basename(files) != output_name]

# Helper: find a column without considering capitalization
get_first_value <- function(data, column_name) {

  matching_column <- names(data)[
    tolower(names(data)) == tolower(column_name)
  ]

  if (length(matching_column) == 0) {
    return(NA_character_)
  }

  as.character(data[[matching_column[1]]][1])
}

file_summary <- map_dfr(files, function(file) {

  tryCatch({
    dat <- read_csv(
      file,
      n_max = 1,
      show_col_types = FALSE,
      progress = FALSE
    )

    expected_columns <- c("condition", "participant", "session")
    missing_columns <- expected_columns[
      !expected_columns %in% tolower(names(dat))
    ]

    tibble(
      file_name = basename(file),
      file_path = file,
      condition = get_first_value(dat, "condition"),
      participant = get_first_value(dat, "participant"),
      session = get_first_value(dat, "session"),
      status = if (length(missing_columns) == 0) {
        "OK"
      } else {
        paste(
          "Missing column(s):",
          paste(missing_columns, collapse = ", ")
        )
      }
    )

  }, error = function(e) {
    tibble(
      file_name = basename(file),
      file_path = file,
      condition = NA_character_,
      participant = NA_character_,
      session = NA_character_,
      status = paste(
        "Could not read file:",
        conditionMessage(e)
      )
    )
  })
})

# Display all results
print(file_summary, n = Inf)

# Save as one CSV file
write_csv(
  file_summary,
  file.path(working_folder, output_name)
)
#----

files_to_zip <- list.files(
  "data/s_files/psychopy_working_files",
  full.names = TRUE
)

zip(
  zipfile = "psychopy_working_files.zip",
  files = files_to_zip
)

file.exists("psychopy_working_files.zip")
