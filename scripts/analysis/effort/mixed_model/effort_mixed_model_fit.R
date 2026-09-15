library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tibble)

# Setting file directories
input_path <- "analysis/files/effort/effort_trials_long.csv"
model_dir <- "analysis/files/effort/mixed_model"
output_dir <- "figures/effort/mixed_model"

diagnostic_table_path <- file.path(output_dir, "model_diagnostic_summary.csv")
missingness_path <- file.path(output_dir, "model_analysis_missingness.csv")
session_info_path <- file.path(model_dir, "model_session_info.txt")

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading trial-level effort data
effort_data <- read_csv(input_path, show_col_types = FALSE)

# Checking variables required for the models
required_columns <- c(
  "ID", "Condition", "Trial", "trial_order", "baseline_effort", "Effort"
)

missing_columns <- setdiff(required_columns, names(effort_data))

if (length(missing_columns) > 0) {
  stop(
    "Required column(s) missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Preparing factors and centering baseline effort
model_data <- effort_data %>%
  mutate(
    ID = factor(ID),
    Condition = factor(Condition, levels = c("HB", "LB")),
    Trial = factor(Trial, levels = 1:4),
    trial_order = factor(trial_order, levels = c("HB_LB", "LB_HB")),
    baseline_effort_c = baseline_effort -
      mean(baseline_effort, na.rm = TRUE)
  )

#invalid value checks
invalid_effort_values <- model_data %>%
  filter(
    !is.na(Effort),
    Effort < 1 | Effort > 10
  ) %>%
  select(ID, Condition, Trial, Effort)

print(invalid_effort_values, n = Inf)

#invalid value check baseline
invalid_baseline_values <- model_data %>%
  filter(
    !is.na(baseline_effort),
    baseline_effort < 1 | baseline_effort > 10
  ) %>%
  select(ID, Condition, baseline_effort) %>%
  distinct()

print(invalid_baseline_values, n = Inf)



# Checking valid ranges and required identifiers
if (any(!is.na(model_data$Effort) &
        (model_data$Effort < 0 | model_data$Effort > 10))) {
  stop("Effort contains a value outside the valid 0-10 range.")
}

if (any(!is.na(model_data$baseline_effort) &
        (model_data$baseline_effort < 0 | model_data$baseline_effort > 10))) {
  stop("baseline_effort contains a value outside the valid 0-10 range.")
}

if (any(is.na(model_data$ID)) ||
    any(is.na(model_data$Condition)) ||
    any(is.na(model_data$Trial)) ||
    any(is.na(model_data$trial_order))) {
  stop("ID, Condition, Trial, or trial_order contains an invalid/missing value.")
}

# Recording which observations are available for each model
model_missingness <- model_data %>%
  summarise(
    total_rows = n(),
    missing_effort = sum(is.na(Effort)),
    missing_baseline = sum(is.na(baseline_effort)),
    available_unadjusted = sum(!is.na(Effort)),
    available_baseline_adjusted = sum(
      !is.na(Effort) & !is.na(baseline_effort)
    ),
    participants_total = n_distinct(ID),
    participants_with_effort = n_distinct(ID[!is.na(Effort)]),
    participants_in_adjusted_model = n_distinct(
      ID[!is.na(Effort) & !is.na(baseline_effort)]
    )
  )

write_csv(model_missingness, missingness_path, na = "")

# Fitting the primary baseline-adjusted random-intercept model
model_adjusted_intercept <- lmer(
  Effort ~ Condition + Trial + trial_order + baseline_effort_c + (1 | ID),
  data = model_data,
  REML = TRUE,
  na.action = na.exclude,
  control = lmerControl(optimizer = "bobyqa")
)

# Fitting an unadjusted sensitivity model
model_unadjusted_intercept <- lmer(
  Effort ~ Condition + Trial + trial_order + (1 | ID),
  data = model_data,
  REML = TRUE,
  na.action = na.exclude,
  control = lmerControl(optimizer = "bobyqa")
)

# Fitting a random-slope candidate model
# This allows the HB-LB difference to vary between participants.
model_adjusted_slope <- lmer(
  Effort ~ Condition + Trial + trial_order + baseline_effort_c +
    (1 + Condition | ID),
  data = model_data,
  REML = TRUE,
  na.action = na.exclude,
  control = lmerControl(optimizer = "bobyqa")
)

# Saving model objects for later tables, contrasts, and figures
saveRDS(
  model_adjusted_intercept,
  file.path(model_dir, "model_adjusted_intercept.rds")
)
saveRDS(
  model_unadjusted_intercept,
  file.path(model_dir, "model_unadjusted_intercept.rds")
)
saveRDS(
  model_adjusted_slope,
  file.path(model_dir, "model_adjusted_slope.rds")
)

# Extracting convergence messages in a readable form
convergence_message <- function(model) {
  messages <- model@optinfo$conv$lme4$messages

  if (is.null(messages)) {
    "None"
  } else {
    paste(messages, collapse = " | ")
  }
}

# Summarising diagnostic information for model selection
model_list <- list(
  adjusted_random_intercept = model_adjusted_intercept,
  unadjusted_random_intercept = model_unadjusted_intercept,
  adjusted_random_slope = model_adjusted_slope
)

#qick check for model list (nobs)
lapply(model_list, class)

model_diagnostics <- bind_rows(
  lapply(names(model_list), function(model_name) {

    fitted_model <- model_list[[model_name]]

    tibble(
      model = model_name,
      observations = nrow(fitted_model@frame),
      participants = n_distinct(fitted_model@frame$ID),
      AIC = AIC(fitted_model),
      BIC = BIC(fitted_model),
      singular_fit = isSingular(fitted_model, tol = 1e-4),
      convergence_message = convergence_message(fitted_model)
    )
  })
)

write_csv(model_diagnostics, diagnostic_table_path, na = "")

# Creating residual and Q-Q diagnostic plots for each candidate model
save_diagnostic_plot <- function(model, model_name) {
  png_path <- file.path(output_dir, paste0(model_name, "_diagnostics.png"))

  png(
    filename = png_path,
    width = 2400,
    height = 1200,
    res = 300,
    bg = "white"
  )

  par(mfrow = c(1, 2), family = "sans", mar = c(5, 5, 3, 1))

  plot(
    fitted(model),
    resid(model),
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals versus fitted values",
    pch = 16,
    col = "#0072B280"
  )
  abline(h = 0, lty = 2)

  qqnorm(
    resid(model),
    main = "Normal Q-Q plot of residuals",
    pch = 16,
    col = "#0072B280"
  )
  qqline(resid(model), lty = 2)

  dev.off()
}

invisible(lapply(names(model_list), function(model_name) {
  save_diagnostic_plot(model_list[[model_name]], model_name)
}))

# Saving package and R version information for reproducibility
writeLines(capture.output(sessionInfo()), session_info_path)

message("Model objects written to: ", model_dir)
message("Diagnostic outputs written to: ", output_dir)
message(
  "Review singular_fit, convergence_message, missingness, and diagnostic ",
  "plots before selecting the final model."
)
