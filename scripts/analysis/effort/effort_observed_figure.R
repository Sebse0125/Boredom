library(readr)
library(dplyr)
library(ggplot2)

# Setting file directories
input_path <- "analysis/files/effort/effort_all_timepoints_long.csv"
output_dir <- "figures/effort"

summary_path <- file.path(output_dir, "effort_trajectory_summary.csv")
figure_png_path <- file.path(output_dir, "effort_observed_trajectory.png")
figure_pdf_path <- file.path(output_dir, "effort_observed_trajectory.pdf")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading prepared effort data
effort_data <- read_csv(input_path, show_col_types = FALSE)

# Checking the required variables
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

# Calculating observed means and 95% confidence intervals
trajectory_summary <- effort_data %>%
  group_by(Timepoint_number, Timepoint, Condition) %>%
  summarise(
    n = sum(!is.na(Effort)),
    Mean = if_else(n > 0, mean(Effort, na.rm = TRUE), NA_real_),
    SD = if_else(n > 1, sd(Effort, na.rm = TRUE), NA_real_),
    SE = if_else(n > 1, SD / sqrt(n), NA_real_),
    CI_low = if_else(n > 1, Mean - qt(0.975, n - 1) * SE, NA_real_),
    CI_high = if_else(n > 1, Mean + qt(0.975, n - 1) * SE, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    Condition = factor(Condition, levels = c("HB", "LB")),
    Timepoint = factor(
      Timepoint,
      levels = c("Baseline", "Trial 1", "Trial 2", "Trial 3", "Trial 4")
    )
  ) %>%
  arrange(Condition, Timepoint_number)

write_csv(trajectory_summary, summary_path, na = "")

# Creating an APA-style descriptive trajectory figure
effort_figure <- ggplot(
  trajectory_summary,
  aes(
    x = Timepoint,
    y = Mean,
    colour = Condition,
    group = Condition,
    shape = Condition,
    linetype = Condition
  )
) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.08,
    linewidth = 0.55,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.7) +
  scale_colour_manual(
    values = c("HB" = "#0072B2", "LB" = "#D55E00"),
    labels = c("HB" = "High boredom", "LB" = "Low boredom")
  ) +
  scale_shape_manual(
    values = c("HB" = 16, "LB" = 17),
    labels = c("HB" = "High boredom", "LB" = "Low boredom")
  ) +
  scale_linetype_manual(
    values = c("HB" = "solid", "LB" = "dashed"),
    labels = c("HB" = "High boredom", "LB" = "Low boredom")
  ) +
  scale_y_continuous(breaks = 0:10) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    x = "Measurement timepoint",
    y = "Mean effort rating",
    colour = "Condition",
    shape = "Condition",
    linetype = "Condition"
  ) +
  theme_classic(base_family = "Aptos", base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(8, 12, 8, 8)
  )

# Saving high-resolution PNG and vector PDF versions
ggsave(
  figure_png_path,
  plot = effort_figure,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  figure_pdf_path,
  plot = effort_figure,
  width = 7,
  height = 5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

message("Trajectory summary written: ", summary_path)
message("Observed trajectory PNG written: ", figure_png_path)
message("Observed trajectory PDF written: ", figure_pdf_path)
