library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(officer)

# Setting file directories
model_path <- paste0(
  "analysis/files/effort/mixed_model/",
  "model_adjusted_slope.rds"
)
output_dir <- "figures/effort/mixed_model"

estimates_path <- file.path(output_dir, "model_estimated_trial_means.csv")
figure_png_path <- file.path(output_dir, "effort_model_estimated_trajectory.png")
figure_pdf_path <- file.path(output_dir, "effort_model_estimated_trajectory.pdf")
figure_docx_path <- file.path(output_dir, "effort_model_estimated_trajectory_APA.docx")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading the selected model and recovering its analysed observations
saved_model <- readRDS(model_path)
analysis_data <- droplevels(saved_model@frame)

# Refitting a clean copy removes missing-row bookkeeping retained by na.exclude
primary_model <- lmer(
  formula(saved_model),
  data = analysis_data,
  REML = TRUE,
  na.action = na.omit,
  control = lmerControl(optimizer = "bobyqa")
)

# Calculating model-adjusted means for each condition and trial
estimated_trial_means <- emmeans(
  primary_model,
  ~ Condition | Trial,
  data = analysis_data,
  lmer.df = "satterthwaite"
) %>%
  confint() %>%
  as.data.frame() %>%
  transmute(
    Condition = factor(Condition, levels = c("HB", "LB")),
    Trial = as.integer(as.character(Trial)),
    estimated_mean = emmean,
    SE,
    df,
    CI_low = lower.CL,
    CI_high = upper.CL
  ) %>%
  arrange(Condition, Trial)

write_csv(estimated_trial_means, estimates_path, na = "")

# Creating the final inferential figure
inferential_figure <- ggplot(
  estimated_trial_means,
  aes(
    x = Trial,
    y = estimated_mean,
    colour = Condition,
    shape = Condition,
    linetype = Condition,
    group = Condition
  )
) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.08,
    linewidth = 0.55,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
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
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(breaks = 0:10) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    x = "Trial",
    y = "Model-adjusted mean effort rating",
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

# Saving high-resolution raster and vector versions
ggsave(
  figure_png_path,
  plot = inferential_figure,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  figure_pdf_path,
  plot = inferential_figure,
  width = 7,
  height = 5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

# Creating an APA-captioned Word version for insertion into the paper
figure_document <- read_docx() %>%
  body_add_fpar(
    fpar(ftext(
      "Figure 2",
      prop = fp_text(bold = TRUE, font.family = "Aptos")
    ))
  ) %>%
  body_add_fpar(
    fpar(ftext(
      "Model-Adjusted Effort Ratings Across Trials by Condition",
      prop = fp_text(italic = TRUE, font.family = "Aptos")
    ))
  ) %>%
  body_add_img(src = figure_png_path, width = 7, height = 5) %>%
  body_add_fpar(
    fpar(ftext(
      paste0(
        "Note. Points represent estimated marginal means from the baseline-",
        "adjusted random-slope model. Error bars represent 95% confidence ",
        "intervals. Estimates are adjusted for baseline effort and trial order."
      ),
      prop = fp_text(font.family = "Aptos")
    ))
  )

print(figure_document, target = figure_docx_path)

message("Model-estimated values written: ", estimates_path)
message("Inferential PNG written: ", figure_png_path)
message("Inferential PDF written: ", figure_pdf_path)
message("APA-captioned figure written: ", figure_docx_path)
