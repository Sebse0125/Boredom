library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(tibble)
library(flextable)
library(officer)

# Setting file directories
model_dir <- "analysis/files/effort/mixed_model"
output_dir <- "figures/effort/mixed_model"

primary_model_path <- file.path(model_dir, "model_adjusted_slope.rds")
sensitivity_model_path <- file.path(model_dir, "model_unadjusted_intercept.rds")
interaction_model_path <- file.path(model_dir, "model_adjusted_slope_interaction.rds")
apa_output_path <- file.path(output_dir, "effort_mixed_model_results_APA.docx")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Reading fitted models from the diagnostic step
primary_model_saved <- readRDS(primary_model_path)
sensitivity_model_saved <- readRDS(sensitivity_model_path)

# Using only rows that were actually included in each fitted model
primary_analysis_data <- droplevels(primary_model_saved@frame)
sensitivity_analysis_data <- droplevels(sensitivity_model_saved@frame)

# Refitting clean copies removes the original 328-row na.exclude bookkeeping.
# The model formulas and the 323 analysed observations remain unchanged.
primary_model <- lmer(
  formula(primary_model_saved),
  data = primary_analysis_data,
  REML = TRUE,
  na.action = na.omit,
  control = lmerControl(optimizer = "bobyqa")
)

sensitivity_model <- lmer(
  formula(sensitivity_model_saved),
  data = sensitivity_analysis_data,
  REML = TRUE,
  na.action = na.omit,
  control = lmerControl(optimizer = "bobyqa")
)

# Formatting p-values for APA-style display tables
format_p <- function(x) {
  ifelse(is.na(x), "—", ifelse(x < .001, "< .001", sprintf("%.3f", x)))
}

# Extracting fixed effects from the primary model
fixed_matrix <- as.data.frame(coef(summary(primary_model))) %>%
  rownames_to_column("Term")

fixed_ci <- as.data.frame(
  confint(primary_model, parm = "beta_", method = "Wald")
) %>%
  rownames_to_column("Term")

names(fixed_ci)[2:3] <- c("CI_low", "CI_high")

fixed_effects <- fixed_matrix %>%
  rename(
    B = Estimate,
    SE = `Std. Error`,
    df = df,
    t = `t value`,
    p = `Pr(>|t|)`
  ) %>%
  left_join(fixed_ci, by = "Term") %>%
  select(Term, B, SE, CI_low, CI_high, df, t, p)

write_csv(
  fixed_effects,
  file.path(output_dir, "primary_model_fixed_effects.csv"),
  na = ""
)

# Estimating adjusted condition means from the primary model
condition_means <- emmeans(
  primary_model,
  ~ Condition,
  data = primary_analysis_data,
  lmer.df = "satterthwaite"
) %>%
  as.data.frame() %>%
  transmute(
    Condition = as.character(Condition),
    estimated_mean = emmean,
    SE,
    df,
    CI_low = lower.CL,
    CI_high = upper.CL
  )

write_csv(
  condition_means,
  file.path(output_dir, "adjusted_condition_means.csv"),
  na = ""
)

# Testing the primary LB-minus-HB contrast
primary_emmeans <- emmeans(
  primary_model,
  ~ Condition,
  data = primary_analysis_data,
  lmer.df = "satterthwaite"
)
primary_contrast <- contrast(
  primary_emmeans,
  method = list("LB - HB" = c(-1, 1)),
  adjust = "none"
) %>%
  summary(infer = c(TRUE, TRUE)) %>%
  as.data.frame() %>%
  transmute(
    model = "Primary: baseline-adjusted random slope",
    contrast,
    estimate,
    SE,
    df,
    CI_low = lower.CL,
    CI_high = upper.CL,
    t = t.ratio,
    p = p.value
  )

# Repeating the contrast in the unadjusted sensitivity model
sensitivity_emmeans <- emmeans(
  sensitivity_model,
  ~ Condition,
  data = sensitivity_analysis_data,
  lmer.df = "satterthwaite"
)
sensitivity_contrast <- contrast(
  sensitivity_emmeans,
  method = list("LB - HB" = c(-1, 1)),
  adjust = "none"
) %>%
  summary(infer = c(TRUE, TRUE)) %>%
  as.data.frame() %>%
  transmute(
    model = "Sensitivity: unadjusted random intercept",
    contrast,
    estimate,
    SE,
    df,
    CI_low = lower.CL,
    CI_high = upper.CL,
    t = t.ratio,
    p = p.value
  )

overall_contrasts <- bind_rows(primary_contrast, sensitivity_contrast)

write_csv(
  overall_contrasts,
  file.path(output_dir, "overall_condition_contrasts.csv"),
  na = ""
)

# Testing whether the HB-LB difference changes across trials
# Maximum likelihood is used for comparing models with different fixed effects.
primary_model_ml <- lmer(
  Effort ~ Condition + Trial + trial_order + baseline_effort_c +
    (1 + Condition | ID),
  data = primary_analysis_data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

interaction_model_ml <- lmer(
  Effort ~ Condition * Trial + trial_order + baseline_effort_c +
    (1 + Condition | ID),
  data = primary_analysis_data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

interaction_comparison <- anova(primary_model_ml, interaction_model_ml)

df_column <- intersect(c("Df", "Chi Df"), names(interaction_comparison))

if (length(df_column) != 1L) {
  stop("Could not identify the degrees-of-freedom column in the interaction test.")
}

interaction_test <- tibble(
  comparison = "Condition × Trial interaction",
  chi_square = interaction_comparison$Chisq[2],
  interaction_df = interaction_comparison[[df_column]][2],
  p = interaction_comparison$`Pr(>Chisq)`[2],
  AIC_main_effects = AIC(primary_model_ml),
  AIC_interaction = AIC(interaction_model_ml)
)

write_csv(
  interaction_test,
  file.path(output_dir, "condition_by_trial_interaction_test.csv"),
  na = ""
)

# Refitting the interaction model with REML for estimates and saving it
interaction_model <- lmer(
  Effort ~ Condition * Trial + trial_order + baseline_effort_c +
    (1 + Condition | ID),
  data = primary_analysis_data,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)

saveRDS(interaction_model, interaction_model_path)

# Estimating HB-LB differences separately at each trial
trial_emmeans <- emmeans(
  interaction_model,
  ~ Condition | Trial,
  data = interaction_model@frame,
  lmer.df = "satterthwaite"
)

trial_contrasts <- contrast(
  trial_emmeans,
  method = list("LB - HB" = c(-1, 1)),
  adjust = "none"
) %>%
  summary(infer = c(TRUE, TRUE)) %>%
  as.data.frame() %>%
  transmute(
    Trial = as.character(Trial),
    contrast,
    estimate,
    SE,
    df,
    CI_low = lower.CL,
    CI_high = upper.CL,
    t = t.ratio,
    p_unadjusted = p.value,
    p_holm = p.adjust(p.value, method = "holm")
  )

write_csv(
  trial_contrasts,
  file.path(output_dir, "trial_specific_condition_contrasts.csv"),
  na = ""
)

# Creating display versions for the APA-style Word document
fixed_effects_display <- fixed_effects %>%
  mutate(
    Term = recode(
      Term,
      `(Intercept)` = "Intercept",
      ConditionLB = "Condition: LB versus HB",
      Trial2 = "Trial 2 versus Trial 1",
      Trial3 = "Trial 3 versus Trial 1",
      Trial4 = "Trial 4 versus Trial 1",
      trial_orderLB_HB = "Order: LB-HB versus HB-LB",
      baseline_effort_c = "Baseline effort (centered)"
    ),
    B = sprintf("%.2f", B),
    SE = sprintf("%.2f", SE),
    `95% CI` = sprintf("[%.2f, %.2f]", CI_low, CI_high),
    df = sprintf("%.1f", df),
    t = sprintf("%.2f", t),
    p = format_p(p)
  ) %>%
  select(Term, B, SE, `95% CI`, df, t, p)

condition_means_display <- condition_means %>%
  mutate(
    Condition = recode(
      Condition,
      HB = "High boredom",
      LB = "Low boredom"
    ),
    `Estimated M` = sprintf("%.2f", estimated_mean),
    SE = sprintf("%.2f", SE),
    `95% CI` = sprintf("[%.2f, %.2f]", CI_low, CI_high)
  ) %>%
  select(Condition, `Estimated M`, SE, `95% CI`)

overall_display <- overall_contrasts %>%
  mutate(
    Estimate = sprintf("%.2f", estimate),
    SE = sprintf("%.2f", SE),
    `95% CI` = sprintf("[%.2f, %.2f]", CI_low, CI_high),
    df = sprintf("%.1f", df),
    t = sprintf("%.2f", t),
    p = format_p(p)
  ) %>%
  select(Model = model, Contrast = contrast, Estimate, SE, `95% CI`, df, t, p)

interaction_display <- interaction_test %>%
  transmute(
    Effect = comparison,
    `χ²` = sprintf("%.2f", chi_square),
    df = as.character(interaction_df),
    p = format_p(p)
  )

trial_display <- trial_contrasts %>%
  mutate(
    Trial = paste("Trial", Trial),
    Estimate = sprintf("%.2f", estimate),
    SE = sprintf("%.2f", SE),
    `95% CI` = sprintf("[%.2f, %.2f]", CI_low, CI_high),
    df = sprintf("%.1f", df),
    t = sprintf("%.2f", t),
    `p (Holm)` = format_p(p_holm)
  ) %>%
  select(Trial, Contrast = contrast, Estimate, SE, `95% CI`, df, t, `p (Holm)`)

make_apa_table <- function(data) {
  flextable(data) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    autofit()
}

add_apa_table <- function(document, number, title, table, note) {
  document %>%
    body_add_fpar(
      fpar(ftext(
        paste("Table", number),
        prop = fp_text(bold = TRUE, font.family = "Aptos")
      ))
    ) %>%
    body_add_fpar(
      fpar(ftext(
        title,
        prop = fp_text(italic = TRUE, font.family = "Aptos")
      ))
    ) %>%
    body_add_flextable(table) %>%
    body_add_fpar(
      fpar(ftext(note, prop = fp_text(font.family = "Aptos")))
    ) %>%
    body_add_par("")
}

# Writing all inferential tables to one APA-style Word document
apa_document <- read_docx()

apa_document <- add_apa_table(
  apa_document,
  2,
  "Fixed Effects From the Primary Linear Mixed-Effects Model",
  make_apa_table(fixed_effects_display),
  paste0(
    "Note. HB is the reference condition, Trial 1 is the reference trial, ",
    "and HB-LB is the reference order. Baseline effort was grand-mean ",
    "centered. Participant-specific intercepts and condition slopes were ",
    "included as random effects."
  )
)

apa_document <- add_apa_table(
  apa_document,
  3,
  "Model-Adjusted Mean Effort Ratings by Condition",
  make_apa_table(condition_means_display),
  "Note. Estimated means are adjusted for trial, trial order, and baseline effort."
)

apa_document <- add_apa_table(
  apa_document,
  4,
  "Overall Low-Boredom Minus High-Boredom Effort Contrast",
  make_apa_table(overall_display),
  paste0(
    "Note. A negative estimate indicates lower effort in the low-boredom ",
    "condition. The unadjusted model is reported as a sensitivity analysis."
  )
)

apa_document <- add_apa_table(
  apa_document,
  5,
  "Omnibus Condition-by-Trial Interaction Test",
  make_apa_table(interaction_display),
  "Note. The likelihood-ratio test compares maximum-likelihood models."
)

apa_document <- add_apa_table(
  apa_document,
  6,
  "Low-Boredom Minus High-Boredom Contrasts at Each Trial",
  make_apa_table(trial_display),
  paste0(
    "Note. Negative estimates indicate lower effort in the low-boredom ",
    "condition. P values are adjusted across the four trials using Holm's method."
  )
)

print(apa_document, target = apa_output_path)

message("Inferential CSV tables written to: ", output_dir)
message("APA-style results document written: ", apa_output_path)
message("Interaction model written: ", interaction_model_path)
