library(flextable)
library(officer)

apa_demographic_table <- demographic_table %>%
  flextable() %>%
  set_header_labels(
    Statistic = "Statistic",
    `Age (years)` = "Age (years)",
    `Height (cm)` = "Height (cm)",
    `Weight (kg)` = "Weight (kg)"
  ) %>%
  theme_booktabs() %>%                # No vertical lines
  bold(part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  autofit() %>%
  set_caption(
    caption = "Table 1\nDemographic Characteristics of the Sample"
  ) %>%
  add_footer_lines(
    values = paste(
      "Note. SD = standard deviation.",
      "Skewness and kurtosis are reported as distribution diagnostics."
    )
  )

apa_demographic_table

#save for docx
dir.create("figures/demographic", recursive = TRUE, showWarnings = FALSE)
save_as_docx(
  "Demographic characteristics" = apa_demographic_table,
  path = "figures/demographic/demographic_table_APA.docx"
)
