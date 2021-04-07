#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- diagnosticCriteria-setup

diagnosticCriteria_table <- data.frame(stringsAsFactors = FALSE,
                                       Disease = c("AD", "VaD"),
                                       Criteria = c("NINCDS-ADRDA", "NINCDS-AIREN"),
                                       Summary = c("Lorem ipsum", "Lorem ipsum 2"))

diagnosticCriteria_caption <- "Diagnostic criteria used to diagnose different types of dementia"

# ---- diagnosticCriteria-table

knitr::kable(
  diagnosticCriteria_table,
  format = "latex",
  caption = diagnosticCriteria_caption,
  booktabs = TRUE
) %>% kable_styling(latex_options = c("hold_position"))