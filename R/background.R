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


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- lipidLevels-table

lipidLevels_table <- read.csv("data/background/lipidLevels.csv") %>%
  mutate("Measure (mg/dL)" = Measure) %>%
  select("Fraction","Measure (mg/dL)","Classification")

if(doc_type == "docx"){
  knitr::kable(lipidLevels_table,caption = "(ref:lipidLevels-caption)")
}else{
  knitr::kable(
    lipidLevels_table,
    format = "latex",
    caption = "(ref:lipidLevels-caption)",
    booktabs = TRUE
  ) %>%
    collapse_rows() %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
}
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- lipidTreatments-table

lipidTreatments_table <- read.csv("data/background/lipidTreatments.csv") %>%
  mutate("Mechanism of action" = Mechanism) %>%
  select(Treatment,Effect,"Mechanism of action",Examples)


col_widths <- 32/ncol(lipidTreatments_table)

if(doc_type == "docx"){
  knitr::kable(lipidTreatments_table,caption = "(ref:lipidTreatments-caption)")
}else{
  knitr::kable(
    lipidTreatments_table,
    format = "latex",
    caption = "(ref:lipidTreatments-caption)",
    booktabs = TRUE
  ) %>%
  column_spec(2:4, width = paste0(col_widths,"em")) %>%
  column_spec(1, bold = TRUE,width = paste0(col_widths,"em")) %>%  
  row_spec(0, bold = TRUE) %>%
  row_spec(2:nrow(lipidTreatments_table)-1, hline_after = TRUE) %>%  
  kable_styling(latex_options = c("hold_position"))
}
