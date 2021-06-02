#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- diagnosticCriteria-table

diagnosticCriteria_table <- read.csv("data/background/dementiaCriteria.csv") %>%
  mutate("Major neurocognitive event (previously dementia)" = Dementia) %>%
  select(Criterion, "Major neurocognitive event (previously dementia)")

col_widths <- 32/ncol(diagnosticCriteria_table)

if(doc_type == "docx") {
  knitr::kable(diagnosticCriteria_table, caption = "(ref:diagnosticCriteria-caption)")
} else{
  table <- knitr::kable(
    diagnosticCriteria_table,
    format = "latex",
    caption = "(ref:diagnosticCriteria-caption)",
    caption.short = "(ref:diagnosticCriteria-scaption)",
    align = "cl",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(2, width = paste0(27,"em")) %>%
    column_spec(1, bold = TRUE,  width = paste0(5,"em")) %>%
    row_spec(2:nrow(diagnosticCriteria_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    kableExtra::footnote(threeparttable = TRUE, symbol = "From DSM: Evidence of decline is based on concern of the individual, a knowledgeable informant, or the clinician that there has been a significant decline in cognitive function and a substantial impairment in cognitive performance, preferably documented by standardized neuropsychological testing or, in its absence, another quantified clinical assessment.")
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- lipidLevels-table

lipidLevels_table <- read.csv("data/background/lipidLevels.csv") %>%
  mutate("Measure (mg/dL)" = Measure) %>%
  select("Fraction","Measure (mg/dL)","Classification")

if(doc_type == "docx"){
  knitr::kable(lipidLevels_table,caption = "(ref:lipidLevels-caption)")
}else{
  table <- knitr::kable(
    lipidLevels_table,
    format = "latex",
    caption = "(ref:lipidLevels-caption)",
    caption.short = "(ref:lipidLevels-scaption)",
    booktabs = TRUE
  ) %>%
    collapse_rows() %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) 
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
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
  table <- lipidTreatments_table %>%
  knitr::kable(
    format = "latex",
    caption = "(ref:lipidTreatments-caption)",
    booktabs = TRUE,
  )  %>%
  column_spec(2:4, width = paste0(col_widths,"em")) %>%
  column_spec(1, bold = TRUE, width = paste0(col_widths,"em")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(2:nrow(lipidTreatments_table)-1, hline_after = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
  
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- thesisOverview-table

thesisOverview_table <- read.csv("data/background/thesisOverview.csv") %>%
  mutate("Exposure/ Intervention" = Exposure.Intervention) %>%
  mutate("Research Question" = Research.Question) %>%
  mutate("Contibution to evidence synthesis framework" = Contribution) %>%
  select("Chapter","Research Question","Exposure/ Intervention","Outcome","Contibution to evidence synthesis framework")

if(doc_type == "docx"){
  knitr::kable(thesisOverview_table,caption = "(ref:thesisOverview-caption)")
}else{
  table <- knitr::kable(
    thesisOverview_table,
    format = "latex",
    caption = "(ref:thesisOverview-caption)",
    caption.short = "(ref:thesisOverview-scaption)",
    booktabs = TRUE
  ) %>%
    column_spec(1, width = paste0(6,"em")) %>%
    column_spec(c(2,5), width = paste0(16,"em")) %>%
    column_spec(c(3,4), width = paste0(7,"em")) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(2:nrow(thesisOverview_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
}