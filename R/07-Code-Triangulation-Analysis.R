#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- triSummary-table

triSummary_table <- rio::import("data/background/thesisOverview.csv") %T>%
  write.csv("data/table_words/triSummary.csv")

if (doc_type == "docx") {
  apply_flextable(triSummary_table, caption = "(ref:triSummary-caption)")
} else{
  knitr::kable(
    triSummary_table,
    format = "latex",
    caption = "(ref:triSummary-caption)",
    caption.short = "(ref:triSummary-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- questionsOfInterest-table

questionsOfInterest_table <- rio::import("data/background/thesisOverview.csv") %T>%
  write.csv("data/table_words/questionsOfInterest.csv")


if(doc_type == "docx"){
apply_flextable(questionsOfInterest_table,caption = "(ref:questionsOfInterest-caption)")
}else{
knitr::kable(questionsOfInterest_table, format = "latex", caption = "(ref:questionsOfInterest-caption)", caption.short = "(ref:questionsOfInterest-scaption)", booktabs = TRUE) %>% 
row_spec(0, bold = TRUE) %>%
kable_styling(latex_options = c("HOLD_position"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- robLevelsMapping-table

robLevelsMapping_table <- rio::import("data/tri/rob_levels_mapping.csv") %T>%
  write.csv("data/table_words/rob_levels_mapping.csv")

if(doc_type == "docx") {
  apply_flextable(robLevelsMapping_table, caption = "(ref:robLevelsMapping-caption)")
} else{
  knitr::kable(
    robLevelsMapping_table,
    format = "latex",
    caption = "(ref:robLevelsMapping-caption)",
    caption.short = "(ref:robLevelsMapping-scaption)",
    booktabs = TRUE, 
    align = "ccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    row_spec(2:nrow(robLevelsMapping_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}
