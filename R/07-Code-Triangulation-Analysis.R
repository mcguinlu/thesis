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
