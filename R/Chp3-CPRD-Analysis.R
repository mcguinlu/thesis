#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- smeethComparison-table

smeethComparison_table <- mtcars[1:4,1:3]

if(doc_type == "docx"){
knitr::kable(smeethComparison_table,caption = "(ref:smeethComparison-caption)")
}else{
knitr::kable(smeethComparison_table, format = "latex", caption = "(ref:smeethComparison-caption)", caption.short = "(ref:smeethComparison-scaption)", booktabs = TRUE) %>% 
row_spec(0, bold = TRUE) %>%
kable_styling(latex_options = c("HOLD_position"))
}