#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- smeeth-comparison-setup

smeeth-comparison_table <- mtcars

smeeth-comparison_caption <- "Caption"

# ---- smeeth-comparison-table

knitr::kable(
  smeeth - comparison_table,
  format = "latex",
  caption = smeeth - comparison_caption,
  booktabs = TRUE
) %>% kable_styling(latex_options = c("striped", "hold_position"))