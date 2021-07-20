#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHits-table

test <-
  # Read in all sheets
  read_excel_allsheets(here::here("data/sys-rev/searchResults_19_06_03.xlsx"))[2] %>%
  
  # Filter columns and clean names
  lapply(
    function(x)
      x %>% select(1:3) %>% rename(
        "#" = x1,
        "Search term" = x2,
        "Hits" = x3
      )
  )


searchHits_table <- test$Medline

if (doc_type == "docx") {
  apply_flextable(searchHits_table, caption = "(ref:searchHits-caption)")
} else{
  knitr::kable(
    searchHits_table,
    format = "latex",
    caption = "(ref:searchHits-caption)",
    caption.short = "(ref:searchHits-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(26,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- wosDatabases-table

wosDatabases_table <- read.csv(here::here("data/sys-rev/wosDatabases.csv"))

if(doc_type == "docx") {
  knitr::kable(wosDatabases_table, caption = "(ref:wosDatabases-caption)")
} else{
  knitr::kable(
    wosDatabases_table,
    format = "latex",
    caption = "(ref:wosDatabases-caption)",
    caption.short = "(ref:wosDatabases-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(column = 1, width = paste0(18,"em")) %>%
    column_spec(column = 2, width = paste0(8,"em")) %>%
    column_spec(column = 3, width = paste0(6,"em")) %>%
    row_spec(2:nrow(wosDatabases_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}
