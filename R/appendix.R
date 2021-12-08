#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchOverviewAppendix-table

searches <-
  # Read in all sheets
  read_excel_allsheets(here::here("data/sys-rev/searchResults_19_06_03.xlsx")) %>%
  
  # Filter columns and clean names
  lapply(
    function(x)
      x %>% select(1:3) %>% rename(
        "#" = x1,
        "Search term" = x2,
        "Hits" = x3
      )
  )

searchOverviewAppendix_table <- tail(searches$Summary,-2) %>%
  select("#","Search term") %>%
  rename("Step"="#",
         Hits = "Search term")

if(doc_type == "docx") {
  apply_flextable(searchOverviewAppendix_table, caption = "(ref:searchOverviewAppendix-caption)")
} else{
  knitr::kable(
    searchOverviewAppendix_table,
    format = "latex",
    caption = "(ref:searchOverviewAppendix-caption)",
    caption.short = "(ref:searchOverviewAppendix-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    linesep = "",
    align = "lc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(c(6,nrow(searchOverviewAppendix_table)), bold = TRUE) %>%
    row_spec(c(5,nrow(searchOverviewAppendix_table) - 1), hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = FALSE, width = paste0(15, "em")) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsMedline-table

searchHitsMedline_table <- searches$Medline

if (doc_type == "docx") {
  apply_flextable(searchHitsMedline_table, caption = "(ref:searchHitsMedline-caption)")
} else{
  knitr::kable(
    searchHitsMedline_table,
    format = "latex",
    caption = "(ref:searchHitsMedline-caption)",
    caption.short = "(ref:searchHitsMedline-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2, "em")) %>%
    column_spec(column = 2, width = paste0(36, "em")) %>%
    column_spec(column = 3, width = paste0(4, "em")) %>%
    kable_styling(
      latex_options = c("HOLD_position", "repeat_header"),
      font_size = 9
    )
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsEmbase-table

searchHitsEmbase_table <- searches$EMBASE

if (doc_type == "docx") {
  apply_flextable(searchHitsEmbase_table, caption = "(ref:searchHitsEmbase-caption)")
} else{
  knitr::kable(
    searchHitsEmbase_table,
    format = "latex",
    caption = "(ref:searchHitsEmbase-caption)",
    caption.short = "(ref:searchHitsEmbase-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsPI-table

searchHitsPI_table <- searches$PyscINFO

if (doc_type == "docx") {
  apply_flextable(searchHitsPI_table, caption = "(ref:searchHitsPI-caption)")
} else{
  knitr::kable(
    searchHitsPI_table,
    format = "latex",
    caption = "(ref:searchHitsPI-caption)",
    caption.short = "(ref:searchHitsPI-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsCentral-table

searchHitsCentral_table <- searches$CENTRAL

if (doc_type == "docx") {
  apply_flextable(searchHitsCentral_table, caption = "(ref:searchHitsCentral-caption)")
} else{
  knitr::kable(
    searchHitsCentral_table,
    format = "latex",
    caption = "(ref:searchHitsCentral-caption)",
    caption.short = "(ref:searchHitsCentral-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsWos-table

searchHitsWos_table <- searches$WoS

if (doc_type == "docx") {
  apply_flextable(searchHitsWos_table, caption = "(ref:searchHitsWos-caption)")
} else{
  knitr::kable(
    searchHitsWos_table,
    format = "latex",
    caption = "(ref:searchHitsWos-caption)",
    caption.short = "(ref:searchHitsWos-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- medRes-table

medRes_table <- rio::import("data/appendix/med_res.csv") %>%
  rename(Status = "pub_ind") %>%
  mutate(Percent = paste0(Percent,"%"))

if(doc_type == "docx") {
  apply_flextable(medRes_table, caption = "(ref:medRes-caption)")
} else{
  knitr::kable(
    medRes_table,
    format = "latex",
    caption = "(ref:medRes-caption)",
    caption.short = "(ref:medRes-scaption)",
    booktabs = TRUE,
    align = "lcc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

