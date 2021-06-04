#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchOverview-table

searchOverview_table <- read.csv(here::here("data","sys-rev","searchOverview.csv"))

  if(doc_type == "docx") {
    knitr::kable(searchOverview_table, caption = "(ref:searchOverview-caption)")
  } else{
    knitr::kable(
      searchOverview_table,
      format = "latex", 
      caption = "(ref:searchOverview-caption)",
      caption.short = "(ref:searchOverview-scaption)",
      booktabs = TRUE,
      align = "cl"
    ) %>%
      row_spec(0, bold = TRUE) %>%
      row_spec(2:nrow(searchOverview_table)-1, hline_after = TRUE) %>%
      kableExtra::column_spec(1, bold= FALSE) %>%
      kable_styling(latex_options = c("HOLD_position"), font_size = 10)  %>%
      kableExtra::footnote(
        threeparttable = TRUE,
        general_title = "",
        general = paste("For all topics, search queries were comprised",
                        "of relevant free text & controlled vocabulary terms."
        )
      )
  }

# ---- prisma-flow-setup





#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- gwet-table

gwet_table <- data.frame(
  stringsAsFactors = FALSE,
  Kappa = c(
    "0    – 0.20",
    "0.21 – 0.39",
    "0.40 – 0.59",
    "0.60 – 0.79",
    "0.80 – 0.90",
    "> 0.90"
  ),
  Interpretation = c(
    "None",
    "Minimal",
    "Weak",
    "Moderate",
    "Strong",
    "Almost perfect"
  )
)

if(doc_type == "docx") {
  knitr::kable(gwet_table, caption = "(ref:gwet-caption)")
} else{
  knitr::kable(
    gwet_table,
    format = "latex",
    caption = "(ref:gwet-caption)",
    caption.short = "(ref:gwet-scaption)",
    booktabs = TRUE, 
    align = "cc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(2:nrow(gwet_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreementtableinter

agreement.table.1 <- data.frame(group = rep("Second reviewer decision",3),
                                reviewer = c("Exclude", "Include", "Total"), 
                                Exclude = c(1244,26,1270), 
                                Include = c(9,22,31), 
                                Total = c(1253, 48,1301))

knitr::kable(agreement.table.1, booktabs = TRUE, col.names = c("","","Exclude", "Include","Total"),  caption = 'Inter-rater reliability') %>%
  kableExtra::kable_styling("striped") %>%
  kableExtra::add_header_above(c(" " = 2, "Initial screening descision" = 3)) %>%
  kableExtra::column_spec(1, bold = T) %>%
  kableExtra::column_spec(2, bold = T) %>%
  kableExtra::column_spec(4, border_right = T) %>%
  kableExtra::collapse_rows(columns = 1, valign ="middle") %>%
  kableExtra::row_spec(2,extra_css = "border-bottom: 1px solid")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreementtableintra

agreement.table.2 <- data.frame(group = rep("Same reviewer decision",3),
                                reviewer = c("Exclude", "Include", "Total"), 
                                Exclude = c(1266,4,1270), 
                                Include = c(14,17,31), 
                                Total = c(1280,21,1301))

knitr::kable(agreement.table.2, booktabs = TRUE, col.names = c("","","Exclude", "Include","Total"),  caption = 'Intra-rater reliability') %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 2, "Initial screening decision" = 3)) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T, border_right = T) %>%
  column_spec(4, border_right = T) %>%
  collapse_rows(columns = 1, valign ="middle") %>%
  row_spec(2,extra_css = "border-bottom: 1px solid")