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

prisma_df <- read.csv(here::here("data/sys-rev/PRISMAflow.csv"), stringsAsFactors = F)

prisma_full <- read.csv(system.file("extdata", "PRISMA.csv", package= "PRISMA2020"), stringsAsFactors = F)[,c(1:4,6:7)] %>%
  dplyr::left_join(prisma_df, by = "description") %>%
  PRISMA2020::read_PRISMAdata()

attach(prisma_full)
PRISMA2020::PRISMA_save(plotobj = PRISMA2020::PRISMA_flowdiagram(
  prisma_full,
  interactive = F,
  previous = F,
  other = T,
  fontsize = 12
),
"figures/sys-rev/prismaflow.png")
detach("prisma_full")


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
# ---- agreeInter-table

agreeInter_table <- data.frame(group = rep("Second reviewer decision",3),
                               reviewer = c("Exclude", "Include", "Total"), 
                               Exclude = c(1244,26,1270), 
                               Include = c(9,22,31), 
                               Total = c(1253, 48,1301))

discrepancy_Inter <- agreeInter_table$Exclude[2]

agreeInter_coeff_table <- agreeInter_table[1:2,2:4]
rownames(agreeInter_coeff_table) <- agreeInter_coeff_table[,1]
agreeInter_coeff_table <- agreeInter_coeff_table[-1]

agreeInter_coeff <- unlist(c(comma(irrCAC::gwet.ac1.table(agreeInter_coeff_table)[2]),
                             comma(irrCAC::kappa2.table(agreeInter_coeff_table)[2])))

if(doc_type == "docx") {
  knitr::kable(agreeInter_table, caption = "(ref:agreeInter-caption)")
} else{
  knitr::kable(
    agreeInter_table,
    format = "latex",
    caption = "(ref:agreeInter-caption)",
    caption.short = "(ref:agreeInter-scaption)",
    booktabs = TRUE,
    col.names = c("", "", "Exclude", "Include", "Total")
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    kableExtra::add_header_above(c(" " = 2, "Initial screening decision" = 3)) %>%
    kableExtra::column_spec(0:2, bold = T) %>%
    kableExtra::column_spec(4, border_right = T) %>%
    kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
    kableExtra::row_spec(2, extra_css = "border-bottom: 1px solid")
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreeIntra-table

agreeIntra_table <- data.frame(group = rep("Same reviewer decision",3),
                               reviewer = c("Exclude", "Include", "Total"), 
                               Exclude = c(1266,4,1270), 
                               Include = c(14,17,31), 
                               Total = c(1280,21,1301))

discrepancy_Intra <- agreeIntra_table$Exclude[2]

agreeIntra_coeff_table <- agreeIntra_table[1:2,2:4]
rownames(agreeIntra_coeff_table) <- agreeIntra_coeff_table[,1]
agreeIntra_coeff_table <- agreeIntra_coeff_table[-1]

agreeIntra_coeff <- unlist(c(comma(irrCAC::gwet.ac1.table(agreeIntra_coeff_table)[2]),
                                 comma(irrCAC::kappa2.table(agreeIntra_coeff_table)[2])))

if(doc_type == "docx") {
  knitr::kable(agreeIntra_table, caption = "(ref:agreeIntra-caption)")
} else{
  knitr::kable(
    agreeIntra_table,
    format = "latex",
    caption = "(ref:agreeIntra-caption)",
    caption.short = "(ref:agreeIntra-scaption)",
    booktabs = TRUE,
    col.names = c("", "", "Exclude", "Include", "Total")
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))  %>%
    kableExtra::add_header_above(c(" " = 2, "Initial screening decision" = 3)) %>%
    kableExtra::column_spec(0:2, bold = T) %>%
    kableExtra::column_spec(4, border_right = T) %>%
    kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
    kableExtra::row_spec(2, extra_css = "border-bottom: 1px solid")
}


