# ---- prismaflow

# png(file = "figures/sys-rev/prismaflow.png", bg = "transparent")
# PRISMAstatement::prisma(
#   found = 16109,
#   found_other = 0,
#   no_dupes = 16109,
#   screened = 16109,
#   screen_exclusions = 15722,
#   full_text = 387,
#   full_text_exclusions = 0,
#   qualitative = 0,
#   quantitative = 0,
#   labels = NULL,
#   extra_dupes_box = FALSE,
#   dpi = 72,
#   font_size = 10)
# dev.off()
# 
# knitr::include_graphics("figures/sys-rev/prismaflow.png", 
#                         )

 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- gwet-setup

gwet_table <- data.frame(
  stringsAsFactors = FALSE,
  Kappa = c(
    "0 – 0.20",
    "0.21 – 0.39",
    "0.40 –.59",
    "0.60 –0.79",
    "0.80–0.90",
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

gwet_caption <- "Agreement for kappa"

# ---- gwet-table

knitr::kable(gwet_table, format = "latex", caption = gwet_caption, booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

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