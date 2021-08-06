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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- studyCharacteristics-table

toc_df <- rio::import("data/sys-rev/Book1.xlsx",which = 1) %>%
  janitor::clean_names() %>%
  select(study_id, author, year, location, data_source)

toc_df2 <- rio::import("data/sys-rev/Book1.xlsx",which = 2) %>%
  janitor::clean_names() %>%
  filter(!is.na(study_id)) %>%
  group_by(study_id) %>% 
  summarize(Criteria = paste0(unique(diagnostic_criteria), collapse = "; "),
            Exposures = paste0(unique(exposure), collapse = "; "), 
            Outcomes = paste0(unique(outcome), collapse = "; ")) %>%
  ungroup()

studyCharacteristics_table <- left_join(toc_df, toc_df2) %>%
  mutate(Study = paste(author, year)) %>%
  select(Study, location, data_source, Exposures, Outcomes, Criteria, -c(study_id,author, year)) %>%
  rename("Location" = location,
         "Data source" = data_source, 
         "Diagnostic criteria" = Criteria) %>%
  arrange(Study) %>%
  filter(!is.na(Location))

if(doc_type == "docx") {
  apply_flextable(studyCharacteristics_table, caption = "(ref:studyCharacteristics-caption)")
} else{
  knitr::kable(
    studyCharacteristics_table,
    format = "latex",
    caption = "(ref:studyCharacteristics-caption)",
    caption.short = "(ref:studyCharacteristics-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    kableExtra::column_spec(0:1, width = "5em") %>%
    kableExtra::column_spec(2:6, width = "10em")
    
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# cohortLocationsSetup ----

world <- toc_df %>%
  count(location) %>%
  right_join(
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
    by = c("location" = "name")
  ) %>%
  tidyr::replace_na(list(n = 0))

p1 <- ggplot(data = world) +
  geom_sf(aes(fill = n, geometry = geometry), size = 0.1, legend = "none") +
  geom_rect(
    xmax = 36,
    xmin = -10,
    ymax = 70,
    ymin = 35,
    fill = "transparent",
    color = "black",
    size = 0.5
  ) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(ylim = c(-60, 85),
           expand = FALSE) +
  geom_rect(
    xmax = Inf,
    xmin = Inf,
    ymax = -80,
    ymin = -60,
    fill = "black",
    color = "black",
    size = 1
  ) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = "transparent"))

p2 <- ggplot(data = world) +
  geom_sf(aes(fill = n, geometry = geometry), size = 0.1) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(36,-12),
           ylim = c(70, 35),
           expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = "transparent"))

plot_space <- ggplot() +
  coord_sf(
    xlim = c(36,-12),
    ylim = c(70, 35),
    expand = FALSE,
    clip = "off"
  ) +
  annotate(
    "segment",
    x = -160,
    xend = -1180,
    y = -720,
    yend = 71,
    colour = "black",
  ) +
  annotate(
    "segment",
    x = 625,
    xend = 1210,
    y = -720,
    yend = 71,
    colour = "black",
  ) +
  theme_void()

p3 <- p2 + plot_space + p1 + plot_layout(
  ncol = 1,nrow = 3,
  guides = 'collect',
  widths = c(1, 1, 1),
  heights = c(5, 0.1, 10)
)  & labs(fill="No. of\ncohorts") &
  scale_fill_gradient(low = "grey95",
                      high = "grey10") &
  theme(legend.position = "left", 
        legend.box.margin=margin(0,20,0,0))

ggsave(
  here::here("figures/sys-rev/cohortLocations.png"),
  p3,
  height = 6,
  width = 8
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# primaryFigures----

results_statins <- rio::import("data/sys-rev/data_extraction_reviewer1.xlsx", which = 1) %>%
  janitor::clean_names() %>%
  dplyr::filter(done == "Y",
         !is.na(point_estimate),
         measure == "HR",
         outcome == "Dementia",
         exposure %like% "[Ss]tatin",
         !grepl(pattern = "[Ll]ipo|[Hh]ydro", x = .$exposure)) %>%
  mutate(point = log(point_estimate),
         SE = (log(lower_95_percent) - log(upper_95_percent))/3.92) %>%
  arrange(author, year)

t <- metafor::rma.uni(data = results_statins,
                      yi = point,
                      sei = SE,
                      slab = paste(author, year))

statins <- broom_ma(t) %>%
  mutate(point_colour = ifelse(type == "summary",
         "black",
         "grey50"))

forester_thesis(
  statins[,1],
  statins$estimate,
  statins$conf.low,
  statins$conf.high,
  estimate_precision = 2,
  font_family = "Fira Sans",
  file_path = here::here("figures/sys-rev/forester_statins_any.png"),
  arrows = TRUE,
  display = FALSE,
  x_scale_linear = F,
  stripe_colour = "white",
  null_line_at = 1,
  xlim = c(0.3,3), 
  xbreaks = c(0.3,1,3),
  adjustment = 0.35,
  colour_vec = statins$point_colour,
  arrow_labels = c("Favours control","Favours intervention")
)

# 
png(file = here::here("figures/sys-rev/funnel_statins_any.png"))
metafor::funnel(t)
dev.off()
# 
# 
# 
# test <- rio::import("data/sys-rev/data_extraction_reviewer1.xlsx") %>%
#   janitor::clean_names() %>%
#   filter(done == "Y",
#          !is.na(point_estimate), 
#          measure == "HR", 
#          outcome %like% "VaD", 
#          exposure %like% "[Ss]tatin", 
#          !grepl(pattern = "[Ll]ipo|[Hh]ydro", x = .$exposure)) %>%
#   mutate(point = log(point_estimate), 
#          SE = (log(lower_95_percent) - log(upper_95_percent))/3.92)
# 
# t <- metafor::rma.uni(data = test,
#                       yi = point,
#                       sei = SE)
# 
# png(file = 'forestplot_vad.png') 
# metafor::forest(t, transf=exp, slab = paste(test$author, test$year), order ="obs", refline =1, xlab = "HR for VaD")
# dev.off() 
# 
# 
# 
# test <- rio::import("data/sys-rev/data_extraction_reviewer1.xlsx") %>%
#   janitor::clean_names() %>%
#   filter(done == "Y",
#          !is.na(point_estimate), 
#          measure == "HR", 
#          outcome %like% "AD", 
#          exposure %like% "[Ss]tatin", 
#          !grepl(pattern = "[Ll]ipo|[Hh]ydro", x = .$exposure)) %>%
#   mutate(point = log(point_estimate), 
#          SE = (log(lower_95_percent) - log(upper_95_percent))/3.92)
# 
# t <- metafor::rma.uni(data = test,
#                       yi = point,
#                       sei = SE)
