#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchOverview-table

searchOverview_table <-
  read.csv(here::here("data", "sys-rev", "searchOverview.csv"))

if (doc_type == "docx") {
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
    row_spec(2:nrow(searchOverview_table) - 1, hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = FALSE) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 10)  %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        "For all topics, search queries were comprised",
        "of relevant free text & controlled vocabulary terms."
      )
    )
}

# ---- prisma-flow-setup

prisma_df <-
  read.csv(here::here("data/sys-rev/PRISMAflow.csv"),
           stringsAsFactors = F)

n_included <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(exclude)) %>%
  distinct(study_id, author) %>%
  n_distinct()

prisma_df$n[24] <- n_included

n_extra_reports <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 1) %>%
  janitor::clean_names() %>%
  filter(!is.na(exclude)) %>%
  filter(additional_record_for_included_study == "Y") %>%
  n_distinct()

n_reports_included <- n_included + n_extra_reports

prisma_df$n[25] <- n_reports_included

exclusion_reasons <-
  rio::import(here::here("data/sys-rev/exclusion_reasons.csv")) %>%
  arrange(desc(V2)) %>%
  mutate(label = paste0(V1, ", ", V2))

n_wrong_exposure <- exclusion_reasons %>%
  filter(V1 == "Wrong exposure") %>%
  pull(V2)

n_study_design <- exclusion_reasons %>%
  filter(V1 == "Wrong study design") %>%
  pull(V2)


prisma_df$n[21] <- paste0(exclusion_reasons$label, collapse = "; ")

prisma_full <-
  read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"),
           stringsAsFactors = F)[, c(1:4, 6:7)] %>%
  dplyr::left_join(prisma_df, by = "description") %>%
  PRISMA2020::read_PRISMAdata()

attach(prisma_full)
PRISMA2020::PRISMA_save(
  plotobj = PRISMA2020::PRISMA_flowdiagram(
    prisma_full,
    interactive = F,
    previous = F,
    other = T,
    fontsize = 12
  ),
  "figures/sys-rev/prismaflow.png"
)
detach("prisma_full")

# Check exclusion reasons and number included all add up to number screened
# If FALSE, delete created PRISMA file which will cause RMarkdown render to fail
if (!sum(exclusion_reasons$V2) + n_included + n_extra_reports == prisma_df$n[20]) {
  file.remove(here::here("figures/sys-rev/prismaflow.png"))
}


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
)  %T>%
  write.csv("data/table_words/gwet.csv")

if (doc_type == "docx") {
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
    row_spec(2:nrow(gwet_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agree-setup

# Inter
agreeInter_table <-
  data.frame(
    group = rep("Second reviewer decision", 3),
    reviewer = c("Exclude", "Include", "Total"),
    Exclude = c(1244, 26, 1270),
    Include = c(9, 22, 31),
    Total = c(1253, 48, 1301)
  )

discrepancy_Inter <- agreeInter_table$Exclude[2]

agreeInter_coeff_table <- agreeInter_table[1:2, 2:4]
rownames(agreeInter_coeff_table) <- agreeInter_coeff_table[, 1]
agreeInter_coeff_table <- agreeInter_coeff_table[-1] %T>%
  write.csv("data/table_words/agreeInter.csv")

agreeInter_coeff <-
  unlist(c(
    comma(irrCAC::gwet.ac1.table(agreeInter_coeff_table)[2]),
    comma(irrCAC::kappa2.table(agreeInter_coeff_table)[2])
  ))

# Intra
agreeIntra_table <-
  data.frame(
    group = rep("Same reviewer decision (with 3 month lag)", 3),
    reviewer = c("Exclude", "Include", "Total"),
    Exclude = c(1266, 4, 1270),
    Include = c(14, 17, 31),
    Total = c(1280, 21, 1301)
  )

discrepancy_Intra <- agreeIntra_table$Exclude[2]

agreeIntra_coeff_table <- agreeIntra_table[1:2, 2:4]
rownames(agreeIntra_coeff_table) <- agreeIntra_coeff_table[, 1]
agreeIntra_coeff_table <- agreeIntra_coeff_table[-1] %T>%
  write.csv("data/table_words/agreeIntra.csv")

agreeIntra_coeff <-
  unlist(c(
    comma(irrCAC::gwet.ac1.table(agreeIntra_coeff_table)[2]),
    comma(irrCAC::kappa2.table(agreeIntra_coeff_table)[2])
  ))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreeInter-table

if (doc_type == "docx") {
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
    kableExtra::column_spec(1, width = "12em") %>%
    kableExtra::column_spec(0:2, bold = T) %>%
    kableExtra::column_spec(4, border_right = T) %>%
    kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
    kableExtra::row_spec(2, extra_css = "border-bottom: 1px solid")
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreeIntra-table

if (doc_type == "docx") {
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
    kableExtra::column_spec(1, width = "12em") %>%
    kableExtra::column_spec(4, border_right = T) %>%
    kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
    kableExtra::row_spec(2, extra_css = "border-bottom: 1px solid")
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- characteristicsSetup

toc_df <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(exclude)) %>%
  select(
    study_id,
    author,
    year,
    number_participants,
    subtype,
    age_at_baseline,
    female_percent,
    location,
    data_source,
    group_sizes_if_applicable
  ) %>%
  mutate(type = subtype) %>%
  mutate(author = ifelse(grepl("Heart Protection", author), "HPS", author)) %>%
  mutate(group_sizes_if_applicable = stringr::str_remove_all(group_sizes_if_applicable, " ")) %>%
  rowwise() %>%
  mutate(age_combo = ifelse(
    grepl("\\|", age_at_baseline),
    combine_age(age_at_baseline, group_sizes_if_applicable),
    age_at_baseline
  )) %>%
  mutate(female_combo = ifelse(
    grepl("\\|", female_percent),
    combine_female(female_percent, group_sizes_if_applicable),
    female_percent
  )) %>%
  mutate(
    female_combo = case_when(female_combo == "64.400000000000006" ~ "64.4", T ~ female_combo),
    age_combo = case_when(age_combo == "74.900000000000006" ~ "74.9", T ~ age_combo)
  )

toc_df2 <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  janitor::clean_names() %>%
  filter(!is.na(study_id)) %>%
  mutate(exposure = ifelse(
    stringr::str_detect(exposure, "Statin -"),
    "Statin",
    stringr::str_remove(exposure, "- .*$")
  )) %>%
  mutate(
    exposure = ifelse(
      exposure_category == "Hypercholesterolemia",
      "Hypercholesterolemia",
      exposure
    )
  ) %>%
  group_by(study_id) %>%
  summarize(
    Criteria = paste0(unique(diagnostic_criteria), collapse = "; "),
    Exposures = paste0(unique(exposure), collapse = "; "),
    Outcomes = paste0(unique(outcome), collapse = "; ")
  ) %>%
  ungroup()

# toc_df2 %>%
#   filter(exclude != "Y") %>%
#   filter(type == "NRSI") %>%
#   filter(measure == "RR")
#   group_by(measure) %>%
#   count()

# Generate count of study designs by year
p_type <-
  toc_df %>%
  group_by(year) %>%
  count(type) %>%
  ggplot(aes(x = year, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = 1:12, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  labs(fill = "Study design",
       x = "Year of publication",
       y = "Number of studies") +
  scale_fill_grey() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(here::here("figures/sys-rev/type_by_year.png"),
       p_type,
       height = 3)


study_details <-
  left_join(toc_df, toc_df2, by = c("study_id" = "study_id")) %>%
  mutate(Study = paste(author, year)) %>%
  mutate(type = factor(type, levels = c("RCT", "NRSI", "NRSE", "MR")),)

n_nrsi_included <- study_details %>%
  filter(type == "NRSI") %>%
  group_by(study_id) %>%
  distinct() %>%
  nrow()

n_nrse_included <- study_details %>%
  filter(type == "NRSE") %>%
  group_by(study_id) %>%
  distinct() %>%
  nrow()

n_statins <- study_details %>%
  filter(type == "NRSI") %>%
  mutate(statin = stringr::str_detect(Exposures, "[Ss]tatin")) %>%
  group_by(statin) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrsi_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_fibrates <- study_details %>%
  filter(type == "NRSI") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "[Ff]ibrate")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrsi_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_hyperchol <- study_details %>%
  filter(type == "NRSE") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "Hyperchol")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrse_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_TC <- study_details %>%
  filter(type == "NRSE") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "TC")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrse_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_LDL <- study_details %>%
  filter(type == "NRSE") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "LDL")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrse_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_HDL <- study_details %>%
  filter(type == "NRSE") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "HDL")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrse_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_TG <- study_details %>%
  filter(type == "NRSE") %>%
  mutate(fibrate = stringr::str_detect(Exposures, "TG")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((
    .$n / n_nrse_included
  ) * 100, 1), "%")) %>%
  pull(label)

n_Dementia <- study_details %>%
  mutate(fibrate = stringr::str_detect(Outcomes, "Dementia")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((.$n / n_included) * 100, 1), "%")) %>%
  pull(label)

n_AD <- study_details %>%
  mutate(fibrate = stringr::str_detect(Outcomes, "AD")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((.$n / n_included) * 100, 1), "%")) %>%
  pull(label)

n_VaD <- study_details %>%
  mutate(fibrate = stringr::str_detect(Outcomes, "VaD")) %>%
  filter(fibrate == "TRUE") %>%
  group_by(fibrate) %>%
  count() %>%
  mutate(label = paste0("n = ", .$n, "; ", round((.$n / n_included) * 100, 1), "%")) %>%
  pull(label)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- studyCharacteristics-table

studyCharacteristics_table <- study_details %>%
  # Add asterisk to preprinted studies
  mutate(Study = ifelse(study_id %in% c(90004, 90005, 3232), paste0(Study, "*"), Study)) %>%
  select(
    Study,
    type,
    location,
    number_participants,
    age_combo,
    female_combo,
    Exposures,
    Outcomes,
    Criteria,-c(study_id, author, year)
  ) %>%
  rename(
    "Location" = location,
    "Type" = type,
    "N" = number_participants,
    "Female (%)" = female_combo,
    "Age at baseline" = age_combo,
    "Diagnostic criteria" = Criteria,
  ) %>%
  tidy_nums() %>%
  arrange(desc(Type), Study) %>%
  select(-Type) %>%
  mutate(across(everything(),  ~ stringr::str_replace(., "NA", "NR"))) %T>%
  write.csv("data/table_words/studyCharacteristics.csv")

if (doc_type == "docx") {
  apply_flextable(studyCharacteristics_table, caption = "(ref:studyCharacteristics-caption)")
} else{
  knitr::kable(
    studyCharacteristics_table,
    format = "latex",
    caption = "(ref:studyCharacteristics-caption)",
    caption.short = "(ref:studyCharacteristics-scaption)",
    booktabs = TRUE,
    longtable = TRUE,
    align = "llccclll",
    linesep = "\\addlinespace\\addlinespace"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(
      latex_options = c("HOLD_position", "repeat_header"),
      font_size = 7
    ) %>%
    kableExtra::column_spec(0, width = "20em") %>%
    kableExtra::column_spec(c(1, 3:8), width = "9.5em") %>%
    kableExtra::column_spec(2, width = "5em") %>%
    kableExtra::group_rows(
      group_label = "Randomised controlled trials",
      start_row = 1,
      end_row = 2,
      hline_after = T,
      extra_latex_after = "\\addlinespace"
    ) %>%
    kableExtra::group_rows(
      group_label = "Non-randomised studies of interventions",
      start_row = 3,
      end_row = 33,
      hline_after = T,
      extra_latex_after = "\\addlinespace"
    ) %>%
    kableExtra::group_rows(
      group_label = "Non-randomised studies of exposures",
      start_row = 34,
      end_row = 76,
      hline_after = T,
      extra_latex_after = "\\addlinespace"
    ) %>%
    kableExtra::group_rows(
      group_label = "Mendelian randomisation studies",
      start_row = 77,
      end_row = 84,
      hline_after = T,
      extra_latex_after = "\\addlinespace"
    ) %>%
    kableExtra::footnote(
      symbol = "Denotes preprinted study.",
      threeparttable = T,
      escape = F,
      general_title = "",
      footnote_order = c("symbol", "general"),
      general = paste(
        "\\\\textit{Abbreviations:}",
        "AD - Alzheimer's disease;",
        "DSM - Diagnostic and Statistical Manual (Roman numerals indicate edition);",
        "EHR - Electronic code list;",
        "ICD - International Classification of Diseease (numbers indicate edition);",
        "HDL-c - high density lipoprotein cholesterol;",
        "LDL-c - low density lipoprotein cholesterol;",
        "NINCDS-ADRDA - National Institute of Neurological and Communicative Disorders and Stroke and the Alzheimer's Disease and Related Disorders Association;",
        "NINCDS-AIREN - National Institute of Neurological Disorders and Stroke and Association Internationale pour la Recherché et l'Enseignement en Neurosciences;",
        "NR - Not reported;",
        "TC - total cholesterol;",
        "TG - triglycerides;",
        "VaD - vascular dementia."
      )
    )
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortLocationsSetup

n_taiwan <- toc_df %>%
  count(location) %>%
  filter(location == "Taiwan") %>%
  mutate(label = paste0("n = ", .$n, "; ", round((.$n / n_included) * 100, 2), "%")) %>%
  pull(label)

# n_taiwan <- toc_df %>%
#   filter(location == "Taiwan")

world <- toc_df %>%
  count(location) %>%
  right_join(
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% filter(type != "Dependency"),
    by = c("location" = "name")
  )

p1 <- ggplot(data = world) +
  geom_sf(aes(fill = n, geometry = geometry),
          size = 0.1,
          legend = "none") +
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
  ncol = 1,
  nrow = 3,
  guides = 'collect',
  widths = c(1, 1, 1),
  heights = c(5, 0.1, 10)
)  & labs(fill = "No. of\nstudies") &
  scale_fill_gradient2(
    low = "#ffffff",
    mid = "grey50",
    midpoint = 8,
    na.value = "#ffffff",
    high = "#000000",
    guide = guide_colourbar(reverse = TRUE)
  ) &
  theme(legend.position = "left",
        legend.box.margin = margin(0, 20, 0, 0))

ggsave(
  here::here("figures/sys-rev/cohortLocations.png"),
  p3,
  height = 6,
  width = 8
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- preprintWeights-table

preprintWeights_table <-
  rio::import("data/sys-rev/preprintWeights.csv")

preprintWeights_table$Weight[1] <- mrStatinPreprintWeight

if (doc_type == "docx") {
  apply_flextable(preprintWeights_table, caption = "(ref:preprintWeights-caption)")
} else{
  knitr::kable(
    preprintWeights_table,
    format = "latex",
    caption = "(ref:preprintWeights-caption)",
    caption.short = "(ref:preprintWeights-scaption)",
    booktabs = TRUE,
    align = "cccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- preprintGrowthSetup

# medrxivr::mx_snapshot() %>%
#   # Keep first version of each record
#   group_by(doi) %>%
#   slice(which.min(version)) %>%
#   # Generate grouping variables
#   mutate(month = paste0(lubridate::year(date),"-",lubridate::month(date))) %>%
#   group_by(month) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
#   arrange(month) %>%
#   mutate(new_papers_cumulative = cumsum(n)) %>%
#   filter(month != as.Date("2021-08-01"))

n_at_search <-
  c(
    med = read.csv(here::here("data/sys-rev/medrxiv_growth.csv")) %>%
      mutate(month = as.Date(month, format = "%Y-%m-%d")) %>%
      filter(month < as.Date("2019-07-19")) %>%
      pull(new_papers_cumulative) %>%
      last() %>%
      comma()
  )

n_at_search["bio"] <-
  rbiorxiv::biorxiv_summary(interval = "m", format = "df") %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
  filter(month < as.Date("2019-07-19")) %>%
  pull(new_papers_cumulative) %>%
  last() %>%
  comma()

p_med <-
  read.csv(here::here("data/sys-rev/medrxiv_growth.csv"),
           stringsAsFactors = F) %>%
  mutate(month = as.Date(month, format = "%Y-%m-%d")) %>%
  ggplot() +
  geom_bar(aes(x = month, y = new_papers_cumulative),
           fill = "#989898",
           stat = "identity") +
  labs(x = "",
       y = "medRxiv") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b-%y",
    expand = c(0, 0),
    limits = as.Date(c("2014-01-01", "2021-08-01"))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 135000),
    expand = c(0, 0)
  ) +
  geom_vline(xintercept = as.Date("2019-07-20")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.y.left = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.1, 0, 0, 0), "cm")
  )

p_bio <-
  rbiorxiv::biorxiv_summary(interval = "m", format = "df") %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
  ggplot() +
  geom_bar(aes(x = month, y = new_papers_cumulative),
           fill = "#989898",
           stat = "identity") +
  labs(x = "",
       y = "bioRxiv") +
  geom_vline(xintercept = as.Date("2019-07-20")) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = c(rep(" ", 11), "\nDate of searches\nfor this review", rep(" ", 4)),
    expand = c(0, 0),
    limits = as.Date(c("2014-01-01", "2021-08-01"))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 135000),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(vjust = 0.5, colour = "black"),
    axis.title.y.left = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "npc")
  )

ggsave(
  "figures/sys-rev/preprint_growth.png",
  p_bio / p_med,
  height = 7,
  width = 6
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- rctStatinDementia

library(metafor)
# RCTs

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  # Extract two trials
  filter(study_id %in% c("10562", "90003")) %>%
  mutate(across(c(
    point_estimate, starts_with(c("number_", "cases_")), ends_with("_ci")
  ), as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  mutate(
    author = ifelse(grepl("Heart", author), "HPS", author),
    author = ifelse(grepl("FDA", author), "JUPITER", author)
  ) %>%
  mutate(number_exposed = as.numeric(number_exposed)) %>%
  mutate(
    tpos = cases_exposed,
    tneg = number_exposed - cases_exposed,
    cpos = cases_unexposed,
    cneg = number_unexposed - cases_unexposed
  )

# Get risk of bias data
dat_rob <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 3) %>%
  janitor::clean_names() %>%
  filter(result_id %in% dat$result_id) %>%
  select(-c(result, d6, d7, summary_of_biases, comments))

dat <-
  escalc(
    measure = "OR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat
  )

res <- metafor::rma(yi, vi, dat = dat)

rct_statin_acd <- estimate(res$beta, res$ci.lb, res$ci.ub, exp = T)

try(dev.off(), silent = T)

png(
  here::here("figures/sys-rev/fp_rct_statins_Dementia.png"),
  pointsize = 15,
  width = 1750,
  height = 550,
  res = 100
)

forest_strata_rob(dat,
                  dat_rob,
                  at = log(c(0.3, 1, 3)),
                  rob_me = "Low",
                  xlab = "Odds ratio")

text(-5, c(1, 2), cex = 1.2, dat$tpos)
text(-4, c(1, 2), cex = 1.2, dat$tneg)
text(-2.5, c(1, 2), cex = 1.2, dat$cpos)
text(-1.5, c(1, 2), cex = 1.2, dat$cneg)
text(c(-5,-4,-2.5,-1.5),
     4.5,
     c("D+", "D-", "D+", "D-"),
     cex = 1.2,
     font = 2)

text(-7, 5, "Type", cex = 1.2, font = 2)
text(-7, c(1, 2), cex = 1.2, dat$exposure)

text(c(-4.5,-2),
     5,
     c("Statin", "Placebo"),
     cex = 1.2,
     font = 2)
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- obsStatins

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(
    exposure_category == "Drug",
    measure == "HR",
    exposure == "Statin - Ever",
    outcome %in% c("Dementia", "AD", "VaD"),
    primary == 1
  ) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  rename("n" = number_exposed) %>%
  select(
    result_id,
    author,
    year,
    sex,
    exposure_category,
    dose_range,
    age,
    n,
    exposure,
    outcome,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  group_by(outcome)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))

purrr::map(dat,  ~ save_fp(.x, at = log(c(0.3, 1, 3)), xlab = "Hazard ratio"))

obsStatins <- purrr::map(dat,  ~ meta_estimate(.x, type = "HR"))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- obsFibrates

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(exposure_category == "Drug",
    measure == "HR",
    exposure == "Fibrate",
    outcome %in% c("Dementia", "AD", "VaD")) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  rename("n" = number_exposed) %>%
  select(
    result_id,
    author,
    year,
    sex,
    exposure_category,
    dose_range,
    age,
    n,
    exposure,
    outcome,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  group_by(outcome)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))

purrr::map(dat,  ~ save_fp(.x, at = log(c(0.3, 1, 3)), xlab = "Hazard ratio"))

obsFibrates <- purrr::map(dat,  ~ meta_estimate(.x, type = "HR"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- Hypercholesterolemia

# TODO remove OR results

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(
    is.na(direction),
    !grepl("Critical", comments),
    grepl("Hyperch", exposure_category),
    point_estimate != "Missing",
    # measure = "HR",
    exposure != "TG",
    primary == 1
  ) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  mutate(author = case_when(!is.na(age) ~ paste0(author, " (", age, ")"),
                            T ~ author)) %>%
  rename("n" = number_exposed) %>%
  select(
    result_id,
    author,
    year,
    sex,
    exposure_category,
    dose_range,
    age,
    n,
    exposure,
    outcome,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  group_by(outcome)

obsHyperchol_n <- get_study_n(dat)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))


# obsHyperchol_cite <- purrr::map(dat,get_citations_per_analysis)
# obsHyperchol_n <- purrr::map(dat,get_study_n)

purrr::map2(
  dat,
  c("Some concerns", "Some concerns", "Some concerns"),
  ~ save_fp(
    .x,
    rob_me = .y,
    at = log(c(.3, 1, 3)),
    preface = "hyperchol"
  )
)

obsHyperchol <- purrr::map(dat,  ~ meta_estimate(.x, type = "HR"))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- riskOfBias

critical <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(
    is.na(direction),
    grepl("[Cc]ritical", comments),
    grepl("Hyperch", exposure_category),
    point_estimate != "Missing",
    primary == 1
  )

critical_citations <- get_citations_per_analysis(critical)

n_critical <- critical %>%
  group_by(study_id) %>%
  n_distinct()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- mrStatins

dat <-
  rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  general_filters() %>%
  filter(type == "MR",
         exposure == "HMGCR") %>%
  rename(n = number_exposed) %>%
  mutate(across(c(n, point_estimate, se, ends_with("_ci")), as.numeric)) %>%
  rowwise() %>%
  clean_effects() %>%
  group_by(outcome)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))

# Generate forest plot for AD outcome, as it is the only one with >1 result
dat_ad <- dat$AD %>%
  arrange(desc(author))

dat_rob <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 3) %>%
  janitor::clean_names() %>%
  filter(result_id %in% dat_ad$result_id) %>%
  select(-c(d6, d7, result, summary_of_biases, comments))


png(
  here::here("figures/sys-rev/fp_MR_HMGCR_AD.png"),
  width = 1750,
  height = 550,
  pointsize = 15,
  res = 100
)

forest_strata_rob(
  dat_ad,
  dat_rob,
  sei = sei,
  at = log(c(0.3, 1, 3)),
  rob_me = "Some concerns",
  xlab = "Odds ratio"
)

text(-5, c(1, 2), cex = 1.2, dat_ad$snps)
text(-5, 5, cex = 1.2, "HMGCR SNPS", font = 2)

dev.off()

mrStatin <- purrr::map(dat,  ~ meta_estimate(.x, type = "RR"))

# Get weight from fixed effect meta-analysis
mrStatinPreprintWeight <- dat$AD %>%
  metafor::rma(yi - yi,
               sei = sei,
               data = .,
               method = "FE") %>%
  stats::weights() %>%
  last() %>%
  comma(.) %>%
  paste0("%")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- mrLipidsAD

dat <-
  rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  general_filters() %>%
  filter(type == "MR",
         # Just pick one study
         study_id == 2439,
         
         exposure_category != "Drug") %>%
  rename(n = number_exposed) %>%
  mutate(across(c(n, point_estimate, se, ends_with("_ci")), as.numeric)) %>%
  rowwise() %>%
  clean_effects() %>%
  group_by(exposure)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))

# No images for this one, as duplicated analysis using summary stats.

mrLipidsAD <- purrr::map(dat,  ~ meta_estimate(.x, type = "RR"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- LipidsSD

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  mutate(exposure_category = ifelse(exposure_category == "Lipid", "Lipids", exposure_category)) %>%
  mutate(author = ifelse(author == "Tynkkynen", paste0(author, " (", cohort, ")"), author)) %>%
  filter(
    is.na(mr_type),
    is.na(age),
    is.na(sex),
    is.na(direction),
    !grepl("Critical", comments),
    exposure_category %in% c("Lipids", "Lipid"),
    exposure %in% c("TC", "TG", "LDL-c", "HDL-c"),
    outcome %in% c("Dementia", "AD", "VaD"),
    point_estimate != "Missing"
  ) %>%
  rename("n" = number_exposed) %>%
  select(
    result_id,
    author,
    year,
    sex,
    exposure,
    dose_range,
    age,
    n,
    exposure,
    outcome,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  nest_by(outcome, exposure)

names <- group_keys(dat) %>%
  mutate(names = paste0(outcome, "_", stringr::str_remove(exposure, "-.+"))) %>%
  pull(names)

dat$data <- dat$data %>%
  set_names(names)

# obsLipids_cite <- purrr::map(dat,get_citations_per_analysis)
# obsLipids_n <- purrr::map(dat,get_study_n)

purrr::pmap(
  list(
    dat = dat$data,
    name = names(dat$data),
    rob_me = rep("High", 12),
    title = rep(
      c(
        "High-density lipoprotein cholesterol",
        "Low-density lipoprotein cholesterol",
        "Total cholesterol",
        "Triglycerides"
      ),
      3
    )
  ),
  purrr::possibly(
    ~ save_fp(
      ..1,
      preface = ..2,
      rob_me = ..3,
      title = ..4,
      at = log(c(0.3, 1, 3))
    ),
    otherwise =  NULL
  )
)

obsLipids <- purrr::map(dat$data,  ~ meta_estimate(.x, type = "HR"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- doseResponse

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(exposure_category == "Lipids - DR")

n_dr_initial <- dat %>%
  distinct(study_id) %>%
  nrow()

dat <- dat %>%
  filter(
    !is.na(number_exposed),
    !grepl("Quartile|Highest", dose_range),
    !is.na(cases),
    !grepl("Non",exposure),
    !is.na(dose_range),
    cases != "NR"
  ) %>%
  rename("n" = number_exposed) %>%
  select(
    study_id,
    author,
    year,
    exposure,
    outcome,
    dose_range,
    n,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  # Deal with finding SE
  mutate(
    point_estimate = ifelse(point_estimate == "Ref", 1, point_estimate),
    lower_ci = ifelse(lower_ci == "Ref", 1, lower_ci),
    upper_ci = ifelse(upper_ci == "Ref", 1, upper_ci)
  ) %>%
  mutate(across(c(
    cases, n, point_estimate, ends_with("_ci")
  ), as.numeric)) %>%
  mutate(se = (upper_ci - lower_ci) / 3.92) %>%
  select(-ends_with("_ci")) %>%
  # Deal with doses
  mutate(dose_range = ifelse(
    stringr::str_detect(dose_range, "^>"),
    paste0(dose_range, "<"),
    dose_range
  )) %>%
  mutate(dose_range = stringr::str_replace(dose_range, "^>", "")) %>%
  tidyr::separate(dose_range, c("dose_lower", "dose_upper"), sep = ">|<|-") %>%
  mutate(across(starts_with("dose"), as.numeric)) %>%
  mutate(dose_width = dose_upper - dose_lower) %>%
  group_by(study_id) %>%
  mutate(dose_width_av = mean(dose_width, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    dose_lower = ifelse(is.na(dose_lower), dose_upper -  dose_width_av, dose_lower),
    dose_upper = ifelse(is.na(dose_upper), dose_lower +  dose_width_av, dose_upper)
  ) %>%
  select(-contains("width")) %>%
  mutate(dose = dose_lower + dose_upper / 2) %>%
  select(-starts_with("dose_")) %>%
  mutate(dose = ifelse(dose < 20, dose * 38.67, dose)) %>%
  mutate(loghr = log(point_estimate)) %>%
  mutate(type = as.factor("ir")) %>%
  select(study_id,
         author,
         year,
         exposure,
         outcome,
         type,
         dose,
         cases,
         n,
         loghr,
         se)

n_dr_final <- dat %>%
  distinct(study_id) %>%
  nrow()

n_dr_excluded <- n_dr_initial - n_dr_final
  
dat <- dat %>%  
  nest_by(outcome, exposure)

names <- group_keys(dat) %>%
  mutate(names = paste0(outcome, "_", stringr::str_remove(exposure, "-.+"))) %>%
  pull(names)

dat$data <- dat$data %>%
  set_names(names)

purrr::pmap(
  list(
    dat = dat$data,
    xref = rep(c(40,100,200,150,40,100,200,40,100,200,150)),
    title = c(
        "High-density lipoprotein cholesterol",
        "Low-density lipoprotein cholesterol",
        "Total cholesterol",
        "Triglycerides",
        "High-density lipoprotein cholesterol",
        "Low-density lipoprotein cholesterol",
        "Total cholesterol",
        "High-density lipoprotein cholesterol",
        "Low-density lipoprotein cholesterol",
        "Total cholesterol",
        "Triglycerides"
    ),
    preface = names(dat$data)
  ),
  purrr::possibly(
    ~ save_dr(
      ..1,
      xref = ..2,
      title = ..3,
      preface = ..4
    ),
    otherwise =  NULL
  )
)





#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- mrDuplicationSetup

res <-
  rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  janitor::clean_names() %>%
  filter(exclude != "Y",
         study_id != 99999) %>%
  filter(type == "MR",
         exposure %in% c("LDL-c")) %>%
  rename("n" = number_exposed) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  arrange(author) %>%
  rowwise() %>%
  clean_effects()

png(
  here::here("figures/sys-rev/mrDuplication.png"),
  width = 600,
  height = 300
)
forest.default(
  res$yi,
  sei = res$sei,
  transf = exp,
  at = c(.8, 1, 1.2),
  refline = 1,
  rows = c(8:2),
  slab = paste0(res$author, ", ", res$year),
  ilab = cbind(res$exposure_gwas, res$outcome_gwas, res$snps),
  xlim = c(-0.25, 1.5),
  ilab.xpos = c(0.25, 0.5, .75),
  header = "Author & Year",
  xlab = "Odds ratio"
)

text(
  c(-0.09, 0.25, 0.5, .75, 1.325),
  1,
  c("Mukherjee, 2013", "NR", "ADGC", "NR", "Missing"),
  font = 3
)

text(c(0.25, 0.5), 8.35, c("Exposure", "Outcome"), font = 2)
text(c(.375), 9, c("GWAS"), font = 2)

text(c(.75), 9, c("# SNPS"), font = 2)
dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- mrAssumptionsSetup

library(dagitty)
library(ggdag)

dag <- dagitty::dagitty(
  "dag {
    G -> X
    o <- C -> X
    X -> Y
    X <- U -> Y
    X <- C -> Y
    X -> Y
    I [pos=\"-.2,.01\"]
    II [pos=\"-.2,.08\"]
    III [pos=\".1,-.05\"]
    bb =\"-2,-2,2,2\"
    C [pos=\"0.25,0.1\"]
    G [pos=\"-.5,0\"]
    X [exposure,pos=\"0,0\"]
    Y [outcome,pos=\"0.5,0\"]
  }"
) %>% ggdag_classic() +
  theme_dag() +
  geom_curve(
    data = data.frame(
      x = -0.481435967917461,
      y = 0.00985575213338419,
      xend = 0.220459770229494,
      yend = 0.0995368157305137
    ),
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    angle = 90L,
    colour = "black",
    linetype = "dashed",
    curvature = -0.17,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) + geom_curve(
    data = data.frame(
      x = -0.483240327141489,
      y = -0.00951911045017763,
      xend = 0.478004935385662,
      yend = -0.00700575091499871
    ),
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    angle = 90L,
    linetype = "dashed",
    colour = "black",
    curvature = 0.25,
    inherit.aes = FALSE,
    show.legend = FALSE
  )



ggplot2::ggsave(
  filename = file.path("figures", "sys-rev", "mrAssumptions.png"),
  dag,
  height = 4.2
)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- lipidFrationsSetup
# Creates composite images for lipid fraction forest plots

# AD
img <- list()
img$TC <- readPNG(here::here("figures/sys-rev/fp_obs_AD_TC_.png"))
img$LDL <- readPNG(here::here("figures/sys-rev/fp_obs_AD_LDL_.png"))
img$HDL <- readPNG(here::here("figures/sys-rev/fp_obs_AD_HDL_.png"))
img$TG <- readPNG(here::here("figures/sys-rev/fp_obs_AD_TG_.png"))


# setup plot
try(dev.off())
pdf(
  here::here("figures/sys-rev/fp_lipids_composite_AD.pdf"),
  width = 8,
  height = 12
)
par(mai = rep(0, 4)) # no margins

# layout the plots into a matrix w/ 12 columns, by row
layout(matrix(1:4, ncol = 1, byrow = TRUE))

# do the plotting
for (i in 1:4) {
  plot(
    NA,
    xlim = 0:1,
    ylim = 0:1,
    bty = "n",
    axes = 0,
    xaxs = 'i',
    yaxs = 'i'
  )
  rasterImage(img[[i]], 0, 0, 1, 1)
}

# write to PDF
dev.off()

# Dementia
img <- list()
img$TC <-
  readPNG(here::here("figures/sys-rev/fp_obs_Dementia_TC_.png"))
img$LDL <-
  readPNG(here::here("figures/sys-rev/fp_obs_Dementia_LDL_.png"))
img$HDL <-
  readPNG(here::here("figures/sys-rev/fp_obs_Dementia_HDL_.png"))
img$TG <-
  readPNG(here::here("figures/sys-rev/fp_obs_Dementia_TG_.png"))

# setup plot
try(dev.off())
pdf(
  here::here("figures/sys-rev/fp_lipids_composite_Dementia.pdf"),
  width = 8,
  height = 12
)
par(mai = rep(0, 4)) # no margins

# layout the plots into a matrix w/ 12 columns, by row
layout(matrix(1:4, ncol = 1, byrow = TRUE))

# do the plotting
for (i in 1:4) {
  plot(
    NA,
    xlim = 0:1,
    ylim = 0:1,
    bty = "n",
    axes = 0,
    xaxs = 'i',
    yaxs = 'i'
  )
  rasterImage(img[[i]], 0, 0, 1, 1)
}

# write to PDF
dev.off()


## Don't need to do for vascular dementia as only one lipid fraction (TC) has
## greater than one study presenting results on it.


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- obsStatinsOR

dat <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(
    exposure_category == "Drug",
    measure %in% c("RR","OR"),
    exposure == "Statin - Ever",
    outcome %in% c("Dementia", "AD", "VaD"),
    primary == 1
  ) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  rename("n" = number_exposed) %>%
  select(
    result_id,
    author,
    year,
    sex,
    exposure_category,
    dose_range,
    age,
    n,
    exposure,
    outcome,
    cases,
    point_estimate,
    ends_with("_ci")
  ) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  group_by(outcome)

dat <- dat %>%
  group_split() %>%
  set_names(unlist(group_keys(dat)))

obsStatinsOR <- purrr::map(dat,  ~ meta_estimate(.x, type = "HR"))
