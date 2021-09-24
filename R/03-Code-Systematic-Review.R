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

prisma_df <-
  read.csv(here::here("data/sys-rev/PRISMAflow.csv"),
           stringsAsFactors = F)

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
# ---- agree-setup

# Inter
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

# Intra
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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- agreeInter-table

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

toc_df <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),which = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(exclude)) %>%
  select(study_id, author, year, number_participants, subtype, age_at_baseline, female_percent, location, data_source, group_sizes_if_applicable) %>%
  mutate(type = subtype) %>%
  mutate(author = ifelse(grepl("Heart Protection",author),"HPS",author)) %>%
  mutate(age_at_baseline = stringr::str_remove_all(age_at_baseline," "),
         female_percent = stringr::str_remove_all(female_percent," "),
         group_sizes_if_applicable = stringr::str_remove_all(group_sizes_if_applicable," ")) %>%
  rowwise() %>%
  mutate(age_combo = ifelse(grepl("\\|",age_at_baseline),combine_age(age_at_baseline,group_sizes_if_applicable), age_at_baseline)) %>%
  mutate(female_combo = ifelse(grepl("\\|",female_percent),combine_female(female_percent,group_sizes_if_applicable), female_percent)) %>%
  mutate(female_combo = case_when(female_combo=="64.400000000000006"~"64.4", T ~ female_combo),
         age_combo = case_when(age_combo=="74.900000000000006"~"74.9", T ~ age_combo))


# toc_df %>%
#   filter(is.na(exclude)) %>%
#   distinct(study_id,.keep_all = T) %>%
#   count()

toc_df2 <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),which = 2) %>%
  janitor::clean_names() %>%
  filter(!is.na(study_id)) %>%
  mutate(exposure = ifelse(stringr::str_detect(exposure,"Statin -"),"Statin",stringr::str_remove(exposure,"- .*$"))) %>%
  mutate(exposure = ifelse(exposure_category == "Hypercholesterolemia","Hypercholesterolemia",exposure)) %>%
  group_by(study_id) %>% 
  summarize(Criteria = paste0(unique(diagnostic_criteria), collapse = "; "),
            Exposures = paste0(unique(exposure), collapse = "; "), 
            Outcomes = paste0(unique(outcome), collapse = "; ")) %>%
  ungroup()

# toc_df2 %>%
#   filter(exclude != "Y") %>%
#   filter(type == "NRSI") %>%
#   filter(measure == "RR")
#   group_by(measure) %>%
#   count()

p_type <- 
  toc_df %>%
  group_by(year) %>%
  count(type) %>%
  ggplot(aes(x = year, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = 1:12, expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1995,2020, by = 5)) +
  labs(fill = "Study design",
       x = "Year of publication",
       y= "Number of studies") +
  scale_fill_grey() +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

ggsave(here::here("figures/sys-rev/type_by_year.png"), p_type, height = 3)

studyCharacteristics_table <- left_join(toc_df, toc_df2, by =c("study_id"="study_id")) %>%
  mutate(Study = paste(author, year)) %>%
  mutate(type = factor(type,levels = c("RCT","NRSI","NRSE","MR")),) %>%
  
  # mutate(location = paste0(data_source," (",location,")")) %>%
  select(Study, type, location, number_participants, age_combo, female_combo, Exposures, Outcomes, Criteria, -c(study_id,author, year)) %>%
  rename("Location" = location,
         "Type" = type,
         "N" = number_participants,
         "Female (%)" = female_combo,
         "Age at baseline" = age_combo,
         "Diagnostic criteria" = Criteria,
         ) %>%
  tidy_nums() %>%
  arrange(desc(Type),Study) %>%
  select(-Type)



if(doc_type == "docx") {
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
    kable_styling(latex_options = c("HOLD_position","repeat_header"), font_size = 7) %>%
    kableExtra::column_spec(0, width = "20em") %>%
    kableExtra::column_spec(c(1,3:8), width = "10em") %>%
    kableExtra::column_spec(2, width = "5em") %>%
    kableExtra::group_rows(group_label = "RCTS", start_row = 1, end_row = 2, hline_after = T,
                           extra_latex_after = "\\addlinespace") %>%
    kableExtra::group_rows(group_label = "NRSI", start_row = 3, end_row = 33, hline_after = T,
                           extra_latex_after = "\\addlinespace") %>%
    kableExtra::group_rows(group_label = "NRSE", start_row = 34, end_row = 74, hline_after = T,
                           extra_latex_after = "\\addlinespace") %>%
    kableExtra::group_rows(
      group_label = "MR",
      start_row = 75,
      end_row = 81,
      hline_after = T,
      extra_latex_after = "\\addlinespace"
    )
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortLocationsSetup

world <- toc_df %>%
  count(location) %>%
  right_join(
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% filter(type != "Dependency"),
    by = c("location" = "name")
  )

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
)  & labs(fill="No. of\nstudies") &
  scale_fill_gradient2(low = "#ffffff",
                       mid = "grey50",
                       midpoint = 8, na.value = "#ffffff",
                       high = "#000000", guide = guide_colourbar(reverse = TRUE)) &
  theme(legend.position = "left",
        legend.box.margin=margin(0,20,0,0))

ggsave(
  here::here("figures/sys-rev/cohortLocations.png"),
  p3,
  height = 6,
  width = 8
)

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

n_at_search <- c(med = read.csv(here::here("data/sys-rev/medrxiv_growth.csv")) %>%
  mutate(month = as.Date(month, format = "%Y-%m-%d")) %>%
  filter(month < as.Date("2019-07-19")) %>%
  pull(new_papers_cumulative) %>%
  last() %>%
    comma())

n_at_search["bio"] <- rbiorxiv::biorxiv_summary(interval = "m", format = "df") %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
  filter(month < as.Date("2019-07-19")) %>%
  pull(new_papers_cumulative) %>%
  last() %>%
  comma()

p_med <- read.csv(here::here("data/sys-rev/medrxiv_growth.csv"), stringsAsFactors = F) %>%
  mutate(month = as.Date(month, format = "%Y-%m-%d")) %>%
  ggplot() +
  geom_bar(aes(x = month, y = new_papers_cumulative),
           fill = "#989898",
           stat = "identity") +
  labs(x = "",
       y= "medRxiv") +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b-%y",
               expand = c(0,0), 
               limits = as.Date(c("2014-01-01","2021-08-01"))) +
  scale_y_continuous(labels = scales::comma, limits = c(0,135000), expand = c(0,0)) +
  geom_vline(xintercept = as.Date("2019-07-20")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.y.left = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.1,0,0,0), "cm")
  )

p_bio <- rbiorxiv::biorxiv_summary(interval = "m", format = "df") %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
  ggplot() +
  geom_bar(aes(x = month, y = new_papers_cumulative),
           fill = "#989898",
           stat = "identity") +
  labs(x = "",
       y = "bioRxiv") +
  geom_vline(xintercept = as.Date("2019-07-20")) +
  scale_x_date(date_breaks = "6 months",
               date_labels = c(rep(" ",11),"\nDate of searches\nfor this review",rep(" ",4)),
               expand = c(0,0), 
               limits = as.Date(c("2014-01-01","2021-08-01"))) +
  scale_y_continuous(labels = scales::comma, limits = c(0,135000), expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(vjust = 0.5, colour = "black"),
    axis.title.y.left = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0,0,0,0), "npc")
  )

ggsave("figures/sys-rev/preprint_growth.png", p_bio/p_med, height = 7, width = 6)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- primaryFigures

library(metafor)
# RCTs

results_statins_rct <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 2) %>%
  general_filters() %>%
  # Extract two trials
  filter(study_id %in% c("10562","90003")) %>%
  mutate(across(c(point_estimate,starts_with(c("number_","cases_")), ends_with("_ci")),as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci))/3.92) %>%
  mutate(author = ifelse(grepl("Heart",author),"HPS",author),
         author = ifelse(grepl("FDA",author),"JUPITER",author)) %>%
  mutate(number_exposed = as.numeric(number_exposed)) %>%
  mutate(tpos = cases_exposed,
         tneg = number_exposed - cases_exposed,
         cpos = cases_unexposed,
         cneg = number_unexposed - cases_unexposed)

dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=results_statins_rct)

res <- metafor::rma.uni(data = dat,
                        yi, vi, 
                        slab = paste(author, year))

# TODO Update with proper ROB assessments
dat_rob <- robvis::data_rob2[1:2,]
dat_rob$Study <- res$slab
dat_rob[2,2:7] <- "Low"

try(dev.off(),silent = T)
png(here::here("figures/sys-rev/fp_statins_any_rct.png"), height = 300, width = 800)

robvis::rob_append_to_forest(
  res,
  rob_data = dat_rob,
  rob_caption = F,
  transf = exp,
  refline = 1,
  alim = c(0.5, 2),
  ilab = cbind(
    results_statins_rct$exposure_category,
    results_statins_rct$tpos,
    results_statins_rct$tneg,
    results_statins_rct$cpos,
    results_statins_rct$cneg
  ),
  xlab = "Odds Ratio",
  ilab.xpos = c(-10, -6.5, -5, -3, -1.5),
  at = c(.5, 1, 2),
  xlim = c(-16, 7),
  header = "Author(s) and Year"
)

text(c(-6.5,-5,-3,-1.5), 3.5, c("D+", "D-", "D+", "D-"), font=2)
text(c(-5.75,-2.25),     4.5, c("Statin", "Placebo"),   font=2)
text(-10, 4, "Type",  font =2)
# text(-15, -1, pos=4,  bquote(paste("RE Model (Q = ",
#                                             .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
#                                             ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
#                                             .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- doseResponse

results_tc_dr <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 2) %>%
  general_filters() %>%
  filter(exposure_category == "Lipids - DR", 
         author != "Beydoun",
         !is.na(number_exposed),
         !grepl("Quartile",dose_range),
         !is.na(cases), 
         cases != "NR") %>% 
  rename("n" = number_exposed) %>%
  select(study_id, author, year,exposure, outcome, dose_range, n, cases,point_estimate,ends_with("_ci")) %>%
  # Deal with finding SE
  mutate(point_estimate = ifelse(point_estimate == "Ref",1,point_estimate),
        lower_ci = ifelse(lower_ci == "Ref",1,lower_ci),
         upper_ci = ifelse(upper_ci == "Ref",1,upper_ci)) %>%
  mutate(across(c(cases, n, point_estimate, ends_with("_ci")),as.numeric)) %>%
  mutate(se = (upper_ci-lower_ci)/3.92) %>%
  select(-ends_with("_ci")) %>%
  # Deal with doses
  mutate(dose_range = ifelse(stringr::str_detect(dose_range,"^>"), paste0(dose_range,"<"), dose_range)) %>%
  mutate(dose_range = stringr::str_replace(dose_range,"^>","")) %>%
  tidyr::separate(dose_range, c("dose_lower", "dose_upper"), sep = ">|<|-") %>%
  mutate(across(starts_with("dose"),as.numeric)) %>%
  mutate(dose_width = dose_upper - dose_lower) %>%
  group_by(study_id) %>%
  mutate(dose_width_av = mean(dose_width, na.rm = T)) %>%
  ungroup() %>%
  mutate(dose_lower = ifelse(is.na(dose_lower),dose_upper -  dose_width_av, dose_lower),
         dose_upper = ifelse(is.na(dose_upper),dose_lower +  dose_width_av, dose_upper)) %>%
  select(-contains("width")) %>%
  mutate(dose = dose_lower + dose_upper/2) %>%
  select(-starts_with("dose_")) %>%
  mutate(dose = ifelse(dose<20,dose*38.67,dose)) %>%
  mutate(loghr = log(point_estimate)) %>%
  mutate(type = as.factor("ir")) %>%
  select(study_id, author,year,exposure,outcome, type, dose, cases,n, loghr, se)


t<- results_tc_dr %>%
  group_by(exposure, outcome) %>%
  group_split()

ggplot(t[[4]], aes(dose, loghr, group = study_id, shape = author)) + 
  geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
  xlab("Alcohol intake (mL/day)")

lin <-
  dosresmeta::dosresmeta(
    formula = loghr~dose,
    id = study_id,
    se = se,
    type = type,
    cases = cases,
    n = n,
    data = t[[4]]
  )

pred <- predict(lin,data.frame(dose = seq(100,250,10)))
round(pred, 2)

with(predict(lin, xref = 100,data.frame(dose = seq(0,300,10))), {
  plot(dose, pred, type = "l", ylab = "Relative risk", las = 1,
       xlab = "Body Mass Index (BMI)", bty = "l")
  lines(dose, ci.lb, lty = "dashed")
  lines(dose, ci.ub, lty = "dashed")
})



knots <- round(with(t[[4]], quantile(dose, probs = c(.25, .5, .75) )), 2)

spl <-
  dosresmeta::dosresmeta(
    formula = loghr~rms::rcs(dose, knots),
    id = study_id,
    se = se,
    type = type,
    cases = cases,
    n = n,
    data = t[[4]]
  )


pred <- predict(spl,data.frame(dose = seq(100,250,10)))
round(pred, 2)

newdata = data.frame(dose = seq(0,250, 10))
with(predict(spl, newdata, xref = 100), {
  plot(
    get("rms::rcs(dose, knots)dose"),
    pred,
    type = "l",
    ylab = "Relative risk",
    ylog = F,
    ylim = c(-1,1),
    las = 1,
    xlab = "Alcohol intake, grams/day",
    bty = "l"
  )
  lines(get("rms::rcs(dose, knots)dose"), ci.lb, lty = "dashed")
  lines(get("rms::rcs(dose, knots)dose"), ci.ub, lty = "dashed")
  lines(y=c(0,0),x=c(-10,250), lty = 8)
})


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- Hypercholesterolemia

res <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 2) %>%
  general_filters() %>%
  filter(is.na(age),
         is.na(sex),
         is.na(direction),
         !grepl("Critical", comments),
         grepl("Hyperch",exposure_category), 
         !grepl("<", dose_range),
         point_estimate != "Missing") %>%
  rename("lower_ci" = upper_95_percent, 
         "upper_ci" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases,point_estimate,ends_with("_ci")) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")),as.numeric)) %>%
  mutate(point = log(point_estimate),
       SE = (log(upper_ci) - log(lower_ci))/3.92) %>%
  arrange(author, year) %>%
  mutate(study_name = paste(author, year, "-", sex, "/", age))

t<- res %>%
  group_by(exposure, outcome) %>%
  group_split()

purrr::

# purrr::map(t,save_fp)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- LipidsSD

res <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 2) %>%
  general_filters() %>%
  mutate(exposure_category = ifelse(exposure_category=="Lipid","Lipids",exposure_category)) %>%
  mutate(author = ifelse(author=="Tynkkynen",paste(author,"-",cohort),author)) %>%
  filter(is.na(mr_type),
         is.na(age),
         is.na(sex),
         is.na(direction),
         !grepl("Critical", comments),
        exposure_category %in% c("Lipids","Lipid"),
         point_estimate != "Missing"
         ) %>%
  rename("lower_ci" = upper_95_percent, 
         "upper_ci" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases,point_estimate,ends_with("_ci")) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")),as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci))/3.92) %>%
  arrange(author, year) %>%
  mutate(study_name = paste(author, year, "-", sex, "/", age))

t <- res %>%
  group_by(exposure, outcome) %>%
  group_split()

# purrr::map(t,save_fp)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- obsStatins

res <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 2) %>%
  general_filters() %>%
  filter(exposure_category == "Drug",
         measure == "HR",
         is.na(sex),
         is.na(age)
  ) %>%
  rename("lower_ci" = upper_95_percent, 
         "upper_ci" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases,point_estimate,ends_with("_ci")) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")),as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci))/3.92) %>%
  arrange(author, year)

set.seed(0)
t <- res %>%
  mutate(bias = sample(
    c("Serious", "Moderate"),
    size = n(),
    replace = T,
    prob = c(.68, .32)
  )) %>%
  group_by(exposure, outcome, bias) %>%
  group_split()

obs_tri_res_statins <- purrr::map_df(t, meta_grouped) %>%
  mutate(Type = "Observational Study") %>%
  filter(exposure == "Statin - Ever",
         outcome %in% c("Dementia", "AD", "VaD")) %>%
  mutate(exposure = "Statin") %>%
  select(Type, everything())

# purrr::map(t,save_fp)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- mrDuplicationSetup

res <- rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  general_filters() %>%
  filter(type == "MR",
         exposure %in% c("LDL-c"),
         study_id != "90005") %>%
  rename("n" = number_exposed) %>%
  mutate(across(c(n, point_estimate, ends_with("_ci")),as.numeric)) %>%
  rowwise() %>%
  clean_effects()


png(here::here("figures/sys-rev/mrDuplication.png"), width = 600, height = 225)
forest.default(
  res$yi,
  sei = res$sei,
  transf = exp,
  refline = 1,
  slab = paste(res$author, ", ", res$year),
  ilab = res$snps, 
  xlim = c(0.4,1.5),
  ilab.xpos = .75, 
  header = "Author & Year",
  xlab = "Odds ratio"
)
text(c(.75), 6, c("# SNPS"), font=2)
dev.off()

