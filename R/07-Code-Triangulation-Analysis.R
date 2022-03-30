#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- thesisOverview-table ----

thesisOverview_table <- read.csv("data/background/thesisOverview.csv") %>%
  mutate("Exposure/ Intervention" = Exposure.Intervention) %>%
  mutate("Research Question" = Research.Question) %>%
  mutate("Contribution to evidence synthesis framework" = Contribution) %>%
  select("Chapter","Research Question","Exposure/ Intervention","Outcome","Contribution to evidence synthesis framework") %T>%
  write.csv("data/table_words/thesisOverview.csv") 

if(doc_type == "docx"){
  knitr::kable(thesisOverview_table,caption = "(ref:thesisOverview-caption)")
}else{
  table <- knitr::kable(
    thesisOverview_table,
    format = "latex",
    caption = "(ref:thesisOverview-caption)",
    caption.short = "(ref:thesisOverview-scaption)",
    booktabs = TRUE
  ) %>%
    column_spec(1, width = paste0(6,"em")) %>%
    column_spec(c(2,5), width = paste0(16,"em")) %>%
    column_spec(c(3,4), width = paste0(7,"em")) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(2:nrow(thesisOverview_table)-1, hline_after = TRUE)
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- robLevelsMapping-table ----

robLevelsMapping_table <- rio::import("data/tri/rob_levels_mapping.csv") %T>%
  write.csv("data/table_words/rob_levels_mapping.csv")

if(doc_type == "docx") {
  apply_flextable(robLevelsMapping_table, caption = "(ref:robLevelsMapping-caption)")
} else{
  knitr::kable(
    robLevelsMapping_table,
    format = "latex",
    caption = "(ref:robLevelsMapping-caption)",
    caption.short = "(ref:robLevelsMapping-scaption)",
    booktabs = TRUE, 
    align = "ccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    row_spec(2:nrow(robLevelsMapping_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- priorsAdd-table ----

priors_add_table <- rio::import("data/tri/priors_bias_tab.csv") %T>%
  write.csv("data/table_words/priors_add.csv")

if (doc_type == "docx") {
  apply_flextable(priors_add_table, caption = "(ref:priorsAdd-caption)")
} else{
  knitr::kable(
    priors_add_table,
    format = "latex",
    caption = "(ref:priorsAdd-caption)",
    caption.short = "(ref:priorsAdd-scaption)",
    booktabs = TRUE, 
    align = "lccc",
    col.names = c('', 'Scenario 1', 'Scenario 2', '')
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kableExtra::add_header_above(c("Bias Level" = 1, "Additive bias" = 2,"Proportional bias" = 1),bold = T, line = F) %>%
    row_spec(2:nrow(priors_add_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- singleIndirect-table ----

singleIndirect_table <- rio::import(here::here("data/tri/single_indirectness.csv")) %T>%
  write.csv("data/table_words/single_indirectness.csv")

if (doc_type == "docx") {
  apply_flextable(singleIndirect_table, caption = "(ref:singleIndirect-caption)")
} else{
  knitr::kable(
    singleIndirect_table,
    format = "latex",
    caption = "(ref:singleIndirect-caption)",
    caption.short = "(ref:singleIndirect-scaption)",
    booktabs = TRUE, 
    align = "lcccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(2:nrow(singleIndirect_table) - 1, hline_after = TRUE) %>%
    column_spec(c(1), width = paste0(5, "em")) %>%
    column_spec(c(4), width = paste0(3, "em")) %>%
    column_spec(c(5), width = paste0(6, "em")) %>%
    column_spec(c(2,3), width = paste0(8, "em"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- priorsIndirect-table ----

priors_ind_table <- rio::import("data/tri/priors_indirect_tab.csv") %T>%
  write.csv("data/table_words/priors_indirect.csv")

if (doc_type == "docx") {
  apply_flextable(priors_ind_table, caption = "(ref:priorsIndirect-caption)")
} else{
  knitr::kable(
    priors_ind_table,
    format = "latex",
    caption = "(ref:priorsIndirect-caption)",
    caption.short = "(ref:priorsIndirect-scaption)",
    booktabs = TRUE, 
    align = "lc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    row_spec(2:nrow(priors_ind_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ldlAdBIAMA ----

# read in values from spreadsheet
# TODO replace serious with high

bias_values_scenario1 <- rio::import("data/tri/bias_values_scenario1.csv") %>%
  rename_with(~paste0("bias_",.), matches("_"))

bias_values_scenario2 <- rio::import("data/tri/bias_values_scenario2.csv") %>%
  rename_with(~paste0("bias_",.), matches("_"))

indirect_values <- rio::import("data/tri/indirectness_values.csv")  %>%
  rename_with(~paste0("ind_",.), matches("_"))

# Load general data
dat_gen <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                       which = 2) %>% 
  janitor::clean_names() %>%
  mutate(author = ifelse(study_id == "2140", paste0(author, " (", cohort, ")"), author)) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  select(result_id, author, year,) %>%
  mutate(year = as.character(year)) %>%
  tibble::add_row(result_id = "999-1", author = "Chp 5 - CPRD", year = "")

# Read in result and risk of bias data
dat_rob <- read.csv("data/tri/ldl_ad_rob.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(type %in% c("NRSE","MR - LDL"), yi*-1, yi),
         d7d = case_when(type %in% c("NRSE","MR - LDL") & d7d == "Right" ~ "Left",
                         type %in% c("NRSE","MR - LDL") & d7d == "Left" ~ "Right",
                         T ~ d7d))

dat_ind <- read.csv("data/tri/ldl_rob_indirect.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(grepl("NRSE", "MR - LDL"), yi*-1, yi))

ldl_ad_citations <- get_citations_per_analysis(dat_rob)

# Save bias direction plot

dat_rob_single <- filter(dat_rob,
                         result_id=="999-1")

png(
  here::here("figures/tri/midlife_AD_single.png"),
  width = 1500,
  height = 600,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat_rob_single,
                     title = "LDL-c and AD",
                     at = log(c(0.3, 1, 3)),   
                     xlab = "Favours experimental | Favours comparator",
                     legend_cex = 0.9,
                     cex.lab	 = 0.9,
                     x_min = -5)

dev.off()

png(
  here::here("figures/tri/midlife_AD.png"),
  width = 1750,
  height = 1400,
  pointsize = 15,
  res = 100
)

forest_triangulation(
  dat_rob,
  sei = dat$sei,
  title = "LDL-c and AD",
  legend_cex = 0.9,
  xlab = "Favours experimental | Favours comparator",
  type_levels = c("MR - LDL","MR - HMGCR","NRSI","NRSE","RCT"),
  cex.lab	 = 0.9
)

dev.off()

# Prepare data for bias-/indirectness-adjusted meta-analysis

dat_final_single <-
  prep_tri_data(dat_rob_single, dat_ind, bias_values_scenario1, indirect_values)

metafor::forest(dat_final_single$yi,
                dat_final_single$vi, atransf=exp)

single_unadjusted <- estimate(
  dat_final_single$yi,
  dat_final_single$yi - 1.96 * sqrt(dat_final_single$vi),
  dat_final_single$yi + 1.96 * sqrt(dat_final_single$vi),
  exp = T,
  type = "",
  sep = "("
)

single_adjusted <- estimate(
  dat_final_single$yi_adj,
  dat_final_single$yi_adj - 1.96 * sqrt(dat_final_single$vi_adj),
  dat_final_single$yi_adj + 1.96 * sqrt(dat_final_single$vi_adj),
  exp = T,
  type = "",
  sep = "("
)

levels <- c("Low","Moderate","Serious","Critical")

levels_type <- c("MR - LDL","MR - HMGCR","NRSI","NRSE","RCT")

dat_rob <- dat_rob %>%
  mutate(type = factor(type, levels = levels_type)) %>%
  mutate(overall = factor(overall, levels =levels)) %>%
  arrange(desc(type), desc(overall), author)

dat_final_scenario1 <-
  prep_tri_data(dat_rob, dat_ind, bias_values_scenario1, indirect_values)

dat_final_scenario2 <-
  prep_tri_data(dat_rob, dat_ind, bias_values_scenario2, indirect_values)

# Run unadjusted model and extract results
model_unadj <- metafor::rma.uni(
  yi = yi,
  vi = vi,
  data = dat_final_scenario1,
  slab = paste(dat_final_scenario1$author, dat_final_scenario1$year)
)

unadj_effect <-
  estimate(exp(model_unadj$b),
           exp(model_unadj$ci.lb),
           exp(model_unadj$ci.ub),
           type = "")

unadj_tau2 <- comma(model_unadj$tau2)
unadj_I2 <- comma(model_unadj$I2)

metafor::forest(model_unadj, annotate = T, showweights = TRUE)

# Run bias-/indirectness-adjusted model using Scenario 1
model_adj_scenario1 <- metafor::rma.uni(
  yi = yi_adj,
  vi = vi_adj,
  data = dat_final_scenario1,
  slab = paste(dat_final_scenario1$author, dat_final_scenario1$year)
)

adj_effect_scenario1 <-
  estimate(
    exp(model_adj_scenario1$b),
    exp(model_adj_scenario1$ci.lb),
    exp(model_adj_scenario1$ci.ub),
    type = ""
  )

adj_tau2 <- comma(model_adj_scenario1$tau2)
adj_I2 <- comma(model_adj_scenario1$I2)

metafor::forest(model_adj_scenario1,
                annotate = T,
                showweights = TRUE)

# Run bias-/indirectness-adjusted model using Scenario 2
model_adj_scenario2 <- metafor::rma(
  yi = yi_adj,
  vi = vi_adj,
  data = dat_final_scenario2,
  slab = paste(dat_final_scenario2$author, dat_final_scenario2$year),
  control=list(stepadj=0.5,maxiter=100)
)

adj_effect_scenario2 <-
  estimate(
    exp(model_adj_scenario2$b),
    exp(model_adj_scenario2$ci.lb),
    exp(model_adj_scenario2$ci.ub),
    type = ""
  )

adj_tau2 <- model_adj_scenario2$tau2
adj_I2 <- model_adj_scenario2$I2

metafor::forest(model_adj_scenario2,
                annotate = T,
                showweights = TRUE)

# Build paired forest plot

png("figures/tri/fp_paired_midlife_ldl_ad.png", height = 400, width = 800)

par(mfrow=c(1,2))
par(mar=c(5,0,1,0))

metafor::forest(
  model_unadj,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  showweights = TRUE
)
text(log(0.01), 25, "Author and Year", cex=.8, font=2)
text(log(40), 25.5, "Unadjusted", cex=.8, font=2)
text(log(9.3), 24.5, "Weight", cex=.8, font=2)
text(log(100), 24.5, "Estimate", cex=.8, font=2)

par(mar=c(5,0,1,0))
metafor::forest(
  model_adj_scenario1,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  slab = rep("", length(dat_final_scenario1$yi)),
  showweights = TRUE
)
text(log(40), 25.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 24.5, "Weight", cex=.8, font=2)
text(log(100), 24.5, "Estimate", cex=.8, font=2)

dev.off()

# Scenario comparison
png("figures/tri/fp_paired_midlife_ldl_ad_scenarios.png", height = 400, width = 800)

par(mfrow=c(1,2))
par(mar=c(5,0,1,0))

par(mar=c(5,0,1,0))
metafor::forest(
  model_adj_scenario1,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  showweights = TRUE
)
text(log(0.01), 25, "Author and Year", cex=.8, font=2)
text(log(40), 25.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 24.5, "Weight", cex=.8, font=2)
text(log(100), 24.5, "Estimate", cex=.8, font=2)

par(mar=c(5,0,1,0))
metafor::forest(
  model_adj_scenario2,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  slab = rep("", length(dat_final_scenario2$yi)),
  showweights = TRUE
)
text(log(40), 25.5, "Adjusted (Scenario 2)", cex=.8, font=2)
text(log(9.3), 24.5, "Weight", cex=.8, font=2)
text(log(100), 24.5, "Estimate", cex=.8, font=2)

dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- tgVadBIAMA ----
# 
vad_set <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                       which = 2) %>% 
  janitor::clean_names() %>%
  filter(exposure == "TG",
         outcome == "VaD",
         !(study_id %in% c(10312,13401))) %>%
  select(result_id, point_estimate, upper_ci,lower_ci) %>%
  tibble::add_row(result_id = "999-1",point_estimate = "1.25",lower_ci="1.02",upper_ci="1.57") %>%
  tibble::add_row(result_id = "999-2",point_estimate = "1.49",lower_ci="1.27",upper_ci="1.74") %>%
  tibble::add_row(result_id = "999-3",point_estimate = "1.29",lower_ci="0.83",upper_ci="2.02") %>%
  mutate(across(c(
    point_estimate, starts_with(c("number_", "cases_")), ends_with("_ci")
  ), as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci)) / 3.92)

# Read in general data
dat_gen <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
which = 2) %>% 
  janitor::clean_names() %>%
  mutate(year = ifelse(is.na(age), year, paste(year,"-",age))) %>%
  select(result_id, author, year) %>%
  
  mutate(year = as.character(year)) %>%
  tibble::add_row(result_id = "999-1", author = "Chp 6 - IPD (CaPS)", year = "") %>%
  tibble::add_row(result_id = "999-2", author = "Chp 6 - IPD (Whitehall II)", year = "") %>%
  tibble::add_row(result_id = "999-3", author = "Chp 5 - CPRD", year = "")

# Read in result and risk of bias data
dat_rob_vad <- read.csv("data/tri/tg_vad_rob.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(grepl("NRSE",type), yi*-1, yi),
         d7d = case_when(type %in% c("NRSE","MR - LDL") & d7d == "Right" ~ "Left",
                         type %in% c("NRSE","MR - LDL") & d7d == "Left" ~ "Right",
                         T ~ d7d))

dat_ind_vad <- read.csv("data/tri/tg_vad_indirect.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(type %in% c("NRSE","MR"), yi*-1, yi))

tg_vad_citations <- get_citations_per_analysis(dat_rob_vad)

png(
  here::here("figures/tri/midlife_VaD.png"),
  width = 1750,
  height = 1000,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat_rob_vad,
                     sei = sei,
                     at = log(c(.3,1,3)),
                     xlab = "Favours experimental | Favours comparator",
                     cex.lab	 = 0.9,
                     legend_cex = 0.9,
                     type_levels = c("NRSI - Fibrates","NRSE - Hypertriglyceridemia","NRSE - Triglycerides (SD)"),
                     title = "Triglycerides and VaD")

dev.off()


levels_type <- c("NRSI - Fibrates","NRSE - Hypertriglyceridemia","NRSE - Triglycerides (SD)")

dat_rob_vad <- dat_rob_vad %>%
  mutate(type = factor(type, levels = levels_type)) %>%
  mutate(overall = factor(overall, levels =levels)) %>%
  arrange(desc(type), desc(overall), author)

#Prep data and run meta-analyses
dat_final_scenario1_vad <-
  prep_tri_data(dat_rob_vad, dat_ind_vad, bias_values_scenario1, indirect_values)

dat_final_scenario2_vad <-
  prep_tri_data(dat_rob_vad, dat_ind_vad, bias_values_scenario2, indirect_values)

# Run unadjusted model and extract results
model_unadj_vad <- metafor::rma.uni(
  yi = yi,
  vi = vi,
  data = dat_final_scenario1_vad,
  slab = paste(dat_final_scenario1_vad$author, dat_final_scenario1_vad$year)
)

unadj_effect_vad <-
  estimate(exp(model_unadj_vad$b),
           exp(model_unadj_vad$ci.lb),
           exp(model_unadj_vad$ci.ub),
           type = "")

unadj_vad_tau2 <- comma(model_unadj_vad$tau2)
unadj_vad_I2 <- comma(model_unadj_vad$I2)

metafor::forest(model_unadj_vad, annotate = T, showweights = TRUE)

# Run bias-/indirectness-adjusted model using Scenario 1
model_adj_scenario1_vad <- metafor::rma.uni(
  yi = yi_adj,
  vi = vi_adj,
  data = dat_final_scenario1_vad,
  slab = paste(dat_final_scenario1_vad$author, dat_final_scenario1_vad$year)
)

adj_effect_scenario1_vad <-
  estimate(
    exp(model_adj_scenario1_vad$b),
    exp(model_adj_scenario1_vad$ci.lb),
    exp(model_adj_scenario1_vad$ci.ub),
    type = ""
  )

adj_vad_tau2 <- comma(model_adj_scenario1_vad$tau2)
adj_vad_I2 <- comma(model_adj_scenario1_vad$I2)

metafor::forest(model_adj_scenario1_vad,
                annotate = T,
                showweights = TRUE)

model_adj_scenario2_vad <- metafor::rma.uni(
  yi = yi_adj,
  vi = vi_adj,
  data = dat_final_scenario2_vad,
  slab = paste(dat_final_scenario2_vad$author, dat_final_scenario2_vad$year)
)

adj_effect_scenario2_vad <-
  estimate(
    exp(model_adj_scenario2_vad$b),
    exp(model_adj_scenario2_vad$ci.lb),
    exp(model_adj_scenario2_vad$ci.ub),
    type = ""
  )

# Build paired forest plot

png("figures/tri/fp_paired_midlife_tg_vad.png", height = 400, width = 800)

par(mfrow=c(1,2))
par(mar=c(5,0,1,0))

metafor::forest(
  model_unadj_vad,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  showweights = TRUE
)
text(log(0.01), 9, "Author and Year", cex=.8, font=2)
text(log(40), 9.5, "Unadjusted", cex=.8, font=2)
text(log(9.3), 8.5, "Weight", cex=.8, font=2)
text(log(100), 8.5, "Estimate", cex=.8, font=2)

par(mar=c(5,0,1,0))
metafor::forest(
  model_adj_scenario1_vad,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  slab = rep("", length(dat_final_scenario1_vad$yi)),
  showweights = TRUE
)
text(log(40), 9.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 8.5, "Weight", cex=.8, font=2)
text(log(100), 8.5, "Estimate", cex=.8, font=2)

dev.off()


png("figures/tri/fp_paired_midlife_tg_vad_scenarios.png", height = 400, width = 800)

par(mfrow=c(1,2))
par(mar=c(5,0,1,0))

metafor::forest(
  model_adj_scenario1_vad,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  showweights = TRUE
)
text(log(0.01), 9, "Author and Year", cex=.8, font=2)
text(log(40), 9.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 8.5, "Weight", cex=.8, font=2)
text(log(100), 8.5, "Estimate", cex=.8, font=2)

par(mar=c(5,0,1,0))
metafor::forest(
  model_adj_scenario2_vad,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  slab = rep("", length(dat_final_scenario1_vad$yi)),
  showweights = TRUE
)
text(log(40), 9.5, "Adjusted (Scenario 2)", cex=.8, font=2)
text(log(9.3), 8.5, "Weight", cex=.8, font=2)
text(log(100), 8.5, "Estimate", cex=.8, font=2)

dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- turnerValidation ----

# Validate additive biases using Turner data for single assessor
bias_values <-
  read.csv("data/appendix/turner_validation_bias_values.csv") %>%
  rename_with( ~ paste0("bias_", .), matches("_"))

dat_rob <- rio::import("data/appendix/turner_validation_rob.csv")

dat_rob_long <- dat_rob %>%
  append_values_bias(bias_values, common = F)

i_add <- dat_rob_long %>%
  group_by(result_id) %>%
  summarise(addimn = sum(bias_m_add),
            addivar = sum(bias_v_add)) %>%
  select(result_id, starts_with("add"))


# Validate BIAMA formula using all Turner data
turner <- rio::import("turner_bias/propbias.dta") %>%
  arrange(study, assessor) %>%
  left_join(rio::import("turner_bias/addbias.dta")) %>%
  filter(assessor == 1) %>%
  select(-assessor) %>%
  rename("yi" = estlogor,
         "vi" = varlogor) %>%
  calculate_adjusted_estimates()  

model <- metafor::rma.uni(yi = yi_adj,
                          vi = vi_adj,
                          data = turner)

metafor::forest(model, showweights = TRUE)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- exampleDirectionSetup ----

try(dev.off())

png("figures/tri/exampleDirection.png", width = 600, height = 300)

forest.default(
  x = c(-0.5, 0.5,-0.5, 0.5),
  vi = c(0.01, 0.01,0.01, 0.01),
  xlim = c(-2.2, 1.8),
  annotate = T,
  rows = c(5,4,2,1),
  slab = rep("",4),
  xlab = "Favours experimental | Favours comparator",
  header = c("Bias"),
  top = 2.5
)

graphics::text(-2.2, 4.5, "Additive - Favours comparator",pos=4)
graphics::text(-2.2, 1.5, "Proportional - Towards null",pos=4)

graphics::text(-.15, 5, "\u2014", cex = 1.8, col = "red")
graphics::text(-0.1, 5, "\u2014", cex = 1.8, col = "red")
graphics::text(0, 5, "\U2192", cex = 2, col = "red")
graphics::text(.85, 4, "\U2192", cex = 2, col = "red")
graphics::text(-.15, 2, "\U2192", cex = 2, col = "red")
graphics::text(.15, 1, "\U2190", cex = 2, col = "red")

dev.off()
