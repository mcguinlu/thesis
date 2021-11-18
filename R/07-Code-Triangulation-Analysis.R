#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- thesisOverview-table

thesisOverview_table <- read.csv("data/background/thesisOverview.csv") %>%
  mutate("Exposure/ Intervention" = Exposure.Intervention) %>%
  mutate("Research Question" = Research.Question) %>%
  mutate("Contibution to evidence synthesis framework" = Contribution) %>%
  select("Chapter","Research Question","Exposure/ Intervention","Outcome","Contibution to evidence synthesis framework") %T>%
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
    row_spec(2:nrow(thesisOverview_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
  
  table <- gsub("textbackslash\\{\\}newline","\\newline",table)
  
  table
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- robLevelsMapping-table

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
# ---- priorsAdd-table

priors_add_table <- rio::import("data/tri/priors_add.csv") %T>%
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
# ---- turnerEstimates

add <- rio::import("turner_bias/addbias_full.dta") %>%
# select est/variance for each internal bias for each assessor
select(study,
       matches("estsel|estper|estatt|estdet"),
       matches("varsel|varper|varatt|vardet")) %>%
  tidyr::pivot_longer(!study,
                      names_to = c("estimate","domain","assessor"), 
                      names_pattern = "(.{3})(.{3})(.)",
                      values_to = c("value")) %>%
  group_by(study, estimate, domain) %>%
  # Get mean of assessors for each estimate in each domain in each study
  # Take absolute, as direction is encoded in these values, whereas for us it
  # will be defined separately, so just after the total amount it was adjusted
  # by in any direction
  summarise(value = abs(mean(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give av impact of any single domain
  # in any study + maximum across domains and studies to give max imapct of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value))

prop_i <- rio::import("turner_bias/propbias_full.dta") %>%
  janitor::clean_names() %>%
  filter(external == 0) %>%
  select(study, assessor, result, logbiasmean, logbiasvar) %>%
  rename("domain" = result, 
         "est" = logbiasmean, 
         "var" = logbiasvar) %>%
  tidyr::pivot_longer(!c(study, assessor, domain),
                      names_to = "estimate", 
                      values_to = "value") %>%
  # On the log scale, so n adjusted = 0
  mutate(value = ifelse(is.na(value),0,value)) %>%
  # Get mean of assessors for each estimate in each domain in each study
  # Take absolute, as direction is encoded in these values, whereas for us it
  # will be defined separately, so just after the total amount it was adjusted
  # by in any direction
  group_by(study, estimate, domain) %>%
  summarise(value = abs(mean(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give av impact of any single domain
  # in any study + maximum across domains and studies to give max imapct of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value))


prop_e <- rio::import("turner_bias/propbias_full.dta") %>%
  janitor::clean_names() %>%
  filter(external == 1) %>%
  select(study, assessor, result, logbiasmean, logbiasvar) %>%
  rename("domain" = result, 
         "est" = logbiasmean, 
         "var" = logbiasvar) %>%
  tidyr::pivot_longer(!c(study, assessor, domain),
                      names_to = "estimate", 
                      values_to = "value") %>%
  # On the log scale, so n adjusted = 0
  mutate(value = ifelse(is.na(value),0,value)) %>%
  # Get mean of assessors for each estimate in each domain in each study
  # Take absolute, as direction is encoded in these values, whereas for us it
  # will be defined separately, so just after the total amount it was adjusted
  # by in any direction
  group_by(study, estimate, domain) %>%
  summarise(value = abs(mean(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give av impact of any single domain
  # in any study + maximum across domains and studies to give max imapct of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ldlAdBIAMA 

# read in values from spreadsheet

bias_values_scenario1 <- rio::import("turner_bias/bias_values_scenario1.csv") %>%
  rename_with(~paste0("bias_",.), matches("_"))

bias_values_scenario2 <- rio::import("turner_bias/bias_values_scenario2.csv") %>%
  rename_with(~paste0("bias_",.), matches("_"))

indirect_values <- rio::import("turner_bias/indirectness_values.csv")  %>%
  rename_with(~paste0("ind_",.), matches("_"))

# Load general data
dat_gen <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                       which = 2) %>% janitor::clean_names() %>%
  select(result_id, author, year) %>%
  mutate(year = as.character(year)) %>%
  tibble::add_row(result_id = "999-1", author = "Chp 5 - CPRD", year = "") %>%
  tibble::add_row(result_id = "999-2", author = "Chp 6 - IPD", year = "")

# Read in result and risk of bias data
dat_rob <- read.csv("turner_bias/real_example_rob.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(type %in% c("NRSE","MR"), yi*-1, yi))

dat_ind <- read.csv("turner_bias/real_example_indirect.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())  %>%
  # Standardise effect direction
  mutate(yi = ifelse(type %in% c("NRSE","MR"), yi*-1, yi))

# Save bias direction plot

dat_rob_single <- head(tail(dat_rob,2),1)

png(
  here::here("figures/tri/midlife_AD_single.png"),
  width = 1500,
  height = 600,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat_rob_single, title = "LDL-c and AD", at = log(c(0.3,1,3)), x_min = -5)

dev.off()

png(
  here::here("figures/tri/midlife_AD.png"),
  width = 1750,
  height = 1300,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat_rob, sei = dat$sei, title = "LDL-c and AD")

dev.off()

# Prepare data for bias-/indirectness-adjusted meta-analysis

dat_final_single <-
  prep_tri_data(dat_rob_single, dat_ind, bias_values_scenario1, indirect_values)

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

unadj_tau2 <- model_unadj$tau2
unadj_I2 <- model_unadj$I2

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

adj_tau2 <- model_adj_scenario1$tau2
adj_I2 <- model_adj_scenario1$I2

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
text(log(0.01), 20, "Author and Year", cex=.8, font=2)
text(log(40), 20.5, "Unadjusted", cex=.8, font=2)
text(log(9.3), 19.5, "Weight", cex=.8, font=2)
text(log(100), 19.5, "Estimate", cex=.8, font=2)

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
text(log(40), 20.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 19.5, "Weight", cex=.8, font=2)
text(log(100), 19.5, "Estimate", cex=.8, font=2)

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
text(log(40), 20.5, "Adjusted (Scenario 1)", cex=.8, font=2)
text(log(9.3), 19.5, "Weight", cex=.8, font=2)
text(log(100), 19.5, "Estimate", cex=.8, font=2)

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
text(log(40), 20.5, "Adjusted (Scenario 2)", cex=.8, font=2)
text(log(9.3), 19.5, "Weight", cex=.8, font=2)
text(log(100), 19.5, "Estimate", cex=.8, font=2)

dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- turnerValidation 

# Validate formula using Turner data
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
# ---- exampleDirectionSetup

try(dev.off())

png("figures/tri/exampleDirection.png", width = 600, height = 300)

forest.default(
  x = c(-0.5, 0.5,-0.5, 0.5),
  vi = c(0.01, 0.01,0.01, 0.01),
  xlim = c(-2.2, 1.8),
  annotate = T,
  slab = c(
    "Additive - Favours intervention",
    "Additive - Favours intervention",
    "Proportional - Towards null",
    "Proportional - Towards null"
  ),
  xlab = "Favours comparator | Favours intervention",
  header = c("Bias"),
  top = 2.5
)

graphics::text(-.15, 4, "\U2192", cex = 2, col = "red")
graphics::text(.85, 3, "\U2192", cex = 2, col = "red")
graphics::text(-.15, 2, "\U2192", cex = 2, col = "red")
graphics::text(.15, 1, "\U2190", cex = 2, col = "red")

dev.off()
