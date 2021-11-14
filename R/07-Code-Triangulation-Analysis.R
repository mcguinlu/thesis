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
    align = "lccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kableExtra::add_header_above(c(" " = 1, "Additive bias" = 2," " = 1),bold = T) %>%
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

prop <- rio::import("turner_bias/propbias_full.dta") %>%
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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ldlAdBIAMA 

# read in values from spreadsheet

bias_values <- rio::import("turner_bias/bias_values.csv") %>%
  rename_with(~paste0("bias_",.), matches("_"))

indirect_values <- rio::import("turner_bias/indirectness_values.csv")  %>%
  rename_with(~paste0("ind_",.), matches("_"))

# Load general data
dat_gen <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                       which = 2) %>% janitor::clean_names() %>%
  select(result_id, author, year)

# Read in result and risk of bias data
dat_rob <- read.csv("turner_bias/real_example_rob.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything()) 

dat_ind <- read.csv("turner_bias/real_example_indirect.csv",
                    stringsAsFactors = F) %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything()) 

# Save bias direction plot

png(
  here::here("figures/tri/midlife_AD.png"),
  width = 1750,
  height = 1200,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat_rob, sei = dat$sei, title = "LDL-c and AD")

dev.off()

# Prepare data for bias-/indirectness-adjusted meta-analysis

dat_final <- prep_tri_data(dat_rob, dat_ind, bias_values, indirect_values)

model_unadj <- metafor::rma.uni(yi = yi,
                                vi = vi,
                                data = dat_final, 
                                slab = paste(dat_final$author,dat_final$year))

unadj_effect <- estimate(exp(model_unadj$b),exp(model_unadj$ci.lb),exp(model_unadj$ci.ub),type = "")

unadj_tau2 <- model_unadj$tau2
unadj_I2 <- model_unadj$I2

metafor::forest(model_unadj, annotate = T, showweights = TRUE)

model_adj <- metafor::rma.uni(yi = yi_adj,
                              vi = vi_adj,
                              data = dat_final,
                              slab = paste(dat_final$author,dat_final$year))
adj_effect <- estimate(exp(model_adj$b),exp(model_adj$ci.lb),exp(model_adj$ci.ub),type = "")
adj_tau2 <- model_adj$tau2
adj_I2 <- model_adj$I2

metafor::forest(model_adj,annotate = T,  showweights = TRUE)

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
  model_adj,
  xlim = c(-6, 6),
  xlab = "Effect measure",
  atransf = exp,
  at = log(c(0.3, 1, 3)),
  mlab = " ",
  slab = rep("", length(dat_final$yi)),
  showweights = TRUE
)
text(log(40), 20.5, "Adjusted", cex=.8, font=2)
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
# ---- exampleDirection

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
