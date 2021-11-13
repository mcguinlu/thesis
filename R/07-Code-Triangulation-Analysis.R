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
    align = "lcc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    row_spec(2:nrow(priors_add_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- biasDirectionPlots

source("R/forest tri.R")

dat <- read.csv("turner_bias/real_example_rob.csv",stringsAsFactors = F) %>%
  left_join(
    rio::import(
      here::here("data/sys-rev/data_extraction_main.xlsx"),
      which = 2
    ) %>% janitor::clean_names() %>%
      select(result_id, author, year)
  ) %>%
  # Get sampling variance (square of standard error)
  mutate(vi = sei^2)

dat <- dat %>%
  select(result_id, author, type, yi, vi, everything())

png(
  here::here("figures/tri/midlife_AD.png"),
  width = 1750,
  height = 1200,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat, sei = dat$sei, title = "LDL-c and AD")

dev.off()


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
