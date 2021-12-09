#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchOverviewAppendix-table

searches <-
  # Read in all sheets
  read_excel_allsheets(here::here("data/sys-rev/searchResults_19_06_03.xlsx")) %>%
  
  # Filter columns and clean names
  lapply(
    function(x)
      x %>% select(1:3) %>% rename(
        "#" = x1,
        "Search term" = x2,
        "Hits" = x3
      )
  )

searchOverviewAppendix_table <- tail(searches$Summary,-2) %>%
  select("#","Search term") %>%
  rename("Step"="#",
         Hits = "Search term")

if(doc_type == "docx") {
  apply_flextable(searchOverviewAppendix_table, caption = "(ref:searchOverviewAppendix-caption)")
} else{
  knitr::kable(
    searchOverviewAppendix_table,
    format = "latex",
    caption = "(ref:searchOverviewAppendix-caption)",
    caption.short = "(ref:searchOverviewAppendix-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    linesep = "",
    align = "lc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(c(6,nrow(searchOverviewAppendix_table)), bold = TRUE) %>%
    row_spec(c(5,nrow(searchOverviewAppendix_table) - 1), hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = FALSE, width = paste0(15, "em")) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsMedline-table

searchHitsMedline_table <- searches$Medline

if (doc_type == "docx") {
  apply_flextable(searchHitsMedline_table, caption = "(ref:searchHitsMedline-caption)")
} else{
  knitr::kable(
    searchHitsMedline_table,
    format = "latex",
    caption = "(ref:searchHitsMedline-caption)",
    caption.short = "(ref:searchHitsMedline-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2, "em")) %>%
    column_spec(column = 2, width = paste0(36, "em")) %>%
    column_spec(column = 3, width = paste0(4, "em")) %>%
    kable_styling(
      latex_options = c("HOLD_position", "repeat_header"),
      font_size = 9
    )
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsEmbase-table

searchHitsEmbase_table <- searches$EMBASE

if (doc_type == "docx") {
  apply_flextable(searchHitsEmbase_table, caption = "(ref:searchHitsEmbase-caption)")
} else{
  knitr::kable(
    searchHitsEmbase_table,
    format = "latex",
    caption = "(ref:searchHitsEmbase-caption)",
    caption.short = "(ref:searchHitsEmbase-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsPI-table

searchHitsPI_table <- searches$PyscINFO

if (doc_type == "docx") {
  apply_flextable(searchHitsPI_table, caption = "(ref:searchHitsPI-caption)")
} else{
  knitr::kable(
    searchHitsPI_table,
    format = "latex",
    caption = "(ref:searchHitsPI-caption)",
    caption.short = "(ref:searchHitsPI-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsCentral-table

searchHitsCentral_table <- searches$CENTRAL

if (doc_type == "docx") {
  apply_flextable(searchHitsCentral_table, caption = "(ref:searchHitsCentral-caption)")
} else{
  knitr::kable(
    searchHitsCentral_table,
    format = "latex",
    caption = "(ref:searchHitsCentral-caption)",
    caption.short = "(ref:searchHitsCentral-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchHitsWos-table

searchHitsWos_table <- searches$WoS

if (doc_type == "docx") {
  apply_flextable(searchHitsWos_table, caption = "(ref:searchHitsWos-caption)")
} else{
  knitr::kable(
    searchHitsWos_table,
    format = "latex",
    caption = "(ref:searchHitsWos-caption)",
    caption.short = "(ref:searchHitsWos-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    align = "lll",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    # row_spec(c(18,36,73:76,78,84,96,97,100,107,108,118:123), bold= T, italic = T) %>%
    column_spec(column = 1, width = paste0(2,"em")) %>%
    column_spec(column = 2, width = paste0(36,"em")) %>%
    column_spec(column = 3, width = paste0(4,"em")) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"),
                  font_size = 9)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- wosDatabases-table

wosDatabases_table <- read.csv(here::here("data/sys-rev/wosDatabases.csv"))

if(doc_type == "docx") {
  knitr::kable(wosDatabases_table, caption = "(ref:wosDatabases-caption)")
} else{
  knitr::kable(
    wosDatabases_table,
    format = "latex",
    caption = "(ref:wosDatabases-caption)",
    caption.short = "(ref:wosDatabases-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(column = 1, width = paste0(18,"em")) %>%
    column_spec(column = 2, width = paste0(8,"em")) %>%
    column_spec(column = 3, width = paste0(6,"em")) %>%
    row_spec(2:nrow(wosDatabases_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- medRes-table

medRes_table <- rio::import("data/appendix/med_res.csv") %>%
  rename(Status = "pub_ind") %>%
  mutate(Percent = paste0(Percent,"%"))

if(doc_type == "docx") {
  apply_flextable(medRes_table, caption = "(ref:medRes-caption)")
} else{
  knitr::kable(
    medRes_table,
    format = "latex",
    caption = "(ref:medRes-caption)",
    caption.short = "(ref:medRes-scaption)",
    booktabs = TRUE,
    align = "lcc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- biasData-table

biasData_table <- robvis::data_rob2 %>%
  head(5)

if (doc_type == "docx") {
  apply_flextable(biasData_table, caption = "(ref:biasData-caption)")
} else{
  knitr::kable(
    biasData_table,
    format = "latex",
    caption = "(ref:biasData-caption)",
    caption.short = "(ref:biasData-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 10)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- biasDirectionData-table

biasDirectionData_table <- rio::import("data/tri/ldl_ad_rob.csv") %>%
  mutate(yi = round(yi,2),
         sei = round(sei,2)) %>%
  head(5) %>%
  mutate(Study = paste("Study ",1:5)) %>%
  select(Study, everything(),-c(result_id,type, starts_with("d6"),starts_with("d7")))

biasDirectionData_table[biasDirectionData_table=="Add"] <- "A"
biasDirectionData_table[biasDirectionData_table=="Prop"] <- "P"
biasDirectionData_table[biasDirectionData_table=="None"] <- NA

if(doc_type == "docx") {
  apply_flextable(biasDirectionData_table, caption = "(ref:biasDirectionData-caption)")
} else{
  knitr::kable(
    biasDirectionData_table,
    format = "latex",
    caption = "(ref:biasDirectionData-caption)",
    caption.short = "(ref:biasDirectionData-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"), 
                  font_size = 10)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- turnerEstimates-table

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
  summarise(value = mean(abs(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give the impact of any single domain
  # in any study + maximum across domains and studies to give max impact of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value),
            type = "Bias (Additive)")

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
  # On the log scale, so no adjustment = 0
  mutate(value = ifelse(is.na(value),0,value)) %>%
  # Get mean of assessors for each estimate in each domain in each study
  # Take absolute, as direction is encoded in these values, whereas for us it
  # will be defined separately, so just after the total amount it was adjusted
  # by in any direction
  group_by(study, estimate, domain) %>%
  summarise(value = mean(abs(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give av impact of any single domain
  # in any study + maximum across domains and studies to give max imapct of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value),
            type = "Bias (Proportional)")

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
  summarise(value = mean(abs(value))) %>%
  ungroup() %>%
  # Get mean across domains and studies, to give av impact of any single domain
  # in any study + maximum across domains and studies to give max imapct of
  # single domain in any study
  group_by(estimate) %>%
  summarise(mean = mean(value),
            max = max(value),
            type = "Indirectness (Proportional)")

turnerEstimates_table <- rbind(add, prop_e, prop_i) %>%
  tidyr::pivot_wider(names_from = estimate, values_from = c(mean, max)) %>%
  arrange(type) %>%
  mutate(across(-type,~round(.,2))) %>%
  mutate(Mean = paste0("N(",mean_est,", ",mean_var,")")) %>%
  mutate(Max = paste0("N(",max_est,", ",max_var,")")) %>%
  select(type, Mean, Max) %>%
  rename(" "=type)

if(doc_type == "docx") {
  apply_flextable(turnerEstimates_table, caption = "(ref:turnerEstimates-caption)")
} else{
  knitr::kable(
    turnerEstimates_table,
    format = "latex",
    caption = "(ref:turnerEstimates-caption)",
    caption.short = "(ref:turnerEstimates-scaption)",
    booktabs = TRUE, 
    align = "lcc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1,bold=TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ipdDementiaDef-table

ipdDementiaDef_table <- rio::import("data/appendix/ipd_dementia_definitions.csv")

if(doc_type == "docx") {
  apply_flextable(ipdDementiaDef_table, caption = "(ref:ipdDementiaDef-caption)")
} else{
  knitr::kable(
    ipdDementiaDef_table,
    format = "latex",
    caption = "(ref:ipdDementiaDef-caption)",
    caption.short = "(ref:ipdDementiaDef-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))  %>%
    column_spec(column = 1, width = paste0(3,"em")) %>%
    column_spec(column = 2:6, width = paste0(5.8,"em"))
}
