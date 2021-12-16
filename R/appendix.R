#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- searchOverviewAppendix-table ----

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
# ---- searchHitsMedline-table ----

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
# ---- searchHitsEmbase-table ----

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
# ---- searchHitsPI-table ----

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
# ---- searchHitsCentral-table ----

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
# ---- searchHitsWos-table ----

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
# ---- wosDatabases-table ----

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
# ---- medRes-table ----

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
# ---- mrTool-table ----

mrTool_table <- read.csv(here::here("data/sys-rev/mrRobTool.csv")) %>%
  select(1:5) %>%
  rename("Bias domain" = Bias.domain)

if(doc_type == "docx") {
  knitr::kable(mrTool_table, caption = "(ref:mrTool-caption)")
} else{
  knitr::kable(
    mrTool_table,
    format = "latex",
    caption = "(ref:mrTool-caption)",
    caption.short = "(ref:mrTool-scaption)",
    longtable = TRUE,
    booktabs = TRUE,
    linesep=""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(column = 1:5, width = paste0(50/5,"em")) %>%
    row_spec(2:nrow(mrTool_table)-1, hline_after = TRUE) %>%
    add_header_above(
      c(" "," ", "Risk of bias judgement" = 3),bold = TRUE
    ) %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- biasData-table ----

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
                  font_size = 8)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- biasDirectionData-table ----

biasDirectionData_table <- rio::import("data/tri/ldl_ad_rob.csv") %>%
  mutate(yi = round(yi,2),
         sei = round(sei,2)) %>%
  head(5) %>%
  mutate(ID = 1:5) %>%
  select(ID, everything(),-c(yi,sei,result_id,type))

biasDirectionData_table[biasDirectionData_table=="Add"] <- "A"
biasDirectionData_table[biasDirectionData_table=="Prop"] <- "P"
biasDirectionData_table[biasDirectionData_table=="Unpredictable"] <- "U"
biasDirectionData_table[biasDirectionData_table=="Right"] <- "R"
biasDirectionData_table[biasDirectionData_table=="Left"] <- "L"
biasDirectionData_table[biasDirectionData_table=="None"] <- "-"
biasDirectionData_table[biasDirectionData_table == "Serious"] <- "High"
biasDirectionData_table[biasDirectionData_table=="Moderate"] <- "M"
biasDirectionData_table[biasDirectionData_table=="High"] <- "H"
biasDirectionData_table[biasDirectionData_table=="Low"] <- "L"

if(doc_type == "docx") {
  apply_flextable(biasDirectionData_table, caption = "(ref:biasDirectionData-caption)")
} else{
  knitr::kable(
    biasDirectionData_table,
    format = "latex",
    caption = "(ref:biasDirectionData-caption)",
    caption.short = "(ref:biasDirectionData-scaption)",
    booktabs = TRUE,
    align = "lcccccccccccccccccccccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(c(1, 4, 7, 10, 13, 16, 19, 22), border_right = T) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 8) %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        "\\\\textbf{Abbreviations:}",
        "H - High risk of bias;",
        "M - Moderate risk of bias;",
        "L - Low risk of bias;",
        "R - Right;",
        "L - Left;",
        "U - Unpredictable;",
        "A - Additive;",
        "P - Proportional"
      ),
      escape = F
    )
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- turnerEstimates-table ----

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
# ---- missingMatrix-table ----

missingMatrix_table <- rio::import("data/appendix/missing_matrix.csv") %>%
  mutate(across(2:4,~stringr::str_replace(.,"\\)","%\\)")),
         across(2:4,~stringr::str_replace(.,"C","Complete")),
         across(2:4,~stringr::str_replace(.,"-","X")))

if(doc_type == "docx") {
  apply_flextable(missingMatrix_table, caption = "(ref:missingMatrix-caption)")
} else{
  knitr::kable(
    missingMatrix_table,
    format = "latex",
    caption = "(ref:missingMatrix-caption)",
    caption.short = "(ref:missingMatrix-scaption)",
    booktabs = TRUE, 
    align = "lccc",
    linesep = ""
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))  %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        "Values shown are N (Percentage) unless otherwise stated.\\\\newline",
        "\\\\textbf{Key:}",
        "Complete - No missing values;",
        "X - Variable not recorded/available in this cohort;",
        "* - LDL values derived from total cholesterol, HDL and triglycerides.\\\\newline",
        "\\\\textbf{Abbreviations:}",
        "BMI - Body mass index;",
        "IHD - Ischemic heart disease;",
        "HDL - Low-density lipoprotein cholesterol;",
        "LDL - Low-density lipoprotein cholesterol;",
        "TG - Triglycerides."
      ), 
      escape = F
    )
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ipdDementiaDef-table ----

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
    column_spec(column = 1, width = paste0(4,"em")) %>%
    column_spec(column = 2:6, width = paste0(6.9,"em")) %>%
    row_spec(2:nrow(ipdDementiaDef_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- addTable1-table ----

addTable1_table <- rio::import("turner_bias/addbias_full.dta") %>%
  # select est/variance for each internal bias for each assessor
  select(study,
         matches("estsel|estper|estatt|estdet"),
         matches("varsel|varper|varatt|vardet")) %>%
  tidyr::pivot_longer(!study,
                      names_to = c("estimate","domain","assessor"), 
                      names_pattern = "(.{3})(.{3})(.)",
                      values_to = c("value")) %>%
tidyr::pivot_wider(names_from = estimate, values_from = value) %>%
  mutate(across(c(est,var),~round(.,2))) %>%
  rename("Study" = study,
         "Domain" = domain,
         "Assessor" = assessor,
         "Position" = est,
         "Variance" = var) %>%
  slice(9:16) %>%
  mutate(Domain = ifelse(Domain == "att","Attrition bias","Detection bias")) 

if (doc_type == "docx") {
  apply_flextable(addTable1_table, caption = "(ref:addTable1-caption)")
} else{
  knitr::kable(
    addTable1_table,
    format = "latex",
    caption = "(ref:addTable1-caption)",
    caption.short = "(ref:addTable1-scaption)",
    booktabs = TRUE,
    align = "ccccc",
    linesep  = c("", "","", "\\addlinespace")
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- addTable2-table ----

addTable2_table <- addTable1_table %>%
  group_by(Study, Domain) %>%
  # Get mean of assessors for each estimate in each domain in each study
  # Take absolute, as direction is encoded in these values, whereas for us it
  # will be defined separately, so just after the total amount it was adjusted
  # by in any direction
  summarise(Position = mean(abs(Position)),
            Variance = mean(abs(Variance))) %>%
  ungroup() 

if(doc_type == "docx") {
  apply_flextable(addTable2_table, caption = "(ref:addTable2-caption)")
} else{
  knitr::kable(
    addTable2_table,
    format = "latex",
    caption = "(ref:addTable2-caption)",
    caption.short = "(ref:addTable2-scaption)",
    booktabs = TRUE,
    align = "cccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- addTable3-table ----

addTable3_table <- addTable2_table %>%
  # Get mean across domains and studies, to give the impact of any single domain
  # in any study + maximum across domains and studies to give max impact of
  # single domain in any study
  summarise('Mean position' = mean(Position),
            'Mean variance' = mean(Variance),
            'Max position' = max(Position),
            'Max variance' = max(Variance))

if(doc_type == "docx") {
  apply_flextable(addTable3_table, caption = "(ref:addTable3-caption)")
} else{
  knitr::kable(
    addTable3_table,
    format = "latex",
    caption = "(ref:addTable3-caption)",
    caption.short = "(ref:addTable3-scaption)",
    booktabs = TRUE,
    align = "ccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- minFollowUp ----

library(ggplot2)
library(patchwork)

df <- data.frame(
  type = c("Exposed","Exposed","Unexposed", "Unexposed"),
  length = c(3,1.5,3,0.8),
  group = c("Excluded time","Captured time", "Excluded time","Captured time")
)

plot1 <-
  ggplot(df, aes(
    y = type,
    x = length,
    fill = group,
    alpha = group
  )) +
  geom_bar(stat = "identity",
           width = .1,
           colour = "black") +
  scale_fill_manual(values = c("black", "white")) +
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_x_continuous(expand = c(0, .2)) +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    axis.text.y.left = element_text(color = "black"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank()
  ) +
  geom_text(
    data = data.frame(
      x = c(0.060048391976198, 0.060048391976198),
      y = c(0.803071359423899, 1.8),
      label = c("Index event\n(cohort entry)", "Index event\n(cohort entry)")
    ),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    angle = 0L,
    lineheight = 1L,
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    colour = "black",
    family = "sans",
    fontface = "plain",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(
      x = c(3, 3),
      y = c(0.803071359423899, 1.8),
      label = "Start of follow-up\n(10 year lag)"
    ),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    angle = 0L,
    lineheight = 1L,
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    colour = "black",
    family = "sans",
    fontface = "plain",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
geom_text(
  data = data.frame(
    x = c(4.5,3.8),
    y = c(0.803071359423899, 1.8),
    label = "Outcome"
  ),
  mapping = aes(x = x,
                y = y,
                label = label),
  angle = 0L,
  lineheight = 1L,
  hjust = 0.5,
  vjust = 0.5,
  size = 3,
  colour = "black",
  family = "sans",
  fontface = "plain",
  inherit.aes = FALSE,
  show.legend = FALSE
)

df2 <- data.frame(
  type = factor(c("Participant B", "Participant B", "Participant A"), levels = c("Participant B", "Participant A")),
  length = c(1, 2, 2),
  group = c("Unexposed", "Exposed", "Unexposed")
)

plot2 <-
  ggplot(df2, aes(
    y = type,
    x = length,
    fill = group,
    alpha = group
  )) +
  geom_bar(stat = "identity",
           width = .1,
           colour = "black") +
  scale_fill_manual(values = c("black", "white", "grey50")) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_x_continuous(expand = c(0, .2)) +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    axis.text.y.left = element_text(color = "black"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank()
  ) +
  geom_text(
    data = data.frame(
      x = c(0.060048391976198, 0.060048391976198),
      y = c(0.803071359423899, 1.8),
      label = "Index event\n(cohort entry)"
    ),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    angle = 0L,
    lineheight = 1L,
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    colour = "black",
    family = "sans",
    fontface = "plain",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(x = 1,
                      y = 0.803071359423899,
                      label = "Statin initiation"),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    angle = 0L,
    lineheight = 1L,
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    colour = "black",
    family = "sans",
    fontface = "plain",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(
      x = c(3, 2),
      y = c(0.803071359423899, 1.8),
      label = "Outcome"
    ),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    angle = 0L,
    lineheight = 1L,
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    colour = "black",
    family = "sans",
    fontface = "plain",
    inherit.aes = FALSE,
    show.legend = FALSE
  )

plot <- plot1 / plot2 +
  plot_annotation(tag_levels = c("A", "B"))


ggsave(
  plot,
  filename =  here::here("figures/appendix/minFU.png"),
  width = 21,
  height = 14,
  units = "cm"
)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- prismaTab-table ----

prismaTab_table <- rio::import(here::here("data/appendix/prisma.csv"))
colnames(prismaTab_table)[4] <- "Location"
rownames(prismaTab_table) <- NULL
prismaTab_table[is.na(prismaTab_table)] <- ""

if(doc_type == "docx"){
apply_flextable(prismaTab_table,caption = "(ref:prismaTab-caption)")
}else{

  prismaTab_table %>%
    knitr::kable(
      format = "latex",
      caption = "(ref:prismaTab-caption)",
      caption.short = "(ref:prismaTab-scaption)",
      booktabs = TRUE,
      longtable = TRUE,
      linesep = "\\addlinespace",
      align = "lcll",
      col.names = c(
        "Topic",
        "No.",
        "Item",
        "Location"
      )
    ) %>%
    row_spec(-1, hline_after = FALSE) %>%
    row_spec(
      0,
      bold = TRUE,
      background = "#63639A",
      color = "white",
      extra_latex_after = "\\arrayrulecolor{white}"
    ) %>%
    row_spec(2:nrow(prismaTab_table) - 1, hline_after = TRUE) %>%
    row_spec(c(1, 3, 5, 8, 26, 38, 43), background = "#FFFFCC", bold = TRUE) %>%
    column_spec(1, width = "8.45em") %>%
    column_spec(2, width = "2.1em") %>%
    column_spec(3, width = "13.5em") %>%
    column_spec(4, width = "7.45em") %>%
    kable_styling(latex_options = c("HOLD_position","repeat_header"))
  
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- funnelPlots

img <- list()
img$hyperDem <- readPNG(here::here("figures/sys-rev/funnel_hyper_dem.png"))
img$statinsDem <- readPNG(here::here("figures/sys-rev/funnel_statins_dem.png"))
img$hyperAD <- readPNG(here::here("figures/sys-rev/funnel_hyper_ad.png"))
img$statinsAD <- readPNG(here::here("figures/sys-rev/funnel_statins_ad.png"))

# setup plot
try(dev.off())
pdf(
  here::here("figures/appendix/funnel_composite.pdf"),
  width = 12,
  height = 12
)
par(mai = rep(0, 4)) # no margins

# layout the plots into a matrix w/ 12 columns, by row
par(mfrow = c(2, 2))

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


