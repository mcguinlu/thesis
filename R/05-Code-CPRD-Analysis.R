#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- smeethComparison-table

smeethComparison_table <- mtcars[1:4, 1:3]

if (doc_type == "docx") {
  apply_flextable(smeethComparison_table, caption = "(ref:smeethComparison-caption)")
} else{
  knitr::kable(
    smeethComparison_table,
    format = "latex",
    caption = "(ref:smeethComparison-caption)",
    caption.short = "(ref:smeethComparison-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- load-files

attrition <-
  read.csv(here::here("data", "cprd", "cohort_attrition.csv"))

p1 <-
  read.csv(here::here("data", "cprd", "regression_results_p1.csv")) %>%
  filter(drug == "Any") %>%
  select(outcome, N_fail) %>%
  unique()

p1$outcome <-
  factor(
    p1$outcome,
    levels = c(
      "Any dementia",
      "Probable AD",
      "Possible AD",
      "Vascular dementia",
      "Other dementia"
    )
  )
p1 <- p1[order(p1$outcome), ]

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- readExample-table

readExample_table <-
  read.csv(here::here("data", "cprd", "read_code_example.csv")) %>%
  dplyr::mutate("Read code" = Read.code) %>%
  dplyr::select("Level", "Read code", "Term") %T>%
  write.csv(here::here("data/table_words/readExample.csv"))

if (doc_type == "docx") {
  apply_flextable(readExample_table, caption = "(ref:readExample-caption)")
} else{
  knitr::kable(
    readExample_table,
    format = "latex",
    caption = "(ref:readExample-caption)",
    caption.short = "(ref:readExample-scaption)",
    booktabs = TRUE,
    align = "cll"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cprdCharacteristics-setUp

table1 <-
  read.csv(here::here("data", "cprd", "table1.csv"), header = FALSE)
table1 <- data.table::transpose(table1)
colnames(table1) <- as.character(unlist(table1[1, ]))
table1 = table1[-1,]

colnames(table1)[1] <- " "

table1[1, 1] <- "Sample size (N)"
table1[2, 1] <- "Year of cohort entry \\newline (median)"
table1[3, 1] <- "Female"
table1[4, 1] <- "Age at cohort entry \\newline (median)"
table1[5, 1] <- "CAD"
table1[6, 1] <- "CBS"
table1[7, 1] <- "CVD"
table1[8, 1] <- "Charlson (ever > 0)"
table1[9, 1] <- "IMD-2010 (median)"
table1[10, 1] <- "Consultation rate (mean/SD)"
table1[11, 1] <- "Alcohol (ever)"
table1[12, 1] <- "Smoking (ever)"
table1[13, 1] <- "BMI (mean/SD)"
table1[14, 1] <- "PAD"
table1[15, 1] <- "Hypertension"
table1[16, 1] <- "Total cholesterol (mean/SD)"
table1[17, 1] <- "CKD"
table1[18, 1] <- "Type 1 Diabetes"

# Rename other columns not included in Table 1
table1[19, 1] <- "Stopped"
table1[20, 1] <- "Added"
table1[21, 1] <- "Switched"
table1[22, 1] <- "LDL cholesterol (mean/SD)"
table1[23, 1] <- "Type 2 Diabetes"
table1[24, 1] <- "Follow up \\newline(years; median)"
table1.copy <- table1

table1 <- table1[, c(1, 10, 7, 9, 2:6, 8)]

eze_sta_n <- table1[1,7]
nag_n <- table1[1,9]

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cprdCharacteristics-table

table1_disp <- table1[c(1:16, 22, 17:18, 23), c(1:6,8,10)]

#Quick check to ensure data quality
t <- table1_disp[1, 2:8]
t <- as.numeric(table1_disp[1, 2:8])

if (t[1] - sum(t[2:7], as.numeric(eze_sta_n), as.numeric(nag_n)) != 0) {
  stop("cprdCharacteristics-table: Sum of subgroups != Total sample size")
}

table1_disp[1,2:8] <- stringr::str_trim(comma(as.numeric(table1_disp[1,2:8])))

# Create display table using kable
cprdCharacteristics_table <- comma(table1_disp) %T>%
  write.csv(here::here("data/table_words/cprdCharacteristics.csv"))

if (doc_type == "docx") {
  apply_flextable(cprdCharacteristics_table, caption = "(ref:cprdCharacteristics-caption)") %>%
    flextable::add_footer_row(
      colwidths = 8,
      values = paste(
        paste0("Note: The 'Nicotinic acid groups' (n=", 
               nag_n,
               ") and 'Ezetimibe and Statins' (n=",
               eze_sta_n,
               ") subgroups are not shown, but are included in the whole sample column\n"),
        "Abbreviations:",
        "BMI - Body mass index;",
        "CAD - Coronary arterial disease;",
        "CBS - Coronary bypass surgery;",
        "CKD - Chronic kidney disease;",
        "CVD - Cardiovascular disease;",
        "IMD - Index of multiple deprivation;",
        "LRA - Lipid regulating agent;",
        "PAD - Peripheral arterial disease;",
        "SD - Standard deviation."
      )
    )
} else{
  table <- knitr::kable(
    cprdCharacteristics_table,
    format = "latex",
    caption = "(ref:cprdCharacteristics-caption)",
    caption.short = "(ref:cprdCharacteristics-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccccccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, width = paste0(15, "em"), bold = TRUE) %>%
    column_spec(2:8, width = paste0(7.7, "em")) %>%
    row_spec(2:nrow(cprdCharacteristics_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 7) %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        paste0("\\\\textit{Note:} The 'Nicotinic acid groups' (n=", 
               nag_n,
               ") and 'Ezetimibe and Statins' (n=",
               eze_sta_n,
               ") subgroups are not shown, but are included in the whole sample column\\\\newline"),
        "\\\\textbf{Abbreviations:}",
        "BMI - Body mass index;",
        "CAD - Coronary arterial disease;",
        "CBS - Coronary bypass surgery;",
        "CKD - Chronic kidney disease;",
        "CVD - Cardiovascular disease;",
        "IMD - Index of multiple deprivation;",
        "LRA - Lipid regulating agent;",
        "PAD - Peripheral arterial disease;",
        "SD - Standard deviation."
      ), escape = FALSE
    )
  
  table <- gsub("textbackslash\\{\\}newline", "\\newline", table)
  
  table
  
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cprdSSA-table

cprdSSA_table <- table1[19:21, -3] %T>%
  write.csv(here::here("data/table_words/cprdSSA.csv"))

if (doc_type == "docx") {
  apply_flextable(cprdSSA_table, caption = "(ref:cprdSSA-caption)")
} else{
  knitr::kable(
    cprdSSA_table,
    format = "latex",
    caption = "(ref:cprdSSA-caption)",
    caption.short = "(ref:cprdSSA-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccccccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, width = paste0(5, "em"), bold = TRUE) %>%
    column_spec(2:9, width = paste0(4.225, "em")) %>%
    row_spec(2:nrow(cprdSSA_table) - 1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 7) %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        "\\\\textbf{Definitions:}",
        "Stopped - last prescription of the primary drug class followed by at least six months of observation with no further prescriptions;",
        "Added - second drug class prescribed before the last prescription of the initial class;",
        "Switched - second drug class being prescribed after the last prescription of the initial class."
      ), escape = FALSE 
    )
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- missing-data

missingdata <-
  read.csv(here::here("data", "cprd", "missingdata.csv"), header = TRUE)

total <- missingdata[1, 1]
totalpercent <- round(total / as.numeric(missingdata[1, 1]) * 100, 1)
totaltext <- paste0(format(total, big.mark = ","), " participants")
total_ab_text <- format(round(total, -3), big.mark = ",")

missing <- missingdata[2, 1]
missingpercent <- round(missing / as.numeric(missingdata[1, 1]) * 100, 1)
missingtext <-
  paste0(format(missing, big.mark = ","),
         " participants (",
         missingpercent,
         "%)")

imd <- missingdata[3, 1]
imdpercent <- round(imd / as.numeric(missingdata[1, 1]) * 100, 1)
imdtext <-
  paste0(format(imd, big.mark = ","), " participants (", imdpercent, "%)")

alcohol <- missingdata[4, 1]
alcoholpercent <- round(alcohol / as.numeric(missingdata[1, 1]) * 100, 1)
alcoholtext <-
  paste0(format(alcohol, big.mark = ","),
         " participants (",
         alcoholpercent,
         "%)")

smoking <- missingdata[5, 1]
smokingpercent <- round(smoking / as.numeric(missingdata[1, 1]) * 100, 1)
smokingtext <-
  paste0(format(smoking, big.mark = ","),
         " participants (",
         smokingpercent,
         "%)")

bmi <- missingdata[6, 1]
bmipercent <- round(bmi / as.numeric(missingdata[1, 1]) * 100, 1)
bmitext <-
  paste0(format(bmi, big.mark = ","), " participants (", bmipercent, "%)")

chol <- missingdata[7, 1]
cholpercent <- round(chol / as.numeric(missingdata[1, 1]) * 100, 1)
choltext <-
  paste0(format(chol, big.mark = ","), " participants (", cholpercent, "%)")

ldl <- missingdata[8, 1]
ldlpercent <- round(ldl / as.numeric(missingdata[1, 1]) * 100, 1)
ldltext <-
  paste0(format(ldl, big.mark = ","), " participants (", ldlpercent, "%)")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- characteristics

characteristics <-
  read.csv(here::here("data", "cprd", "characteristics.csv"), header = TRUE)

# Median and IQR for follow-up
fu_text <- paste0(
  round(characteristics$c1[1], 1),
  " years (IQR: ",
  round(characteristics$c1[2], 1),
  "-",
  round(characteristics$c1[3], 1),
  " years)"
)

fu_ab_text <- paste0(round(characteristics$c1[1], 1),
                     " participant-years")

# Median and IQR for age at index
age_text <- paste0(
  characteristics$c1[4],
  " years (inter-quartile range (IQR): ",
  characteristics$c1[5],
  "-",
  characteristics$c1[6],
  " years)"
)

total_followup <-
  as.numeric(format(characteristics$c1[7], scientific = F))

# Percentage of users taking a statin
t1 <- read.csv(
  here::here("data", "cprd", "table1.csv"),
  header = FALSE,
  stringsAsFactors = FALSE
) %>%
  filter(!V1 %in% c("first_drug", "No LRA", "Whole Sample"))

t1$V2 <- as.numeric(t1$V2)

percentage.statins <-
  paste0(round((t1$V2[which(t1$V1 == "Statins")] / sum(t1$V2[which(t1$V1 != "None")]) *
                  100), 2), "%")

index_event <-
  rio::import(here::here("data/cprd/index_event.tsv"))[-12] %>%
  mutate(number = as.numeric(gsub(",", "", No.))) %>%
  mutate(
    group = case_when(
      index_event %in% c("hc_risk", "hc_cond") ~ "code for hypercholesterolemia",
      index_event %in% c("ldl_all", "tc_all") ~ "elevated cholesterol test result",
      index_event == "Total" ~ "total",
      
      TRUE ~ "prescription of LRA"
    )
  ) %>%
  group_by(group) %>%
  summarise(N = sum(number)) %>%
  filter(group != "total") %>%
  mutate(percentage = N / sum(N) * 100) %>%
  arrange(desc(N))


index_event_text <-
  paste0(
    index_event$group[1],
    ": ",
    comma(index_event$percentage[1]),
    "%, ",
    index_event$group[2],
    ": ",
    comma(index_event$percentage[2]),
    "%, ",
    index_event$group[3],
    ": ",
    comma(index_event$percentage[3]),
    "%"
  )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- azd-text

results_p1 <-
  read.csv(
    here::here("data", "cprd", "regression_results_p1.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  )

results_p2 <-
  read.csv(here::here("data", "cprd", "regression_results_p2.csv"),
           header =
             TRUE)

results <- results_p1 %>%
  rbind(results_p2) %>%
  filter(!drug %in% c("hc_eze_sta", "hc_nag")) %>%
  unique()

generate_forester_plot(results, 
                       here::here("figures/cprd-analysis/forester_p1.png"))

probad <- results %>%
  filter(outcome == "Probable AD" & drug == "Any")

probad_text <- paste0(
  "HR:",
  round(probad$HR, 2),
  ", 95%CI:",
  round(probad$ci_lower, 2),
  "-",
  round(probad$ci_upper, 2)
)

probad_fib <- results %>%
  filter(outcome == "Probable AD" & drug == "Fibrates")

probad_fib_text <- paste0(
  "HR:",
  round(probad_fib$HR, 2),
  ", 95%CI:",
  round(probad_fib$ci_lower, 2),
  "-",
  round(probad_fib$ci_upper, 2)
)

probad_sta <- results %>%
  filter(outcome == "Probable AD" & drug == "Statins")

probad_sta_text <- paste0(
  "HR:",
  round(probad_sta$HR, 2),
  ", 95%CI:",
  round(probad_sta$ci_lower, 2),
  "-",
  round(probad_sta$ci_upper, 2)
)

possad <- results %>%
  filter(outcome == "Possible AD" & drug == "Any")

possad_text <- paste0(
  "HR:",
  round(possad$HR, 2),
  ", 95%CI:",
  round(possad$ci_lower, 2),
  "-",
  round(possad$ci_upper, 2)
)

vasdem <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Any")

vasdem_text <- paste0(
  "HR:",
  round(vasdem$HR, 2),
  ", 95%CI:",
  round(vasdem$ci_lower, 2),
  "-",
  round(vasdem$ci_upper, 2)
)

vasdem_eze <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Ezetimibe")

vasdem_eze_text <- paste0(
  "HR:",
  round(vasdem_eze$HR, 2),
  ", 95%CI:",
  round(vasdem_eze$ci_lower, 2),
  "-",
  round(vasdem_eze$ci_upper, 2)
)

vasdem_fib <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Fibrates")

vasdem_fib_text <- paste0(
  "HR:",
  round(vasdem_fib$HR, 2),
  ", 95%CI:",
  round(vasdem_fib$ci_lower, 2),
  "-",
  round(vasdem_fib$ci_upper, 2)
)

vasdem_sta <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Statins")

vasdem_sta_text <- paste0(
  "HR:",
  round(vasdem_sta$HR, 2),
  ", 95%CI:",
  round(vasdem_sta$ci_lower, 2),
  "-",
  round(vasdem_sta$ci_upper, 2)
)

othdem <- results %>%
  filter(outcome == "Other dementia" & drug == "Any")

othdem_text <- paste0(
  "HR:",
  round(othdem$HR, 2),
  ", 95%CI:",
  round(othdem$ci_lower, 2),
  "-",
  round(othdem$ci_upper, 2)
)

othdem_eze <- results %>%
  filter(outcome == "Other dementia" & drug == "Ezetimibe")

othdem_eze_text <- paste0(
  "HR:",
  round(othdem_eze$HR, 2),
  ", 95%CI:",
  round(othdem_eze$ci_lower, 2),
  "-",
  round(othdem_eze$ci_upper, 2)
)

anydem <- results %>%
  filter(outcome == "Any dementia" & drug == "Any")

anydem_text <- paste0(
  "HR:",
  round(anydem$HR, 2),
  ", 95%CI:",
  round(anydem$ci_lower, 2),
  "-",
  round(anydem$ci_upper, 2)
)

anydem_fib <- results %>%
  filter(outcome == "Any dementia" & drug == "Fibrates")

anydem_fib_text <- paste0(
  "HR:",
  round(anydem_fib$HR, 2),
  ", 95%CI:",
  round(anydem_fib$ci_lower, 2),
  "-",
  round(anydem_fib$ci_upper, 2)
)

anydem_sta <- results %>%
  filter(outcome == "Any dementia" & drug == "Statins")

anydem_sta_text <- paste0(
  "HR:",
  round(anydem_sta$HR, 2),
  ", 95%CI:",
  round(anydem_sta$ci_lower, 2),
  "-",
  round(anydem_sta$ci_upper, 2)
)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- sens-text

results_bp <-
  read.csv(
    here::here("data", "cprd", "regression_results_backpain.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  filter(!drug %in% c("hc_eze_sta", "hc_nag")) %>%
  unique()

sens_bp <- results_bp %>%
  filter(drug == "Any")

sens_bp_text <- paste0(
  "HR: ",
  round(sens_bp$HR, 2),
  ", 95%CI: ",
  round(sens_bp$ci_lower, 2),
  "-",
  round(sens_bp$ci_upper, 2)
)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- attritionFigure
#Define main dataset
main <- read.csv(here::here("data", "cprd", "cohort_attrition.csv"))

# Define node names
main[1, 2] <- paste0("M0")
for (i in 1:13) {
  main[1 + i, 2] <- paste0("M", i)
}
for (i in 1:13) {
  main[14 + i, 2] <- paste0("S", i)
}
colnames(main)[2] <- "Node"

main <- main[, c(2, 1)]

# Create figures for number removed
for (i in 1:13) {
  main[14 + i, 2] <- as.numeric(main[i, 2]) - as.numeric(main[i + 1, 2])
}

main[,2] <- comma_tight(main[,2])

# Create labels
main[1, 3] <-
  paste0("Data extract from the CPRD for ISAC Protocol 15_246R\n(n=",
         main[1, 2],
         ")")
main[2, 3] <-
  paste0("Patients with an index event of interest\n(n=", main[2, 2], ")")
main[3, 3] <-
  paste0("Patients flagged as acceptable by the CPRD\n(n=", main[3, 2], ")")
main[4, 3] <-
  paste0("Patients with an index date after the data extract start date (1/1/87)\n(n=",
         main[4, 2],
         ")")
main[5, 3] <-
  paste0("Patients with an index date before the data extract end date (29/02/16)\n(n=",
         main[5, 2],
         ")")
main[6, 3] <-
  paste0("Patients aged 40 and over at index\n(n=", main[6, 2], ")")
main[7, 3] <-
  paste0("Patients with a minimum of 12 months worth of data prior to index\n(n=",
         main[7, 2],
         ")")
main[8, 3] <- paste0("Patients alive at index\n(n=", main[8, 2], ")")
main[9, 3] <-
  paste0(
    "Patients with an index date prior to the last collection date for their practice\n(n=",
    main[9, 2],
    ")"
  )
main[10, 3] <-
  paste0("Patients with an index date prior to their transferral out of practice\n(n=",
         main[10, 2],
         ")")
main[11, 3] <-
  paste0("Patients recorded as male or female\n(n=", main[11, 2], ")")
main[12, 3] <-
  paste0("Patients initally prescribed a single class of lipid regulating agents \n(n=",
         main[12, 2],
         ")")
main[13, 3] <-
  paste0("Patients whose follow-up ends after their index date\n(n=",
         main[13, 2],
         ")")
main[14, 3] <-
  paste0("Patients with an index date after 1/1/1996\n(n=", main[14, 2], ")")

main[15, 3] <-
  paste0("Patients without an index event of interest\n(n=", main[15, 2], ")")
main[16, 3] <-
  paste0("Patients flagged as unacceptable by the CPRD\n(n=", main[16, 2], ")")
main[17, 3] <-
  paste0("Patients with an index date prior to the data extract start date (1/1/87)\n(n=",
         main[17, 2],
         ")")
main[18, 3] <-
  paste0("Patients with an index date after the data extract end date (29/02/16)\n(n=",
         main[18, 2],
         ")")
main[19, 3] <-
  paste0("Patients aged under 40 at index\n(n=", main[19, 2], ")")
main[20, 3] <-
  paste0("Patients with less than 12 months worth of data prior to index\n(n=",
         main[20, 2],
         ")")
main[21, 3] <-
  paste0("Patients with an index date after death\n(n=", main[21, 2], ")")
main[22, 3] <-
  paste0(
    "Patients with an index date after the last collection date for their practice\n(n=",
    main[22, 2],
    ")"
  )
main[23, 3] <-
  paste0("Patients with an index date after their transferral out of practice\n(n=",
         main[23, 2],
         ")")
main[24, 3] <-
  paste0("Patients of an unknown gender\n(n=", main[24, 2], ")")
main[25, 3] <-
  paste0("Patients initally receiving >1 class of lipid regulating agents\n(n=",
         main[25, 2],
         ")")
main[26, 3] <-
  paste0("Patients whose follow-up ends on/prior to their index date\n(n=",
         main[26, 2],
         ")")
main[27, 3] <-
  paste0("Patients with an index date prior to 1/1/1996\n(n=", main[27, 2], ")")

colnames(main)[3] <- "Label"



# Create graph
graph <- DiagrammeR::grViz(
  "digraph flowchart {
graph [pad=\"0.5\", nodesep=\"1\", ranksep=\".02\"]
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize = 14]


bnode1  [style = invis, shape=point, width = 0]
bnode2  [style = invis, shape=point, width = 0]
bnode3  [style = invis, shape=point, width = 0]
bnode4  [style = invis, shape=point, width = 0]
bnode5  [style = invis, shape=point, width = 0]
bnode6  [style = invis, shape=point, width = 0]
bnode7  [style = invis, shape=point, width = 0]
bnode8  [style = invis, shape=point, width = 0]
bnode9  [style = invis, shape=point, width = 0]
bnode10 [style = invis, shape=point, width = 0]
bnode11 [style = invis, shape=point, width = 0]
bnode12 [style = invis, shape=point, width = 0]
bnode13 [style = invis, shape=point, width = 0]

M1 [width = 7];
M2 [width = 7];
M3 [width = 7];
M4 [width = 7];
M5 [width = 7];
M6 [width = 7];
M7 [width = 7];
M8 [width = 7];
M9 [width = 7];
M10 [width = 7];
M11 [width = 7];
M12 [width = 7];
M13 [width = 7];
M14 [width = 7];
S1 [width = 7];
S2 [width = 7];
S3 [width = 7];
S4 [width = 7];
S5 [width = 7];
S6 [width = 7];
S7 [width = 7];
S8 [width = 7];
S9 [width = 7];
S10 [width = 7];
S11 [width = 7];
S12 [width = 7];
S13 [width = 7]

      #Define ranks
      subgraph {
          rank = same; bnode1; S1
      }
      subgraph {
          rank = same; bnode2; S2
      }
      subgraph {
          rank = same; bnode3; S3
      }
      subgraph {
          rank = same; bnode4; S4
      }
      subgraph {
          rank = same; bnode5; S5
      }
      subgraph {
          rank = same; bnode6; S6
      }
      subgraph {
          rank = same; bnode7; S7
      }
      subgraph {
          rank = same; bnode8; S8
      }
      subgraph {
          rank = same; bnode9; S9
      }
      subgraph {
          rank = same; bnode10; S10
      }
      subgraph {
          rank = same; bnode11; S11
      }
      subgraph {
          rank = same; bnode12; S12
      }
      subgraph {
          rank = same; bnode13; S13
      }

      # edge definitions with the node IDs
      M1 ->  bnode1  [arrowhead = none]
      M2 ->  bnode2  [arrowhead = none]
      M3 ->  bnode3  [arrowhead = none]
      M4 ->  bnode4  [arrowhead = none]
      M5 ->  bnode5  [arrowhead = none]
      M6 ->  bnode6  [arrowhead = none]
      M7 ->  bnode7  [arrowhead = none]
      M8 ->  bnode8  [arrowhead = none]
      M9 ->  bnode9  [arrowhead = none]
      M10 -> bnode10 [arrowhead = none]
      M11 -> bnode11 [arrowhead = none]
      M12 -> bnode12 [arrowhead = none]
      M13 -> bnode13 [arrowhead = none]

      bnode1 -> M2
      bnode2 -> M3
      bnode3 -> M4
      bnode4 -> M5
      bnode5 -> M6
      bnode6 -> M7
      bnode7 -> M8
      bnode8 -> M9
      bnode9 -> M10
      bnode10 -> M11
      bnode11 -> M12
      bnode12 -> M13
      bnode13 -> M14

      bnode1 -> S1 [arrowhead = none]
      bnode2 -> S2 [arrowhead = none]
      bnode3 -> S3 [arrowhead = none]
      bnode4 -> S4 [arrowhead = none]
      bnode5 -> S5 [arrowhead = none]
      bnode6 -> S6 [arrowhead = none]
      bnode7 -> S7 [arrowhead = none]
      bnode8 -> S8 [arrowhead = none]
      bnode9 -> S9 [arrowhead = none]
      bnode10 -> S10 [arrowhead = none]
      bnode11 -> S11 [arrowhead = none]
      bnode12 -> S12 [arrowhead = none]
      bnode13 -> S13 [arrowhead = none]


      # Define labels
      M1 [label = '@@1-1']
      M2 [label = '@@1-2']
      M3 [label = '@@1-3']
      M4 [label = '@@1-4']
      M5 [label = '@@1-5']
      M6 [label = '@@1-6']
      M7 [label = '@@1-7']
      M8 [label = '@@1-8']
      M9 [label = '@@1-9']
      M10 [label = '@@1-10']
      M11 [label = '@@1-11']
      M12 [label = '@@1-12']
      M13 [label = '@@1-13']
      M14 [label = '@@1-14']
      S1 [label = '@@1-15']
      S2 [label = '@@1-16']
      S3 [label = '@@1-17']
      S4 [label = '@@1-18']
      S5 [label = '@@1-19']
      S6 [label = '@@1-20']
      S7 [label = '@@1-21']
      S8 [label = '@@1-22']
      S9 [label = '@@1-23']
      S10 [label = '@@1-24']
      S11 [label = '@@1-25']
      S12 [label = '@@1-26']
      S13 [label = '@@1-27']
      }

      [1]:main$Label

      "
)

# Save

htmltools::html_print(DiagrammeR::add_mathjax(graph), viewer = NULL) %>%
  webshot::webshot(
    file = "figures/cprd-analysis/cohort_attrition.png",
    delay = 1,
    # selector = '.html-widget-static-bound',
    vwidth = 600,
    vheight = 744,
    cliprect = c(5, 300, 370, 510),
    zoom = 6
  )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- decisionTree

graph <- DiagrammeR::grViz(
  "digraph flowchart {
graph [pad=\"0.5\", nodesep=\"0.75\", ranksep=\"2\"]
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize = 30]

M1 [width = 4];
M2 [width = 4];
M3 [width = 4];
M4 [width = 4];

S1 [width = 4];
S2 [width = 4];
S3 [width = 4];

T1 [width = 3.5];
T2 [width = 3.5];
T3 [width = 3.5];
T4 [width = 3.5];
T5 [width = 3.5];


      #Define ranks
      subgraph {
          rank = same; S1; S2; S3
            }

      subgraph {
          rank = same; T1; T2; T3
      }

      subgraph {
          rank = same; T4; T5
      }

      # edge definitions with the node IDs
      {rank = same; M1 ->  M2 -> M3 -> M4[label = '  No', fontsize = 25, style = \"dashed\", color = \"DimGrey\"]}

      {rank = same; T1 ->  T2 -> T3 -> T4 -> T5[color = \"White\"]}


      M1 -> S1 [label = '  Yes', fontsize = 25]
      M2 -> S2 [label = '  Yes', fontsize = 25]
      M3 -> S3 [label = '  Yes', fontsize = 25]

      S1 -> T1 [label = '  No', fontsize = 25, style = \"dashed\", color = \"DimGrey\"]
      S1 -> T4 [label = '  Yes', fontsize = 25]

      S2 -> T2 [label = '  No', fontsize = 25, style = \"dashed\", color = \"DimGrey\"]
      S2 -> T4 [label = 'Yes', fontsize = 25]

      S3 -> T3 [label = '    No', fontsize = 25, style = \"dashed\", color = \"DimGrey\"]
      S3 -> T4 [label = 'Yes', fontsize = 25]

      M4 -> T4 [label = '  Yes', fontsize = 25]
      M4 -> T5 [label = '  No', fontsize = 25, style = \"dashed\", color = \"DimGrey\"]

      # Define labels
      M1 [label = \"Code for `Probable AD`?\"]
      M2 [label = \"Code for `Possible AD`?\"]
      M3 [label = \"Code for `Vascular` dementia?\"]
      M4 [label = \"Code for `Other` dementia \nor any dementia treatment?\"]

      S1 [label = \"Code for `Vascular` \nor `Other` dementia?\"]
      S2 [label = \"Code for `Vascular` \nor `Other` dementia?\"]
      S3 [label = \"Code for `Other` dementia?\"]

      T1 [label = \"Probable AD\"]
      T2 [label = \"Possible AD\"]
      T3 [label = \"Vascular dementia\"]
      T4 [label = \"Other dementia\"]
      T5 [label = \"No dementia\"]
      }



      "
)

htmltools::html_print(DiagrammeR::add_mathjax(graph), viewer = NULL) %>%
  webshot::webshot(
    file = "figures/cprd-analysis/decision_tree.png",
    delay = 1,
    # selector = '.html-widget-static-bound',
    vwidth = 700,
    vheight = 300,
    zoom = 6
  )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- sankeydiagram
library(networkD3)
library(dplyr)

library(readxl)

#initialize readin listing
all_codes <- data.frame()

mysheetlist <-
  c("med_dem_adprob",
    "med_dem_adposs",
    "med_dem_vas",
    "med_dem_oth")

i = 1
for (i in 1:length(mysheetlist)) {
  tempdf <-
    read_excel(path = "data/cprd/all_codes.xlsx", sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  all_codes <- rbind(all_codes, tempdf)
}

all_codes_N <- all_codes %>%
  group_by(sheetname) %>%
  summarise(N = n())

all_codes <- all_codes %>%
  rename(condition = sheetname) %>%
  select(read_code, condition) %>%
  mutate(
    condition = case_when(
      condition == "med_dem_adprob" ~ paste0(" Probable AD (N=", all_codes_N[which(all_codes_N$sheetname == "med_dem_adprob"), 2], ")"),
      condition == "med_dem_adposs" ~ paste0(" Possible AD (N=", all_codes_N[which(all_codes_N$sheetname == "med_dem_adposs"), 2], ")"),
      condition == "med_dem_vas" ~ paste0(" Vascular dementia (N=", all_codes_N[which(all_codes_N$sheetname == "med_dem_vas"), 2], ")"),
      condition == "med_dem_oth" ~ paste0(" Other dementia (N=", all_codes_N[which(all_codes_N$sheetname == "med_dem_oth"), 2], ")")
    )
  )





smeeth_codes <- read_excel(path = "data/cprd/smeeth_codes.xlsx") %>%
  mutate(alzheimers = alzhemiers) %>%
  select(-alzhemiers) %>%
  filter(is.na(prevalent)) %>%
  mutate(condition_smeeth = case_when(alzheimers == "Y" ~ "AD",
                                      other == "Y" ~ "Non-AD dementia")) %>%
  filter(!is.na(condition_smeeth)) %>%
  rename(read_code = medcode) %>%
  select(read_code, condition_smeeth)

smeeth_codes_N <- smeeth_codes %>%
  group_by(condition_smeeth) %>%
  summarise(n = n())

smeeth_codes <- smeeth_codes %>%
  mutate(
    condition_smeeth = case_when(
      condition_smeeth == "AD" ~ paste0("AD (N=", smeeth_codes_N[which(smeeth_codes_N$condition_smeeth == "AD"), 2], ")"),
      condition_smeeth == "Non-AD dementia" ~ paste0("Non-AD dementia (N=", smeeth_codes_N[which(smeeth_codes_N$condition_smeeth == "Non-AD dementia"), 2], ")")
    )
  )


links <- full_join(all_codes, smeeth_codes) %>%
  mutate(condition_smeeth = ifelse(
    is.na(condition_smeeth),
    "Not included (N = 21)",
    condition_smeeth
  )) %>%
  mutate(condition = ifelse(is.na(condition), "Not included (N = 6)", condition)) %>%
  group_by(condition, condition_smeeth) %>%
  summarise(value = n()) %>%
  rename(source = condition, target = condition_smeeth)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name = c(as.character(links$source),
                             as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Make the Network
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  sinksRight = FALSE,
  fontSize = 10,
  colourScale = JS("d3.scaleOrdinal(d3.schemeGreys);"),
)


sn <- htmlwidgets::onRender(
  p,
  '
  function(el,x){
  // select all our node text
  d3.select(el)
  .selectAll(".node text")
  .filter(function(d) { return d.name.startsWith(" "); })
  .attr("x", x.options.nodeWidth - 20)
  .attr("text-anchor", "end");
  }
  '
)

networkD3::saveNetwork(sn, "sn.html", )

webshot::webshot(
  "sn.html",
  file = "figures/cprd-analysis/sankey_diagram.png",
  vwidth = 900,
  vheight = 357,
  cliprect = c(1, 150, 600, 350),
  
  delay = 0,
  zoom = 7
)

file.remove("sn.html")

# ---- immortalTime

library(ggplot2)
library(patchwork)

df <- data.frame(
  type = c("Exposed", "Exposed", "Unexposed"),
  length = c(1, 2, 2),
  group = c("Excluded immortal time", "Captured time", "Captured time")
)

df2 <- data.frame(
  type = c("Exposed", "Exposed", "Unexposed"),
  length = c(1, 2, 2),
  group = c("Misclassified immortal time", "Exposed", "Unexposed")
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
      label = c("Index event", "Index event\n(cohort entry)")
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
                      label = "Statin initiation\n(cohort entry)"),
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
  filename =  "figures/cprd-analysis/immortal_time.png",
  width = 21,
  height = 14,
  units = "cm"
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- statinTypeTable-table

statinTypeTable_table <-
  rio::import(here::here("data/cprd/sta_type_table.tsv"))[c(-1:-3,-8), -7] %>%
  mutate(V2 = as.numeric(gsub(pattern = ",", "", V2)),
         V4 = as.numeric(gsub(pattern = ",", "", V4)),
         V6 = as.numeric(gsub(pattern = ",", "", V6))) %>%
  tidyr::unite("Hydrophilic", 2:3, remove = TRUE, sep = " (") %>%
  tidyr::unite("Lipophilic", 3:4, remove = TRUE, sep = " (") %>%
  mutate(Hydrophilic = paste0(Hydrophilic, "%)")) %>%
  mutate(Lipophilic = paste0(Lipophilic, "%)")) %>%
  rename("Total" = V6) %>%
  mutate(
    V1 = case_when(
      V1 == ">=1996" ~ "<=2000",
      V1 == ">=2001" ~ "2001-2005",
      V1 == ">=2006" ~ "2006-2010",
      V1 == ">=2011" ~ "2010<"
    )
  ) %>%
  rename("Prescription Year Group" = V1) %T>%
  write.csv(here::here("data/table_words/statinType.csv"))

if (doc_type == "docx") {
  apply_flextable(statinTypeTable_table, caption = "(ref:statinTypeTable-caption)")
} else{
  knitr::kable(
    statinTypeTable_table,
    format = "latex",
    caption = "(ref:statinTypeTable-caption)",
    caption.short = "(ref:statinTypeTable-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 7) %>%
    row_spec(2:nrow(statinTypeTable_table) - 1, hline_after = TRUE) %>%
    row_spec(0, bold = TRUE)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- statinType

sta_type <-
  read.csv(
    here::here("data", "cprd", "regression_results_sta_type.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  )

sta_type$outcome[which(sta_type$outcome == "")] <-
  "Vascular dementia"

sta_type$grouping <- "Hydrophilic"
sta_type$grouping[which(sta_type$analysis != "Hydrophilic")] <-
  "Lipophilic"

generate_forester_plot(sta_type,
                       here::here("figures/cprd-analysis/forester_sta_type.png"),
                       first_col = "grouping",
                       display = T)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- completeCase

results_p1 <-
  read.csv(
    here::here("data", "cprd", "regression_results_p1.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(analysis = "Imputed data")

results_cc <-
  read.csv(
    here::here("data", "cprd", "regression_results_complete_case.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  )

results_cc <- results_p1 %>%
  rbind(results_cc)

generate_forester_plot(results_cc,
                       here::here("figures/cprd-analysis/forester_complete_case.png"),
                       first_col = "analysis")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- unadjustedComparison

results_p1 <-
  read.csv(
    here::here("data", "cprd", "regression_results_p1.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(analysis = "Full covariates",
         covariates = "Full covariates")

results_un <-
  read.csv(
    here::here("data", "cprd", "regression_results_unadjusted.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  )

results_un <- results_p1 %>%
  rbind(results_un)

results_un$covariates <-
  factor(results_un$covariates,
         levels = c("Full covariates", "Age + Sex", "Age"))

# Plot unadjusted figure
generate_forester_plot(results_un,
                       here::here("figures/cprd-analysis/forester_unadjusted.png"),
                       first_col = "covariates",xlimits = c(0.5,2))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- controlOutcomesText

results_ihd <-
  read.csv(
    here::here("data", "cprd", "regression_results_ihd.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(outcome = "IHD")

results_bp <-
  read.csv(
    here::here("data", "cprd", "regression_results_backpain.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(outcome = "Back pain")

results_dm <-
  read.csv(
    here::here("data", "cprd", "regression_results_dm_type2.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(outcome = "Type 2 Diabetes")

results_co <- results_ihd %>%
  rbind(results_bp) %>%
  rbind(results_dm) %>%
  filter((drug %in% c("Any"))) %>%
  mutate(grouping = ifelse(drug == "Any", "All classes", "Single class"))


ihd_text <-
  estimate(results_co$HR[1],
           results_co$ci_lower[1],
           results_co$ci_upper[1],
           type = "HR")

backpain_text <-
  estimate(results_co$HR[2],
           results_co$ci_lower[2],
           results_co$ci_upper[2],
           type = "HR")

dm_type2_text <-
  estimate(results_co$HR[3],
           results_co$ci_lower[3],
           results_co$ci_upper[3],
           type = "HR")



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- controlOutcomes

generate_forester_plot(
  results_co,
  here::here("figures/cprd-analysis/forester_control_outcomes.png"),
  first_col = "drug",
  top_title = "Back pain",
  outcome_levels = c("Back pain", "IHD", "Type 2 Diabetes"),
  adjustment = 0.4, height_expansion= 0.1
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- diagnosisType-table

diagnosisType_table <-
  rio::import("data/cprd/dementia_types.tsv")[c(-1:-3), c(-13)] %>%
  mutate(
    V2 = as.numeric(gsub(pattern = ",", "", V2)),
    V4 = as.numeric(gsub(pattern = ",", "", V4)),
    V6 = as.numeric(gsub(pattern = ",", "", V6)),
    V8 = as.numeric(gsub(pattern = ",", "", V8)),
    V10 = as.numeric(gsub(pattern = ",", "", V10)),
    V12 = as.numeric(gsub(pattern = ",", "", V12))
  ) %>%
  tidyr::unite("No dementia", 2:3, remove = TRUE, sep = " (") %>%
  tidyr::unite("Probable AD", 3:4, remove = TRUE, sep = " (") %>%
  tidyr::unite("Possible AD", 4:5, remove = TRUE, sep = " (") %>%
  tidyr::unite("Vascular dementia", 5:6, remove = TRUE, sep = " (") %>%
  tidyr::unite("Other dementia", 6:7, remove = TRUE, sep = " (") %>%
  mutate(across(c(2:6), ~ paste0(.x, "%)"))) %>%
  rename("Total" = V12) %>%
  mutate(
    V1 = case_when(
      V1 == ">=1996" ~ "<=2000",
      V1 == ">=2001" ~ "2001-2005",
      V1 == ">=2006" ~ "2006-2010",
      V1 == ">=2011" ~ "2010<",
      T ~ "Total"
    )
  ) %>%
  rename("Year of cohort entry" = V1) %T>%
  write.csv(here::here("data/table_words/diagnosisType.csv"))

if (doc_type == "docx") {
  apply_flextable(diagnosisType_table, caption = "(ref:diagnosisType-caption)")
} else{
  knitr::kable(
    diagnosisType_table,
    format = "latex",
    caption = "(ref:diagnosisType-caption)",
    caption.short = "(ref:diagnosisType-scaption)",
    booktabs = TRUE,
    row.names = FALSE,
    align = "lccccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1:6, width = paste0(5.33, "em")) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 7) %>%
    row_spec(2:nrow(diagnosisType_table) - 1, hline_after = TRUE) %>%
    row_spec(0, bold = TRUE)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortEntry

results_ce <-
  read.csv(
    here::here("data", "cprd", "regression_results_entry_year.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(
    analysis = case_when(
      analysis == "Year Group 1" ~ "<=2000",
      analysis == "Year Group 2" ~ "2001-2005",
      analysis == "Year Group 3" ~ "2006-2010",
      analysis == "Year Group 4" ~ "2010<"
    )
  )

generate_forester_plot(
  results_ce,
  here::here("figures/cprd-analysis/forester_cohort_entry.png"),
  first_col = "analysis",
  top_title = "Any dementia",
  # outcome_levels = c("Probable AD", "Any dementia"),
  height_expansion= 0.1, adjustment = 0.35
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- fuByCohortEntry

results_fu_by_ce <-
  read.csv(
    here::here("data", "cprd", "fu_by_cohort_entry.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(c1 = case_when(
    c1 == "1" ~ ">=1996",
    c1 == "2" ~ ">=2001",
    c1 == "3" ~ ">=2006",
    c1 == "4" ~ ">=2011"
  )) %>%
  tidy_nums()


text_fu_by_ce <-
  paste0(
    "Median follow-up for each cohort entry group was as follows: ",
    results_fu_by_ce$c1[1],
    ": ",
    results_fu_by_ce$c2[1],
    " years; ",
    results_fu_by_ce$c1[2],
    ": ",
    results_fu_by_ce$c2[2],
    " years; ",
    results_fu_by_ce$c1[3],
    ": ",
    results_fu_by_ce$c2[3],
    " years; and ",
    results_fu_by_ce$c1[4],
    ": ",
    results_fu_by_ce$c2[4],
    " years."
  )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- pregnancy

results_p1 <-
  read.csv(
    here::here("data", "cprd", "regression_results_p1.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  mutate(analysis = "Main cohort")

results_p2 <-
  read.csv(here::here("data", "cprd", "regression_results_pregnancy.csv"),
           header =
             TRUE)

results <- results_p1 %>%
  rbind(results_p2) %>%
  filter(!drug %in% c("hc_eze_sta", "hc_nag"))


# Plot primary analyses #1 & #2

generate_forester_plot(results,
                       here::here("figures/cprd-analysis/forester_pregnancy.png"),
                       first_col = "analysis")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- smeethText

results_smeeth_azd <-
  read.csv(
    here::here("data/cprd/regression_results_smeeth_azd.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  filter(drug == "Any")

results_smeeth_oth <-
  read.csv(
    here::here("data/cprd/regression_results_smeeth_oth.csv"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
  filter(drug == "Any")

smeeth_azd_text <-
  estimate(
    results_smeeth_azd$HR,
    results_smeeth_azd$ci_lower,
    results_smeeth_azd$ci_upper,
    type = "",
    sep = ")"
  )

smeeth_oth_text <-
  estimate(
    results_smeeth_oth$HR,
    results_smeeth_oth$ci_lower,
    results_smeeth_oth$ci_upper,
    type = "",
    sep = ")"
  )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- covariateDef-table

covar <-
  read.csv(
    here::here("data", "cprd", "covariate_definitions.csv"),
    header = FALSE,
    stringsAsFactors = FALSE
  )

colnames(covar) <- covar[1, ]
covar <- covar[-1, ]

covariateDef_table <- covar %T>%
  write.csv(here::here("data/table_words/followUptable.csv"))
  
if (doc_type == "docx") {
  apply_flextable(covariateDef_table, caption = "(ref:covariateDef-caption)")
} else{
  
  covariateDef_table[4,2] <- "(ref:covariateDef-cell1)"
  covariateDef_table[8,2] <- "(ref:covariateDef-cell2)"
  
  knitr::kable(
    covariateDef_table,
    format = "latex",
    caption = "(ref:covariateDef-caption)",
    caption.short = "(ref:covariateDef-scaption)",
    booktabs = TRUE, 
    row.names = FALSE,
    align = "lc"
  ) %>%
    row_spec(0, bold = TRUE) %>%  
    column_spec(1, width = paste0(15, "em")) %>%
    column_spec(2, width = paste0(25, "em")) %>%
    kable_styling(latex_options = c("HOLD_position"), font_size = 9) %>%
    row_spec(2:nrow(covariateDef_table ) - 1, hline_after = TRUE)
}



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- followUp-table

main_tmp <- read.csv(here::here("data/cprd/crude_rates.csv")) %>%
  mutate(c1 = comma(as.numeric(c1)/365.25),
       # Calculate rate per 100000 years
       # Currently in rate per 1000 days-at-risk
       c3 = comma(as.numeric(c3)*365.25*100), 
       c2 = comma(as.numeric(c2))) %>%
  mutate(drug = rep(c("total", "hc_bas", "hc_eze", "hc_eze_sta", "hc_fib", "hc_nag", "hc_om3", "hc_sta", "None"), 5)) %>%
  mutate(c3 = ifelse(drug %in% c("hc_nag","hc_eze_sta"), "-",c3)) %>%
  mutate(outcome = rep(c("All-cause dementia", "Probable Alzheimer's disease","Vascular demenia", "Possible Alzheimer's disease","Other dementia"), each = 9)) 

main <- cbind(main_tmp[1:9,c(4,c(2,1,3))], # All-cause
                          main_tmp[10:18,c(2,1,3)], # prob AD
                          main_tmp[28:36,c(2,1,3)], # Other
                          main_tmp[19:27,c(2,1,3)], # poss AD
                          main_tmp[37:45,c(2,1,3)] # Vascular
                          )

colnames(main) <- c("v2", "v5", "v4", "v6", "v5_2", "v4_2", "v6_2", "v5_3", "v4_3", "v6_3", "v5_4", "v4_4", "v6_4", "v5_5", "v4_5", "v6_5")

main[10,] <- c("class", rep("",15))

main <- main %>%
  arrange(match(v2, c("None","class","hc_sta","hc_om3", "hc_fib","hc_eze","hc_bas","hc_eze_sta","hc_nag"))) %>%
  mutate(v2 = case_when(v2 == "None" ~ "No LRA (unexposed)",
                          v2 == "hc_bas" ~ "  BAS",
                          v2 == "hc_sta" ~ "  Statins",
                          v2 == "hc_fib" ~ "  Fibrates",
                          v2 == "hc_om3" ~ "  Omega-3 FAGs",
                          v2 == "hc_eze" ~ "  Ezetimibe",
                          v2 == "hc_eze_sta" ~ "  Ezetimibe + Statins",
                          v2 == "hc_nag" ~ "  NAG",
                          v2 == "class" ~ "By drug class",
                          T ~ "Total")) %T>%
  write.csv(here::here("data/table_words/followUptable.csv"))


if(doc_type == "docx"){
  
  labels <- c("Exposure group", rep(c("Events","PYAR","Rate"),5))
  
  names(labels) <- colnames(main)
  
  flextable::flextable(main) %>%
    set_header_labels(values = labels) %>%
    flextable::add_header(
      "v2"   = "Exposure group",
      "v5"   = "Any dementia",
      "v4"   ="Any dementia",
      "v6"   = "Any dementia",
      "v5_2" = "Probable AD",
      "v4_2" ="Probable AD",
      "v6_2" = "Probable AD",
      "v5_3" = "Possible AD",
      "v4_3" ="Possible AD",
      "v6_3" = "Possible AD",
      "v5_4" = "Vascular dementia",
      "v4_4" ="Vascular dementia",
      "v6_4" = "Vascular dementia",
      "v5_5" = "Other dementia",
      "v4_5" ="Other dementia",
      "v6_5" = "Other dementia",
      top = TRUE
    ) %>%
    flextable::merge_h(part = "header") %>%
    flextable::merge_v(part = "header") %>%
    flextable::bg(bg = "#A6A6A6", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::bold(j=1,i=c(1:2,10), part = "body") %>%
    flextable::bold(i=10, part = "body") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::hline(part = "header") %>%
    flextable::hline(i=9,part = "body") %>%
    flextable::padding(i=3:9, j=1, padding.left=20) %>%
    flextable::border(j=c(1,4,7,10,13), part = "all", border.right = officer::fp_border(color = "black")) %>%
    flextable::align(align = "center", part = "all" ) %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(i = 1, align = "center", part = "header") %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::set_caption("(ref:followUp-caption)") %>%
    flextable::add_footer(v2 = "") %>%
    flextable::merge_at(j=1:16, part = "footer") %>%
    flextable::fontsize(size = 8, part = "footer") %>%
    flextable::compose(part = "footer",
                       i = 1,
                       j = 1:16,
                       value = as_paragraph(as_sup("a"),
                                     "Crude rate per 100,000 participant-years-at-risk\n",
                                     as_sup("b"),
                                     "One treatment containing both drugs, rather than the two classes being prescribed concurrently\n",
                                     as_b("Abbreviations: "),
                                     "AD - Alzheimer's disease; ",
                                     "BAS - Bile acid sequestrants; ",
                                     "LRA - Lipid regulating agent; ",
                                     "NAG - Nicotinic acid groups; ",
                                     "Omega-3 FAGs - Omega-3 fatty acid groups; ",
                                     "PYAR - Participant-years-at-risk."
    )) %>%
    flextable::compose(part = "body", i = 8, j=1, value = as_paragraph("Ezetimibe + Statins",
                                                                      as_sup("b"))) %>%
    flextable::compose(part = "header", i = 2, j=c(4,7,10,13,16), value = as_paragraph("Rate",
                                                                       as_sup("a")))

} else{
  
  table <- main %>%
    mutate(v2 = as.character(v2)) %>%
    mutate(v2 = ifelse(v2=="  Ezetimibe + Statins", paste("  Ezetimibe + Statins",footnote_marker_symbol(2,"latex")),v2)) %>%
    mutate(v2 = cell_spec(v2, bold  = c(T,T,rep(F,7),T), format = "latex")) %>%
  knitr::kable(
    format = "latex",
    caption = "(ref:followUp-caption)",
    caption.short = "(ref:followUp-scaption)",
    booktabs = TRUE, 
    linesep = "", 
    align = "lccccccccccccccc",
    row.names = FALSE,
    col.names = c("Exposure Group",
                  rep(c("Events","PYAR",paste("Rate",footnote_marker_symbol(1,"latex"))),5)), escape = F
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(position = "center",
                  font_size = 8)  %>%
    add_header_above(
      c(
        " ",
        "Any dementia" = 3,
        "Possible AD" = 3,
        "Probable AD" = 3,
        "Vascular dementia" = 3,
        "Other dementia" = 3
      ),bold = TRUE
    ) %>%
    add_indent(c(3:9), level_of_indent = 1) %>%
    row_spec(nrow(main) - 1, hline_after = TRUE) %>%
    column_spec(1, width = paste0(9, "em")) %>%
    column_spec(2:16, width = paste0(3, "em")) %>%
    column_spec(c(1,4,7,10,13),border_right = T) %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "",
      general = paste(
        footnote_marker_symbol(1, "latex",T),
        "Crude rate per 100,000 participant-years-at-risk\\\\newline",
        footnote_marker_symbol(2, "latex",T),
        "One treatment containing both drugs, rather than the two classes being prescribed concurrently\\\\newline",
        "\\\\textbf{Abbreviations:}",
        "AD - Alzheimer's disease; ",
        "BAS - Bile acid sequestrants;",
        "LRA - Lipid regulating agent; ",
        "NAG - Nicotinic acid groups; ",
        "Omega-3 FGs - Omega-3 Fatty acid groups;",
        "PYAR - Participant-years-at-risk."
        ), escape = F
    )
    
  table <- gsub("textbackslash\\{\\}dag\\\\", "\\dag", table)
  table <- gsub("textbackslash\\{\\}textsuperscript\\\\", "\\textsuperscript", table)
  
  table
  
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- dags
# 


ggplot2::ggsave(filename = file.path("figures","cprd-analysis","indicationBias.png"),
                dagitty::dagitty("dag {
  Statin [exposure,pos=\"0.000,0.000\"]
  \"Vascular outcomes\" [outcome,pos=\"2.000,0.000\"]
  \"Uncontrolled variable\" [pos=\"1.000,1.000\"]
  \"Uncontrolled variable\" -> Statin
  \"Uncontrolled variable\" -> \"Vascular outcomes\"
}") %>% ggdag_classic(size = 6) +
  xlim(-0.5,3) +
  geom_text(data = data.frame(x = 0.316392393234329,
                              y = 0.537705383514239,
                              label = "Prompts"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            angle = 0L,
            lineheight = 1L,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            family = "sans",
            fontface = "bold",
            inherit.aes = FALSE,
            show.legend = FALSE) +
  geom_text(data = data.frame(x = 0.88,
                              y = 0.05,
                              label = "Induced positive"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            angle = 0L,
            lineheight = 1L,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            family = "sans",
            fontface = "bold",
            inherit.aes = FALSE,
            show.legend = FALSE) +
  geom_text(data = data.frame(x = 0.88,
                              y = -0.04,
                              label = "association"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            angle = 0L,
            lineheight = 1L,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            family = "sans",
            fontface = "bold",
            inherit.aes = FALSE,
            show.legend = FALSE) +
  geom_text(data = data.frame(x = 1.65891420807966,
                              y = 0.537705383514239,
                              label = "Causes"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            angle = 0L,
            lineheight = 1L,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            family = "sans",
            fontface = "bold",
            inherit.aes = FALSE,
            show.legend = FALSE) +
  geom_curve(data = data.frame(x = 0.220612676359188,
                               y = -0.000788829298738025,
                               xend = 1.45,
                               yend = -0.00157828084913909),
             mapping = aes(x = x,
                           y = y,
                           xend = xend,
                           yend = yend),
             linetype = "dashed",
             angle = 0L,
             size = 0.6,
             colour = "black",
             curvature = 0,
             arrow = structure(list(angle = 0,
                                    length = structure(0,
                                                       class = "unit",
                                                       valid.unit = 2L,
                                                       unit = "inches"),
                                    ends = 2L,
                                    type = 2L),
                               class = "arrow"),
             inherit.aes = FALSE,
             show.legend = FALSE) +
  
  theme_dag(),
width = 8,
height = 3)