#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- smeethComparison-table

smeethComparison_table <- mtcars[1:4,1:3]

if(doc_type == "docx"){
knitr::kable(smeethComparison_table,caption = "(ref:smeethComparison-caption)")
}else{
knitr::kable(smeethComparison_table, format = "latex", caption = "(ref:smeethComparison-caption)", caption.short = "(ref:smeethComparison-scaption)", booktabs = TRUE) %>% 
row_spec(0, bold = TRUE) %>%
kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- load-files

attrition <- read.csv(here::here("data","cprd","cohort_attrition.csv"))

p1 <- read.csv(here::here("data","cprd","regression_results_p1.csv")) %>%
  filter(drug == "Any") %>%
  select(outcome, N_fail) %>%
  unique()

p1$outcome <- factor(p1$outcome, levels = c("Any dementia", "Probable AD", "Possible AD", "Vascular dementia", "Other dementia"))
p1 <- p1[order(p1$outcome),]

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- readExample-table

readExample_table <- read.csv(here::here("data","cprd","read_code_example.csv")) %>%
  dplyr::mutate("Read code" = Read.code) %>%
  dplyr::select("Level","Read code","Term")

if (doc_type == "docx") {
  knitr::kable(readExample_table, caption = "(ref:readExample-caption)")
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
# ---- cprdCharacteristics-table

table1 <- read.csv(here::here("data","cprd","table1.csv"),header=FALSE)
table1 <- data.table::transpose(table1)
colnames(table1) <- as.character(unlist(table1[1,]))
table1 = table1[-1, ]

colnames(table1)[1] <- " "

table1[1,1] <- "Sample size"
table1[2,1] <- "Index year \\newline(median)"
table1[3,1] <- "Female"
table1[4,1] <- "Age"
table1[5,1] <- "CAD"
table1[6,1] <- "CBS"
table1[7,1] <- "CVD"
table1[8,1] <- "Charlson (ever > 0)"
table1[9,1] <- "IMD-2010 (median)"
table1[10,1] <-"Consulation rate (mean/SD)"
table1[11,1] <-"Alcohol (ever)"
table1[12,1] <-"Smoking (ever)"
table1[13,1] <-"BMI (mean/SD)"
table1[14,1] <-"PAD"
table1[15,1] <-"Hypertension"
table1[16,1] <-"Total cholesterol (mean/SD)"
table1[17,1] <-"CKD"
table1[18,1] <-"Type 1 Diabetes"

# Rename other columns not included in Table 1
table1[19,1] <-"Stopped"
table1[20,1] <-"Added"
table1[21,1] <-"Switched"
table1[22,1] <-"Other drug within 5yrs"
table1.copy <- table1

table1 <- table1[,c(1,10,7,9,2:6,8)]
table1_disp <- table1[c(1:18),]

# Quick check to ensure data quality
t<- table1_disp[1,2:10]
t<- as.numeric(table1_disp[1,2:10])

if(t[1]-sum(t[2:9]) != 0){
  stop("cprdCharacteristics-table: Sum of subgroups != Total sample size")
}

# Create display table using kable
cprdCharacteristics_table <- table1_disp

if(doc_type == "docx") {
  knitr::kable(cprdCharacteristics_table, caption = "(ref:cprdCharacteristics-caption)")
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
    column_spec(1, width = paste0(10,"em"), bold = TRUE) %>%
    column_spec(2:10, width = paste0(6,"em")) %>%
    row_spec(2:nrow(cprdCharacteristics_table)-1, hline_after = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"), font_size = 6) %>%
    kableExtra::footnote(
      threeparttable = TRUE,
      general_title = "Abbreviations:",
      general = paste(
        "LRA - Lipid regulating agent;",
        "IMD - Index of Multiple Deprivation;",
        "BMI - Body Mass Index;",
        "CAD - Coronary Arterial Disease;",
        "CBS - Coronary Bypass Surgery;",
        "CVD - Cardiovascular disease;",
        "PAD - Peripheral arterial disease;",
        "CKD - Chronic Kidney Disease"
      )
    )
    
    table <- gsub("textbackslash\\{\\}newline","\\newline",table)
    
    table
  
}
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- missing-data

missingdata <- read.csv(here::here("data","cprd","missingdata.csv"), header=TRUE)

total <- missingdata[1,1]
totalpercent <- round(total/as.numeric(missingdata[1,1])*100,1)
totaltext <- paste0(format(total, big.mark = ","), " participants")
total_ab_text <- format(round(total,-3), big.mark = ",")

missing <- missingdata[2,1]
missingpercent <- round(missing/as.numeric(missingdata[1,1])*100,1)
missingtext <- paste0(format(missing, big.mark = ","), " participants (", missingpercent,"%)")

imd <- missingdata[3,1]
imdpercent <- round(imd/as.numeric(missingdata[1,1])*100,1)
imdtext <- paste0(format(imd, big.mark = ","), " participants (", imdpercent,"%)")

alcohol <- missingdata[4,1]
alcoholpercent <- round(alcohol/as.numeric(missingdata[1,1])*100,1)
alcoholtext <- paste0(format(alcohol, big.mark = ","), " participants (", alcoholpercent,"%)")

smoking <- missingdata[5,1]
smokingpercent <- round(smoking/as.numeric(missingdata[1,1])*100,1)
smokingtext <- paste0(format(smoking, big.mark = ","), " participants (", smokingpercent,"%)")

bmi <- missingdata[6,1]
bmipercent <- round(bmi/as.numeric(missingdata[1,1])*100,1)
bmitext <- paste0(format(bmi, big.mark = ","), " participants (", bmipercent,"%)")

chol <- missingdata[7,1]
cholpercent <- round(chol/as.numeric(missingdata[1,1])*100,1)
choltext <- paste0(format(chol, big.mark = ","), " participants (", cholpercent,"%)")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- characteristics

characteristics <- read.csv(here::here("data","cprd","characteristics.csv"),header=TRUE)

# Median and IQR for follow-up
fu_text <- paste0(round(characteristics$c1[1],1),
                  " years (IQR:",
                  round(characteristics$c1[2],1),
                  "-",
                  round(characteristics$c1[3],1),
                  ")")

fu_ab_text <- paste0(round(characteristics$c1[1],1),
                     " participant-years")

# Median and IQR for age at index
age_text <- paste0(characteristics$c1[4],
                   " years (IQR:",
                   characteristics$c1[5],
                   "-",
                   characteristics$c1[6],
                   ")")

# Percentage of users taking a statin
t1 <- read.csv(here::here("data","cprd","table1.csv"),
               header=FALSE, 
               stringsAsFactors = FALSE) %>%
  filter(!V1 %in% c("first_drug", "No LRA","Whole Sample"))

t1$V2 <- as.numeric(t1$V2)

percentage.statins <- paste0(round((t1$V2[7]/sum(t1$V2[1:7])*100),2),"%")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- sas

table2 <- table1.copy
table2 <- table2[,c(1,10,7,9,2:6,8)] %>%
  select(-'No LRA')
table2 <- table2[c(1,19:22),]

ft <- flextable::flextable(table2)
ft <- flextable::bg(ft, bg = "#A6A6A6", part = "header")
ft <- bold(ft, part = "header")
ft <- bold(ft, j=1, part = "body")
ft <- align(ft, align = "center", part = "all" )
ft <- align(ft, j = 1, align = "left")
ft <- bg(ft, i = ~ seq(from = 1, to = nrow(table2)) %% 2 == 0, bg = "#DDDDDD", part = "body")
ft <- fontsize(ft, size = 7, part = "all")
ft <- set_table_properties(ft, layout = "autofit")
ft <- flextable::footnote(ft, i = 2:4, j = 1,
                          value = as_paragraph(
                            c("Stopped - greater than 6 months between last prescription and end of follow-up",
                              "Added - second drug prescribed before last prescription for the index drug",
                              "Switched - second drug prescribed after last prescription for the index drug."
                            )
                          ),ref_symbols = c("a","b","c"),
                          part = "body", inline = TRUE)
ft.table2 <- ft

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- azd-text

results_p1 <-
  read.csv(here::here("data","cprd", "regression_results_p1.csv"),
           header = TRUE, 
           stringsAsFactors = FALSE)

results_p2 <-
  read.csv(here::here("data","cprd", "regression_results_p2.csv"), header =
             TRUE)

results <- results_p1 %>%
  rbind(results_p2) %>%
  filter(!drug %in% c("hc_eze_sta","hc_nag")) %>%
  unique()

probad <- results %>%
  filter(outcome == "Probable AD" & drug == "Any")

probad_text <- paste0("HR: ", 
                      round(probad$HR,2),
                      ", 95%CI: ",
                      round(probad$ci_lower,2),
                      "-",
                      round(probad$ci_upper,2))

probad_fib <- results %>%
  filter(outcome == "Any dementia" & drug == "Fibrates")

probad_fib_text <- paste0("HR: ", 
                          round(probad_fib$HR,2),
                          ", 95%CI: ",
                          round(probad_fib$ci_lower,2),
                          "-",
                          round(probad_fib$ci_upper,2))

possad <- results %>%
  filter(outcome == "Possible AD" & drug == "Any")

possad_text <- paste0("HR: ", 
                      round(possad$HR,2),
                      ", 95%CI: ",
                      round(possad$ci_lower,2),
                      "-",
                      round(possad$ci_upper,2))

vasdem <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Any")

vasdem_text <- paste0("HR: ", 
                      round(vasdem$HR,2),
                      ", 95%CI: ",
                      round(vasdem$ci_lower,2),
                      "-",
                      round(vasdem$ci_upper,2))

vasdem_eze <- results %>%
  filter(outcome == "Vascular dementia" & drug == "Ezetimibe")

vasdem_eze_text <- paste0("HR: ", 
                          round(vasdem_eze$HR,2),
                          ", 95%CI: ",
                          round(vasdem_eze$ci_lower,2),
                          "-",
                          round(vasdem_eze$ci_upper,2))

othdem <- results %>%
  filter(outcome == "Other dementia" & drug == "Any")

othdem_text <- paste0("HR: ", 
                      round(othdem$HR,2),
                      ", 95%CI: ",
                      round(othdem$ci_lower,2),
                      "-",
                      round(othdem$ci_upper,2))

othdem_eze <- results %>%
  filter(outcome == "Other dementia" & drug == "Ezetimibe")

othdem_eze_text <- paste0("HR: ", 
                          round(othdem_eze$HR,2),
                          ", 95%CI: ",
                          round(othdem_eze$ci_lower,2),
                          "-",
                          round(othdem_eze$ci_upper,2))



anydem <- results %>%
  filter(outcome == "Any dementia" & drug == "Any")

anydem_text <- paste0("HR: ", 
                      round(anydem$HR,2),
                      ", 95%CI: ",
                      round(anydem$ci_lower,2),
                      "-",
                      round(anydem$ci_upper,2))

anydem_fib <- results %>%
  filter(outcome == "Any dementia" & drug == "Fibrates")

anydem_fib_text <- paste0("HR: ", 
                          round(anydem_fib$HR,2),
                          ", 95%CI: ",
                          round(anydem_fib$ci_lower,2),
                          "-",
                          round(anydem_fib$ci_upper,2))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- sens-text

results_bp <-
  read.csv(here::here("data","cprd", "regression_results_backpain.csv"),
           header = TRUE, 
           stringsAsFactors = FALSE) %>%
  filter(!drug %in% c("hc_eze_sta","hc_nag")) %>%
  unique()

sens_bp <- results_bp %>%
  filter(drug == "Any")

sens_bp_text <- paste0("HR: ", 
                       round(sens_bp$HR,2),
                       ", 95%CI: ",
                       round(sens_bp$ci_lower,2),
                       "-",
                       round(sens_bp$ci_upper,2))