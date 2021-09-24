#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Statins RCT

results_statins_rct <- rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  janitor::clean_names() %>%
  # Extract two trials
  filter(study_id %in% c("10562","90003")) %>%
  rename("lower_CI" = upper_95_percent, 
         "upper_CI" = lower_95_percent) %>%
  mutate(across(c(point_estimate,starts_with(c("number_","cases_")), ends_with("_CI")),as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_CI) - log(lower_CI))/3.92) %>%
  mutate(author = ifelse(grepl("Heart",author),"HPS",author),
         author = ifelse(grepl("FDA",author),"JUPITER",author)) %>%
  mutate(number_exposed = as.numeric(number_exposed)) %>%
  mutate(tpos = cases_exposed,
         tneg = number_exposed - cases_exposed,
         cpos = cases_unexposed,
         cneg = number_unexposed - cases_unexposed)

dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=results_statins_rct)

rob_dat <- robvis::data_rob2[1:2,]
rob_dat$studyid <- c(10562,90003)

png("test.png", width = 800, height = 500)
forest_strata_rob(dat, rob_dat)
dev.off()




res <- metafor::rma.uni(data = dat,
                        yi, vi, 
                        slab = paste(author, year))

rct_tri_res <- data.frame(
  stringsAsFactors = FALSE,
  Type = "RCT",
  exposure = "Statin",
  outcome = "Dementia",
  studies = 2,
  bias = "Low",
  I2 = res$I2,
  predict.rma(res, transf = exp)
)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Obs lipids per SD

res <- rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  janitor::clean_names() %>%
  filter(exclude !="Y") %>%
  filter(exposure_category == "Lipids",
         exposure == "TC",
         measure == "HR",
         point_estimate != "Missing",
         is.na(sex),
         is.na(age)
  ) %>%
  rename("lower_CI" = upper_95_percent, 
         "upper_CI" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases,point_estimate,ends_with("_CI")) %>%
  mutate(across(c(n, point_estimate, ends_with("_CI")),as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_CI) - log(lower_CI))/3.92) %>%
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

obs_tri_res_lipids <- purrr::map_df(t, meta_grouped) %>%
  mutate(Type = "Observational Study") %>%
  filter(outcome %in% c("Dementia", "AD", "VaD")) %>%
  mutate(exposure = "Lipids (per 1-SD increase)") %>%
  select(Type, everything())


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Obs lipids - binary outcome

res <- rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  janitor::clean_names() %>%
  filter(exclude !="Y") %>%
  filter(exposure_category == "Hypercholesterolemia",
         exposure == "TC",
         point_estimate != "Missing",
         measure == "HR",
         is.na(sex),
         is.na(age)
  ) %>%
  rename("lower_CI" = upper_95_percent, 
         "upper_CI" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases,point_estimate,ends_with("_CI")) %>%
  mutate(across(c(n, point_estimate, ends_with("_CI")),as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_CI) - log(lower_CI))/3.92, 
         result_id = 1:n()) %>%
  arrange(author, year)


dat_rob <- rbind(robvis::data_robins,
                 robvis::data_robins[1:5,])
dat_rob$study_id <- res$study_id

t <- res %>%
  group_by(exposure, outcome) %>%
  group_split()

obs_tri_res_hyper <- purrr::map_df(t, meta_grouped) %>%
  mutate(Type = "Observational Study") %>%
  filter(outcome %in% c("Dementia", "AD", "VaD")) %>%
  mutate(exposure = "Hypercholesterolemia") %>%
  select(Type, everything())

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Mendelian Randomisation

res <- rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  janitor::clean_names() %>%
  filter(exclude !="Y") %>%
  filter(
    study_id %in% c(9740,10068),
         type == "MR") %>%
  mutate(exposure = ifelse(exposure == "TC","LDL-c",exposure)) %>%
  filter(exposure %in% c("LDL-c","HMGCR")) %>%
  rename("lower_CI" = upper_95_percent, 
         "upper_CI" = lower_95_percent, 
         "n" = number_exposed) %>%
  select(study_id, author, year, sex,exposure_category,dose_range, age, n, exposure, outcome, cases, measure,se, point_estimate,ends_with("_CI")) %>%
  mutate(across(c(n, point_estimate, ends_with("_CI")),as.numeric)) %>%
  rowwise() %>%
  clean_effects()




set.seed(0)
t <- res %>%
  mutate(bias = "Low") %>%
  group_by(exposure, outcome) %>%
  group_split()

mr_tri_res_lipids <- purrr::map_df(t, meta_grouped) %>%
  mutate(Type = "MR") %>%
  filter(outcome %in% c("Dementia", "AD", "VaD")) %>%
  mutate(exposure = case_when(exposure == "HMGCR" ~ "LDL-c via HMGCR (per 1-SD decrease)",
                              T ~ exposure)) %>%
  select(Type, everything())


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Triangulation prep

tri_data <- rbind(obs_tri_res_statins, 
                  rct_tri_res,
                  obs_tri_res_lipids,
                  obs_tri_res_hyper, 
                  mr_tri_res_lipids
) %>%
  mutate(Type = factor(Type, levels = c("RCT","Observational Study","MR"))) %>%
  arrange(Type) %>%
  mutate(pi.lb = ifelse(studies == 1,NA,pi.lb),
         pi.ub = ifelse(studies == 1,NA,pi.ub), 
         I2 = ifelse(studies == 1,"-",round(I2,2)),
         shape = ifelse(studies == 1,12,15)) %>%
  arrange(outcome,Type) %>%
  rename("Exposure" = exposure,
         "Studies" = studies,
         Bias = bias) %>%
  group_by(outcome) %>%
  group_split() 


statin_row <- data.frame(Type = "Statin",
                         Exposure = "",
                         Studies =NA,
                         Bias = "", 
                         I2 = "", 
                         pred = NA,
                         ci.lb = NA,
                         ci.ub = NA,
                         pi.lb = NA,
                         pi.ub = NA,
                         cr.lb = NA,
                         cr.ub = NA,
                         shape = NA, 
                         stringsAsFactors = F)

lipid_row <- data.frame(Type = "Lipids",
                        Exposure = "",
                        Studies =NA,
                        Bias = "", 
                        I2 = "", 
                        pred = NA,
                        ci.lb = NA,
                        ci.ub = NA,
                        pi.lb = NA,
                        pi.ub = NA,
                        cr.lb = NA,
                        cr.ub = NA,
                        shape = NA, 
                        stringsAsFactors = F)

empty_row <- data.frame(Type = "",
                        Exposure = "",
                        Studies =NA,
                        Bias = "", 
                        I2 = "", 
                        pred = NA,
                        ci.lb = NA,
                        ci.ub = NA,
                        pi.lb = NA,
                        pi.ub = NA,
                        cr.lb = NA,
                        cr.ub = NA,
                        shape = NA, 
                        stringsAsFactors = F)

tmp<-tri_data[[2]][c(1:3,8,4:7),] %>%
  select(Type, Bias, everything()) %>%
  add_row(statin_row, .before =1) %>%
  add_row(lipid_row, .before =6) %>%
  add_row(empty_row, .before =6) %>%
  mutate(Type = ifelse(!(Type %in% c("Statin","Lipids")),paste0("  ", Type),Type),
         bold_vec = ifelse((Type %in% c("Statin","Lipids")),"bold","plain"),
         Type = ifelse(Type == "  Observational Study","  Cohort",Type)) %>%
  rename("Risk of bias" = Bias, 
         "Study design" = Type)

forester_thesis(
  left_side_data = tmp[, c(1:3,5:6)],
  estimate = tmp$pred,
  ci_low = tmp$ci.lb,
  ci_high = tmp$ci.ub,
  pi_low = tmp$pi.lb,
  pi_high = tmp$pi.ub,
  file_path = "tri_Dementia.png",
  null_line_at = 1,
  stripe_colour = "white",
  x_scale_linear = F,
  display = F,
  arrows = T,
  arrow_labels = c("Protective","Harmful"),
  xlim = c(.3,3),bold_vec = tmp$bold_vec,
  point_shapes = tmp$shape, nudge_y = 0.2, height_expansion = 0.01
)

tmp<-tri_data[[1]][c(1,2,7,3:6,8),] %>%
  select(Type, Bias, everything()) %>%
  add_row(statin_row, .before =1) %>%
  add_row(lipid_row, .before =5) %>%
  add_row(empty_row, .before =5) %>%
  mutate(Type = ifelse(!(Type  %in% c("Statin","Lipids")),paste0("  ", Type),Type),
         bold_vec = ifelse((Type %in% c("Statin","Lipids")),"bold","plain"),
         Type = ifelse(Type == "  Observational Study","  Cohort",Type), 
         Exposure = ifelse(Exposure == "LDL-c","LDL-c (per 1-SD decrease)",Exposure),
         Exposure = ifelse(Exposure == "Lipids (per 1-SD increase)","Total cholesterol (per 1-SD increase)",Exposure)) %>%
  rename("Risk of bias" = Bias, 
         "Study design" = Type)

forester_thesis(
  left_side_data = tmp[, c(1:3,5:6)],
  estimate = tmp$pred,
  ci_low = tmp$ci.lb,
  ci_high = tmp$ci.ub,
  pi_low = tmp$pi.lb,
  pi_high = tmp$pi.ub,
  file_path = "tri_AD.png",
  null_line_at = 1,
  stripe_colour = "white",
  x_scale_linear = F,
  display = F,
  arrows = T,
  arrow_labels = c("Protective","Harmful"),
  xlim = c(.3,3),bold_vec = tmp$bold_vec,
  point_shapes = tmp$shape, nudge_y = 0.2, height_expansion = 0.01
)

tmp<-tri_data[[3]][c(1,2,5,3,4),] %>%
  select(Type, Bias, everything()) %>%
  add_row(statin_row, .before =1) %>%
  add_row(lipid_row, .before =5) %>%
  add_row(empty_row, .before =5) %>%
  mutate(Type = ifelse(!(Type  %in% c("Statin","Lipids")),paste0("  ", Type),Type),
         bold_vec = ifelse((Type %in% c("Statin","Lipids")),"bold","plain"),
         Type = ifelse(Type == "  Observational Study","  Cohort",Type), 
         Exposure = ifelse(Exposure == "LDL-c","LDL-c (per 1-SD decrease)",Exposure),
         Exposure = ifelse(Exposure == "Lipids (per 1-SD increase)","Total cholesterol (per 1-SD increase)",Exposure)) %>%
  rename("Risk of bias" = Bias, 
         "Study design" = Type)

forester_thesis(
  left_side_data = tmp[, c(1:3,5:6)],
  estimate = tmp$pred,
  ci_low = tmp$ci.lb,
  ci_high = tmp$ci.ub,
  pi_low = tmp$pi.lb,
  pi_high = tmp$pi.ub,
  file_path = "tri_VaD.png",
  null_line_at = 1,
  stripe_colour = "white",
  x_scale_linear = F,
  display = F,
  arrows = T,
  arrow_labels = c("Protective","Harmful"),
  xlim = c(.3,3),bold_vec = tmp$bold_vec,
  point_shapes = tmp$shape, nudge_y = 0.3, height_expansion = 0.01
)



