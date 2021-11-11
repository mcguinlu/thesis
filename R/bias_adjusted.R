add_rob_columns <- function(df,
                            n,
                            low = c(0, 0),
                            mod = c(0.1, 0.2),
                            ser = c(0.2, 0.2)) {
  
  stopifnot(n %in% c(5, 7))
  
  v <- c()
  
  for (i in 1:n) {
    v <- c(v,
           paste0("d", i, "mean"),
           paste0("d", i, "var"))
  }
  
  df[, v] <- 0
  
  
  df <- df %>%
    mutate(
      d1mean = case_when(
        grepl("moderate|some concerns", d1) ~ mod[1],
        grepl("serious|high", d1) ~ ser[1],
        T ~ 0
      ),
      d2mean = case_when(
        grepl("moderate|some concerns", d2) ~ mod[1],
        grepl("serious|high", d2) ~ ser[1],
        T ~ 0
      ),
      d3mean = case_when(
        grepl("moderate|some concerns", d3) ~ mod[1],
        grepl("serious|high", d3) ~ ser[1],
        T ~ 0
      ),
      d4mean = case_when(
        grepl("moderate|some concerns", d4) ~ mod[1],
        grepl("serious|high", d4) ~ ser[1],
        T ~ 0
      ),
      d5mean = case_when(
        grepl("moderate|some concerns", d5) ~ mod[1],
        grepl("serious|high", d5) ~ ser[1],
        T ~ 0
      )
    ) %>%
    mutate(
      d1var = case_when(
        grepl("moderate|some concerns", d1) ~ mod[2],
        grepl("serious|high", d1) ~ ser[2],
        T ~ 0
      ),
      d2var = case_when(
        grepl("moderate|some concerns", d2) ~ mod[2],
        grepl("serious|high", d2) ~ ser[2],
        T ~ 0
      ),
      d3var = case_when(
        grepl("moderate|some concerns", d3) ~ mod[2],
        grepl("serious|high", d3) ~ ser[2],
        T ~ 0
      ),
      d4var = case_when(
        grepl("moderate|some concerns", d4) ~ mod[2],
        grepl("serious|high", d4) ~ ser[2],
        T ~ 0
      ),
      d5var = case_when(
        grepl("moderate|some concerns", d5) ~ mod[2],
        grepl("serious|high", d5) ~ ser[2],
        T ~ 0
      )
    )
  
  if (n == 7) {
    df <- df %>%
      mutate(
        d6mean = case_when(
          grepl("moderate|some concerns", d6) ~ mod[1],
          grepl("serious|high", d6) ~ ser[1],
          T ~ 0
        ),
        d7mean = case_when(
          grepl("moderate|some concerns", d7) ~ mod[1],
          grepl("serious|high", d7) ~ ser[1],
          T ~ 0
        )
      ) %>%
      mutate(
        d6var = case_when(
          grepl("moderate|some concerns", d6) ~ mod[2],
          grepl("serious|high", d6) ~ ser[2],
          T ~ 0
        ),
        d7var = case_when(
          grepl("moderate|some concerns", d7) ~ mod[2],
          grepl("serious|high", d7) ~ ser[2],
          T ~ 0
        )
      )
  }
  
}

assign_direction <- function(df, null){
  
  df <- df %>%
    mutate(d1mean = ifelse(grepl("tn", d1) & yi < null, d1mean*-1, d1mean),
           d1mean = ifelse(grepl("fc", d1), d1mean*-1, d1mean)) %>%
    mutate(d2mean = ifelse(grepl("tn", d2) & yi < null, d2mean*-1, d2mean),
           d2mean = ifelse(grepl("fc", d2), d2mean*-1, d2mean)) %>%
    mutate(d3mean = ifelse(grepl("tn", d3) & yi < null, d3mean*-1, d3mean),
           d3mean = ifelse(grepl("fc", d3), d3mean*-1, d3mean)) %>%
    mutate(d4mean = ifelse(grepl("tn", d4) & yi < null, d4mean*-1, d4mean),
           d4mean = ifelse(grepl("fc", d4), d4mean*-1, d4mean)) %>%
    mutate(d5mean = ifelse(grepl("tn", d5) & yi < null, d5mean*-1, d5mean),
           d5mean = ifelse(grepl("fc", d5), d5mean*-1, d5mean))

    
    if (ncol(df) > 20) {
      
      df <- df %>%
        mutate(d6mean = ifelse(grepl("tn", d6) & yi < null, d6mean*-1, d6mean),
               d6mean = ifelse(grepl("fc", d6), d6mean*-1, d6mean)) %>%
        mutate(d7mean = ifelse(grepl("tn", d7) & yi < null, d7mean*-1, d7mean),
               d7mean = ifelse(grepl("fc", d7), d7mean*-1, d7mean))
    }
  
  return(df)
  
}

calc_fully_adjusted_estimates <- function(df) {
  df <- df %>%
    # estadjall=(estlogor-addimn-propimn*addemn)/(propimn*propemn)
    mutate(yi_adj = yi - add)
}



ex_rob <- read.csv("turner_bias/example_rob.csv",
                   stringsAsFactors = F) %>%
  janitor::clean_names() %>%
  # Create fake estimate data
  mutate(yi = c(0.9,1,1.2,3)) %>%
  mutate_all(stringr::str_to_lower) %>%
  # Add rob
  add_rob_columns(7) %>%
  assign_direction(1) %>%
  
  select(study,yi, order(colnames(.)), -overall) %>%
  mutate(biasaddmean = rowSums(select(., ends_with("mean"))),
         biasaddvar = rowSums(select(., ends_with("var")))) %>%
  
  
  View()




split_judgements <- function(df, n){
  df <- df %>%
  tidyr::separate(d1,c("d1j","d1d")," - ") %>%
  tidyr::separate(d2,c("d2j","d2d")," - ")%>%
  tidyr::separate(d3,c("d3j","d3d")," - ")%>%
  tidyr::separate(d4,c("d4j","d4d")," - ")%>%
  tidyr::separate(d5,c("d5j","d5d")," - ")
  
  
  if (n == 7) {
    df <- df %>%
    tidyr::separate(d6,c("d6j","d6d")," - ") %>%
    tidyr::separate(d7,c("d7j","d7d")," - ")
  }
  
  df[is.na(df)] <- "none"
  
  return(df)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Get risk of bias data
dat_obs_sta <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(
    exposure_category == "Drug",
    measure == "HR",
    exposure == "Statin - Ever",
    outcome %in% c("AD"),
    primary == 1
  ) %>%
  mutate(author = case_when(!is.na(sex) ~ paste0(author, " (", sex, " only)"),
                            T ~ author)) %>%
  rename("n" = number_exposed) %>%
    mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  select(result_id, type, yi, sei)

dat_mr_lipids <-
  rio::import("data/sys-rev/data_extraction_main.xlsx", which = 2) %>%
  general_filters() %>%
  filter(type == "MR",
         exposure == "LDL-c",
         # Just pick one study
         study_id == 2439,
         
         exposure_category != "Drug") %>%
  rename(n = number_exposed) %>%
  mutate(across(c(n, point_estimate, se, ends_with("_ci")), as.numeric)) %>%
  rowwise() %>%
  clean_effects() %>%
  select(result_id, type, yi, sei)


dat_obs_lipids <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  mutate(exposure_category = ifelse(exposure_category == "Lipid", "Lipids", exposure_category)) %>%
  mutate(author = ifelse(author == "Tynkkynen", paste0(author, " (", cohort, ")"), author)) %>%
  filter(
    is.na(mr_type),
    is.na(age),
    is.na(sex),
    is.na(direction),
    !grepl("Critical", comments),
    exposure_category %in% c("Lipids", "Lipid"),
    exposure %in% c("LDL-c"),
    outcome %in% c("AD"),
    point_estimate != "Missing"
  ) %>%
  rename("n" = number_exposed) %>%
    mutate(across(c(n, point_estimate, ends_with("_ci")), as.numeric)) %>%
  mutate(yi = log(point_estimate),
         sei = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  arrange(author, year) %>%
  select(result_id, type, yi, sei)
  


ids <- rbind(dat_obs_lipids,dat_obs_sta, dat_mr_lipids)

dat_rob <- rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"), which = 3) %>%
  janitor::clean_names() %>%
  select(-c(type, summary_of_biases, comments, result))

final <- left_join(ids, dat_rob, by = c("result_id" = "result_id")) %>%
  rename_at(vars(starts_with('d')), funs(paste0(.,"j"))) 


dat <- read.csv("turner_bias/real_example_rob.csv",stringsAsFactors = F) %>%
  left_join(
    rio::import(
      here::here("data/sys-rev/data_extraction_main.xlsx"),
      which = 2
    ) %>% janitor::clean_names() %>%
      select(result_id, author, year)
  )

dat <- dat %>%
  select(result_id, author, type, yi, sei, everything())

png(
  here::here("triangulation_example_LDL_AD.png"),
  width = 1750,
  height = 1400,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat, sei = dat$sei, title = "LDL-c and AD")

dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Validate with Turner example
turner <- rio::import("turner_bias/propbias.dta") %>%
  arrange(study, assessor) %>%
  left_join(rio::import("turner_bias/addbias.dta")) %>%
  filter(assessor == 1) %>%
  select(-assessor) %>%
  mutate(estadjall = (estlogor-addimn-propimn*addemn)/(propimn*propemn),
         varadjall=(((propimn^2)+propivar)*(propevar*(estadjall^2)+addevar) + propivar*((propemn*estadjall+addemn)^2) + addivar + varlogor)/((propimn*propemn)^2),
         selogor=sqrt(varlogor),
         seadjall=sqrt(varadjall)) %>%
  select(study, contains("logor"), contains("adjall"))
  
  model <- metafor::rma.uni(yi = estadjall,
               sei = seadjall,
               data = turner)
  metafor::forest.default(model, showweights = TRUE)
  
  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

get_max_domain <- function(data) {
  
    data %>%
    janitor::clean_names() %>%
    select(starts_with("d")) %>%
    colnames() %>%
    stringr::str_extract("[0-9].*") %>%
    as.numeric() %>%
    max() %>%
    return()
  
}  


  


