#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# INTERNAL BIAS
# Generate internal summaries

# write code to create this from spreadsheet

values <- list(
  d1 = c(
    "add_serious_mn" = 0.16,
    "add_serious_var" = 0.1,
    "add_moderate_mn" = 0.08,
    "add_moderate_var" = 0.05, 
    "prop_serious_mn" = 0.1,
    "prop_serious_var" = 0.05,
    "prop_moderate_mn" = 0.03,
    "prop_moderate_var" = 0.016
  )
)

dat <- read.csv("turner_bias/real_example_rob.csv",
                stringsAsFactors = F) %>%
  left_join(
    rio::import(
      here::here("data/sys-rev/data_extraction_main.xlsx"),
      which = 2
    ) %>% janitor::clean_names() %>%
      select(result_id, author, year)
  ) %>%
  mutate(vi = sei^2) %>%
  select(result_id, author, type, yi, vi, everything())


append_values <- function(data, values) {

  data %>%
    # Clean and covert to long format
    tidyr::pivot_longer(
      matches("d[0-9]+(d|j|t)"),
      names_to = c("domain", ".value"),
      names_pattern = "(d[0-9]+)(d|j|t)"
    ) %>%
    mutate(across(c(j, d, t), stringr::str_to_lower)) %>%
  # Additive biases
  mutate(m_add = case_when(t == "add" & j == "serious" ~ values[["d1"]][["add_serious_mn"]],
                           t == "add" & j == "moderate" ~ values[["d1"]][["add_moderate_mn"]],
                           T ~ 0),
         v_add = case_when(t == "add" & j == "serious" ~ values[["d1"]][["add_serious_var"]],
                           t == "add" & j == "moderate" ~ values[["d1"]][["add_moderate_var"]],
                           T ~ 0)) %>%
  # Proportional biases
  mutate(m_prop = case_when(t == "prop" & j == "serious" ~ values[["d1"]][["prop_serious_mn"]],
                            t == "prop" & j == "moderate" ~ values[["d1"]][["prop_moderate_mn"]],
                            T ~ 0),
         v_prop = case_when(t == "prop" & j == "serious" ~ values[["d1"]][["prop_serious_var"]],
                            t == "prop" & j == "moderate" ~ values[["d1"]][["prop_moderate_var"]],
                            T ~ 0)) %>%
  # Signs
  mutate(m_add = case_when(d == "left" ~ m_add*-1,
                           T ~ m_add), 
         m_prop = case_when(d == "left" ~ m_prop*-1,
                            T ~ m_prop)) %>%
  return()
}  

# Save bias direction plot

png(
  here::here("triangulation_example_LDL_AD.png"),
  width = 1750,
  height = 1200,
  pointsize = 15,
  res = 100
)

forest_triangulation(dat, sei = dat$sei, title = "LDL-c and AD")

dev.off()


# Calculate total internal additive and proportional bias for each study
# 
dat <- dat %>%
  append_values(values)

i_add <- dat %>%
  group_by(result_id) %>%
  summarise(addimn = sum(m_add, na.rm = T),
            addivr = sum(v_add, na.rm = T)) %>%
  select(result_id, starts_with("add"))


i_prop <- dat %>%
  group_by(result_id) %>%
  summarise(sumlogmn = sum(m_prop, na.rm = T), 
            sumlogvr = sum(v_prop, na.rm = T), 
            propimn = exp(sumlogmn+sumlogvr/2),
            propivr = (exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1))) %>%
  select(result_id, starts_with("prop"))


# Generate external summaries


# Adjust estimates


# Perform adjusted analysis