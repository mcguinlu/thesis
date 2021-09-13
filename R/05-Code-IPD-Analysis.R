tmp <-
  read.csv(
    "data/ipd/ipd_age_sex.csv",
    strip.white = T,
    stringsAsFactors = F,
    header = T
  ) %>%
  filter(cohort != "MEMENTO") %>%
  nest_by(lipid)

library(metafor)

forest_save <- function(data, lipid) {
  png(paste0("figures/ipd/", lipid, ".png"),
      width = 700,
      height = 220)
  
  # metafor::rma(yi = estimate, vi = se, data = data, slab = cohort, method = "DL", measure = "OR") %>%
  #   forest(showweights = T, refline = 1, annotate = T, header = c(lipid,"Estimate [95%CI]"), transf=exp)
  
  meta::metagen(
    TE = estimate,
    seTE = se,
    studlab = cohort,
    data = data
  ) %>% meta::forest(overall = FALSE,
                     xlab = paste("LogOR per 1-SD change in", lipid))
  
  dev.off()
}

purrr::map2(.x = tmp$data, .y = tmp$lipid, ~ forest_save(.x, .y))
