library(metafor)

dat <- rio::import(here::here("iv_example.csv")) %>%
  janitor::clean_names() %>%
  mutate(result_id = 1:n(), 
         year = "")

dat_rob <-
  rio::import(here::here("iv_example_rob.csv")) %>%
  janitor::clean_names() %>%
  mutate(result_id = 1:n()) %>%
  rename("author" = study)

dat <-
  escalc(
    measure = "OR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat
  )

res <- metafor::rma(yi, vi, dat = dat)

try(dev.off(), silent = T)

png(
  here::here("iv_example.png"),
  pointsize = 15,
  width = 1750,
  height = 1000,
  res = 100
)

forest_strata_rob(dat,
                  dat_rob,
                  at = log(c(0.1, 1,5)),
                  rob_me = "No info",
                  xlab = "Odds ratio")
dev.off()
