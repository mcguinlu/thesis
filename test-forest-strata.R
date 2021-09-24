library(metafor)

### copy BCG vaccine meta-analysis data into 'dat'
dat <- dat.bcg[6:12,]

dat_rob <- robvis::data_robins[3:9,]


dat <-
  escalc(
    measure = "RR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat,
    slab = paste(author, year, sep = ", ")
  )

dat$id <- 1:nrow(dat)
dat_rob$id <- 1:nrow(dat_rob)

forest_strata_rob(dat,dat_rob)

png("test.png", width = 800, height = 500)
forest_strata_rob(dat, dat_rob)
dev.off()