forest_strata_rob <- function(dat, dat_rob, rob_tool = "ROB2"){

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

dat_rob <- dat_rob %>%
  mutate(Overall = factor(Overall ,levels =c("Low","Moderate","Serious","Critical"))) %>%
  arrange(Overall)

dat <- left_join(dat, dat_rob) %>%
  arrange(Overall)

dat_rob_vec <- dat_rob %>%
  mutate(row_n = 1:n()) %>%
  group_by(Overall) %>%
  summarise(n=n(),max = max(row_n), min = min(row_n)) %>%
  mutate(offset = seq(1,length(unique(.$Overall))*3,by=3)) %>%
  
  mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
  mutate(min = ifelse(n==1,min-1,min),
         max = ifelse(n==1,max-1,max),
         heading = ifelse(n==1,heading-1,heading))

rows <- c()

for (i in 1:nrow(dat_rob_vec)) {
  
  rows <-c(rows, dat_rob_vec$min[i]:dat_rob_vec$max[i])
  
}


x_min = -12
x_max = 4.6
textpos <- c(x_min, x_max-1)
y_max <- max(rows)+4

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Deal with adding rob data

max_domain_column <- ifelse(rob_tool == "ROB2",7,9)

rob_data <-
  cbind(dat_rob[, 1], data.frame(lapply(dat_rob[, 2:max_domain_column], robvis:::clean_data),
                                  stringsAsFactors = F))


x_pos <- seq(x_max, by = 0.55, length.out = max_domain_column - 2)

x_overall_pos <- max(x_pos) + 1

# Convenience vector, specifying x-axis positions for all risk of bias columns
header_row <- c(x_pos, x_overall_pos)

legend_pos <- x_max+(max(header_row)-min(header_row))/2

# New right-hand x-axis limit
new_x_lim <- x_overall_pos + .5

rob_colours <- robvis:::get_colour(rob_tool, "cochrane")

if (rob_tool %in% c("ROB2", "QUADAS-2")) {
  judgements<-   c("High risk of bias",
                   "Some concerns",
                   "Low risk of bias",
                   "No information")
  
  cols <- c(
    h = rob_colours$high_colour,
    s = rob_colours$concerns_colour,
    l = rob_colours$low_colour,
    n = rob_colours$ni_colour,
    x = rob_colours$na_colour
  )
  
  syms <- c(h = "X",
            s = "-",
            l = "+",
            n = "?",
            x = ""
  )
  
  shapes <- c(h = 25,
            s = 19,
            l = 15,
            n = 19,
            x = 19
  )
}


if (rob_tool == "ROBINS-I") {
  judgements<-   c("Critical risk of bias",
                   "Serious risk of bias",
                   "Moderate risk of bias",
                   "Low risk of bias",
                   "No information")
  cols <- c(
    c = rob_colours$critical_colour,
    s = rob_colours$high_colour,
    m = rob_colours$concerns_colour,
    l = rob_colours$low_colour,
    n = rob_colours$ni_colour,
    x = rob_colours$na_colour
  )
  
  syms <- c(c = "-",
            s = "\U2190",
            m = "\U2192",
            l = "-",
            n = "",
            x = "")
  
  shapes <- c(c = 15,
              s = 15,
              m = 15,
              l = 15,
              n = 15,
              x = 15)
  
}

rob_psize = 3
tsize <- rob_psize * 0.3

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Make forest plot

### fit random-effects model
res <- rma(yi, vi, data=dat, slab = paste(author, ",", year))

### indent study names
res$slab <- paste0("  ", res$slab)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(x_min, new_x_lim), at=log(c(0.05, 0.25, 1, 4)), atransf=exp,
       cex=0.75, ylim=c(-1.5, y_max), rows=rows,textpos = textpos,
       mlab=mlabfun("RE Model for All Studies", res),
       psize=1, header="Author(s) and Year", addpred = T)

### set font expansion factor (as in forest() above) and use a bold font
op <- par(font=2)

### switch to bold italic font
par(font=2)

### add text for the subgroups
for (i in 1:nrow(dat_rob_vec)) {
  
text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$Overall[i], cex = 0.75)
}

### set par back to the original settings
par(op)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Add risk of bias data

headers <- if(rob_tool == "ROB2"){
  c("D1", "D2", "D3", "D4", "D5", "O")}else{
    c("D1", "D2", "D3", "D4", "D5","D6","D7", "O")
  }
par(font = 2)
# Need to add handling of top here
graphics::text(mean(header_row), y_max, labels = "Risk of Bias", cex=0.75)
graphics::text(header_row, y_max-2 + 1, labels = headers, cex=0.75)


# Plot domain points
for (j in 1:length(x_pos)) {
  graphics::points(
    x = rep(x_pos[j], length(rows)),
    y = rows,
    pch = shapes[rob_data[[paste0("D", j)]]],
    col = alpha(cols[rob_data[[paste0("D", j)]]],0.6),
    cex = rob_psize
  )
  graphics::text(x_pos[j], rows, syms[rob_data[[paste0("D", j)]]], cex = tsize)
}


graphics::points(
  rep(x_overall_pos, length(rows)),
  rows,
  pch = 15,
  col = alpha(cols[rob_data[["Overall"]]],0.6),
  cex = rob_psize
)
graphics::text(x_overall_pos, rows, syms[rob_data[["Overall"]]], cex = tsize)
par(op)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Add sub-group, summary polygons & text

rma_flexi <- function(x) {
  rma(yi, vi, subset = (Overall == x), data = dat)
}

res <- purrr::map(dat_rob_vec$Overall, rma_flexi)


### add summary polygons for the three subgroups
for (i in 1:nrow(dat_rob_vec)) {
  
  if (length(res[[i]]$slab) == 1) {
    next
  }
  
  addpoly(
    res[[i]],
    fonts = 4,
    row = dat_rob_vec$stats[i],
    cex = 0.75,
    textpos=textpos,
    atransf = exp,
    annotate = F,
    mlab = mlabfun("RE Model for Subgroup", res[[i]])
  )
  
  annotate_poly(res[[i]]$b,
                res[[i]]$ci.lb,
                res[[i]]$ci.ub,
                textpos = textpos,
                rows = dat_rob_vec$stats[[i]])
  
}



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ Overall, data = dat)

### add text for the test of subgroup differences
text(x_min,-1.9, pos = 4, cex = 0.75, bquote(
  paste(
    "Test for Subgroup Differences: ",
    Q[M],
    " = ",
    .(formatC(
      res$QM, digits = 2, format = "f"
    )),
    ", df = ",
    .(res$p - 1),
    ", p = ",
    .(formatC(
      res$QMp, digits = 2, format = "f"
    ))
  )
))



graphics::legend(
  legend_pos,
  -1,
  judgements,
  pch = 15,
  xjust = 0.5,
  col = head(cols,-1),
  xpd = TRUE,
  title = parse(text = "bold(\"Judgement\")"),
  title.adj = 0.1,
  cex = .7,
  pt.cex = .7,
  y.intersp = 0.7
)


}


### Helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}



annotate_poly <- function(yi, ci.lb, ci.ub, atransf = exp, textpos = 2, width, rows){
  
  if (is.function(atransf)) {

    annotext <- cbind(sapply(yi, atransf), sapply(ci.lb, atransf), sapply(ci.ub, atransf))
    ### make sure order of intervals is always increasing
    
    tmp <- .psort(annotext[,2:3])
    annotext[,2:3] <- tmp
    
  } else {
    
    annotext <- cbind(yi, ci.lb, ci.ub)
    
  }
  
  annotext <- .fcf(annotext, 2)
  
  if (missing(width) || is.null(width)) {
    width <- apply(annotext, 2, function(x) max(nchar(x)))
  } else {
    if (length(width) == 1L)
      width <- rep(width, ncol(annotext))
  }
  
  for (j in seq_len(ncol(annotext))) {
    annotext[,j] <- formatC(annotext[,j], width=width[j])
  }
  
  annotext <- cbind(annotext[,1], " [", annotext[,2], ", ", annotext[,3], "]")
  annotext <- apply(annotext, 1, paste, collapse="")
  text(x=textpos[2], rows, labels=annotext, pos=2, cex=0.75)

}


.fcf <- function(x, digits) {
  
  if (all(is.na(x))) { # since formatC(NA, format="f", digits=2) fails
    x
  } else {
    trimws(formatC(x, format="f", digits=digits))
  }
  
}

.psort <- function(x,y) {
  
  ### t(apply(xy, 1, sort)) would be okay, but problematic if there are NAs;
  ### either they are removed completely (na.last=NA) or they are always put
  ### first/last (na.last=FALSE/TRUE); but we just want to leave the NAs in
  ### their position!
  
  if (is.null(x) || length(x) == 0L) ### need to catch this
    return(NULL)
  
  if (missing(y)) {
    if (is.matrix(x)) {
      xy <- x
    } else {
      xy <- rbind(x) ### in case x is just a vector
    }
  } else {
    xy <- cbind(x,y)
  }
  
  n <- nrow(xy)
  
  for (i in seq_len(n)) {
    if (anyNA(xy[i,]))
      next
    xy[i,] <- sort(xy[i,])
  }
  
  colnames(xy) <- NULL
  
  return(xy)
  
}
