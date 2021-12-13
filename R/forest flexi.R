forest_strata_rob <-
  function(dat,
           dat_rob,
           rob_tool = "ROB2",
           rob_me = "Low",
           sei = NULL,
           title = NULL,
           legend = TRUE,
           legend_cex = 0.9,
           ...) {
    
### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

if (rob_tool == "ROB2") {
  levels <- rev(c("Low","Some concerns","High","Critical"))
} else {
  levels <- rev(c("Low","Moderate","Serious","Critical"))
} 
  
# TODO the ordering of ROB is not quite right!
dat_rob <- dat_rob %>%
  mutate(overall = factor(overall ,levels =levels)) %>%
  arrange(overall)

if ("type" %in% colnames(dat_rob)) {
  dat_rob <- dat_rob %>%
    select(-type)
}

dat <- left_join(dat, dat_rob, by = c("result_id"= "result_id")) %>%
  arrange(overall, desc(author))

dat_rob_vec <- dat_rob %>%
  mutate(row_n = 1:n()) %>%
  group_by(overall) %>%
  summarise(n=n(),max = max(row_n), min = min(row_n)) %>%
  mutate(offset = seq(1,length(unique(.$overall))*3,by=3)) %>%
  mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
  mutate(min = ifelse(n==1,min-1,min),
         max = ifelse(n==1,max-1,max),
         heading = ifelse(n==1,heading-1,heading))

if (length(unique(dat_rob$overall))==1) {
  dat_rob_vec <- dat_rob_vec %>%
    mutate(across(c(min, max, heading),~.-1))
}

rows <- c()

for (i in 1:nrow(dat_rob_vec)) {
  
  rows <-c(rows, dat_rob_vec$min[i]:dat_rob_vec$max[i])
  
}

arg <- list(...)

if (is.null(arg$at)) {
  x_adj <- log(3)
} else {
  x_adj <- arg$at[3]
}

x_min = -10
x_max = 4.6 - log(3) + x_adj
textpos <- c(x_min, x_max-1)
y_max <- max(rows)+4

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Deal with adding rob data

max_domain_column <- ifelse(rob_tool == "ROB2",7,9)

rob_data <-
  cbind(dat_rob[, 1], data.frame(lapply(dat_rob[, 2:max_domain_column], robvis:::clean_data),
                                  stringsAsFactors = F))


x_pos <- seq(x_max, by = 0.45, length.out = max_domain_column - 2)

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
  
  shapes <- c(h = 15,
            s = 15,
            l = 15,
            n = 15,
            x = 15
  )
}

if (rob_tool == "ROBINS-I") {
  judgements<-   c("Serious risk of bias",
                   "Moderate risk of bias",
                   "Low risk of bias",
                   "No information")
  cols <- c(
    s = rob_colours$high_colour,
    m = rob_colours$concerns_colour,
    l = rob_colours$low_colour,
    n = rob_colours$ni_colour,
    x = rob_colours$na_colour
  )
  
  # syms <- c(c = "-",
  #           s = "\U2190",
  #           m = "\U2192",
  #           l = "-",
  #           n = "",
  #           x = "")
  
  syms <- c(s = "X",
            m = "-",
            l = "+",
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
if (!hasArg(sei)) {
  res <- rma(yi, vi, data=dat, slab = paste0(author, ", ", year), method = "DL")
} else {
  res <- rma(yi, sei = sei, data=dat, slab = paste0(author, ", ", year), method = "DL")
}

### indent study names
res$slab <- paste0("  ", res$slab)

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(x_min, new_x_lim), atransf=exp,
       cex=1.2, ylim=c(-1.5, y_max), rows=rows, textpos = textpos,
       mlab=mlabfun("RE Model for all studies", res),
       header="Author(s) and Year", addpred = T,...)

### set font expansion factor (as in forest() above) and use a bold font
op <- par(font=2)

### switch to bold italic font
par(font=2)

### add text for the subgroups
for (i in 1:nrow(dat_rob_vec)) {
  
text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$overall[i], cex = 1.2)
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
graphics::text(mean(header_row), y_max, labels = "Risk of Bias", cex=1.2)
graphics::text(header_row, y_max-2 + 1, labels = headers, cex=1.2)
par(op)

# Plot domain points
for (j in 1:length(x_pos)) {
  graphics::points(
    x = rep(x_pos[j], length(rows)),
    y = rows,
    pch = shapes[rob_data[[paste0("d", j)]]],
    col = alpha(cols[rob_data[[paste0("d", j)]]],0.6),
    cex = rob_psize
  )
  graphics::text(x_pos[j], rows, syms[rob_data[[paste0("d", j)]]], cex = tsize)
}


graphics::points(
  rep(x_overall_pos, length(rows)),
  rows,
  pch = 15,
  col = alpha(cols[rob_data[["overall"]]],0.6),
  cex = rob_psize
)
graphics::text(x_overall_pos, rows, syms[rob_data[["overall"]]], cex = tsize)
par(op)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Add sub-group, summary polygons & text

if (!hasArg(sei)) {
  rma_flexi <- function(x) {
    rma(
      yi,
      vi,
      subset = (overall == x),
      data = dat,
      method = "DL"
    )
  }
} else{
  rma_flexi <- function(x) {
    rma(
      yi,
      sei = sei,
      subset = (overall == x),
      data = dat,
      method = "DL"
    )
  }
}

res <- purrr::map(dat_rob_vec$overall, rma_flexi)

if (length(unique(dat_rob$overall))>1) {
  
### add summary polygons for the three subgroups
for (i in 1:nrow(dat_rob_vec)) {
  
  if (length(res[[i]]$slab) == 1) {
    next
  }
  
  addpoly(
    res[[i]],
    fonts = 4,
    row = dat_rob_vec$stats[i],
    cex = 1.2,
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
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

if(!is.null(title)){
  par(font = 2)
  text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
  par(op)
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

if (length(unique(dat_rob$overall))>1 && nrow(dat)>9) {

# Fit meta-regression model to test for subgroup differences

  if (!hasArg(sei)) {
    
    res <- rma(yi, vi, mods = ~ overall, data = dat, method = "DL")
  } else {
    res <- rma(yi, sei=sei, mods = ~ overall, data = dat, method = "DL")
    
  }

### add text for the test of subgroup differences
text(x_min,-1.8, pos = 4, cex = 1.2, bquote(
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
}


# Add missing evidence
rob_me <- robvis:::clean_data(rob_me)

rob_me_colours <- robvis:::get_colour("ROB2", "cochrane")

rob_me_cols <- c(
  h = rob_me_colours$high_colour,
  s = rob_me_colours$concerns_colour,
  l = rob_me_colours$low_colour,
  n = rob_me_colours$ni_colour,
  x = rob_me_colours$na_colour
)

rob_me_syms <- c(h = "X",
          s = "-",
          l = "+",
          n = "?",
          x = ""
)

text(x_pos[1]-.5,-1,pos=4,cex=1.2,"ROB Missing Evidence: ")

graphics::points(
  x_overall_pos,
  -1,
  pch = 15,
  col = alpha(rob_me_cols[rob_me],0.6),
  cex = rob_psize
)
graphics::text(x_overall_pos,font = 2, -1, rob_me_syms[rob_me], cex = tsize)

if (legend == TRUE) {

graphics::legend(
  legend_pos,
  -1.8,
  judgements,
  pch = 15,
  xjust = 0.5,
  col = head(cols,-1),
  xpd = TRUE,
  title = parse(text = "bold(\"Judgement\")"),
  title.adj = 0.1,
  cex = legend_cex,
  pt.cex = legend_cex,
  y.intersp = 0.7
)
}

}
