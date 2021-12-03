forest_triangulation <-
  function(dat,
           sei = NULL,
           title = NULL,
           ...) {
    
### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

levels <- c("Low","Moderate","Serious","Critical")

levels_type <- c("MR","NRSI","NRSE")
  
# TODO the ordering of ROB is not quite right!
dat <- dat %>%
  mutate(type = factor(type, levels = levels_type)) %>%
  mutate(overall = factor(overall, levels =levels)) %>%
  arrange(type, overall)

dat[is.na(dat)] <- "None"

dat_rob_vec <- dat %>%
  mutate(row_n = 1:n()) %>%
  group_by(type) %>%
  summarise(n=n(),max = max(row_n), min = min(row_n)) %>%
  mutate(offset = seq(1,length(unique(.$type))*3,by=3)) %>%
  mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
  mutate(min = ifelse(n==1,min-1,min),
         max = ifelse(n==1,max-1,max),
         heading = ifelse(n==1,heading-1,heading))

if (length(unique(dat$type))==1) {
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

if (is.null(arg$x_min)) {
  x_min = -10
} else {
  x_min <- arg$x_min
}

x_max = 4.6 - log(3) + x_adj
textpos <- c(x_min, x_max-1)
y_max <- max(rows)+4

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Deal with adding rob data

dat <- dat %>%
  mutate(across(-c(result_id,author,type,yi,sei, year), robvis:::clean_data))

# Combine direction and type
for (j in paste0("d",1:7)) {
  for (i in 1:nrow(dat)) {
    dat[i,paste0(j,"d")] <- paste0(dat[i,paste0(j,"d")],dat[i,paste0(j,"t")])
  }
}


x_pos <- seq(x_max, by = 0.45, length.out = 9 - 2)

x_overall_pos <- max(x_pos) + 1

# Convenience vector, specifying x-axis positions for all risk of bias columns
header_row <- c(x_pos, x_overall_pos)

legend_pos <- x_max+(max(header_row)-min(header_row))/2

# New right-hand x-axis limit
new_x_lim <- x_overall_pos + .5

rob_colours <- robvis:::get_colour("ROBINS-I", "cochrane")

judgements<-   c(  "Serious risk of bias",
                   "Moderate risk of bias",
                   "Low risk of bias")
cols <- c(
    s = rob_colours$high_colour,
    m = rob_colours$concerns_colour,
    l = rob_colours$low_colour,
    n = rob_colours$ni_colour,
    x = "transparent"
  )
  
syms <- c(ua = "?",
          up = "?",
          lp = "<",
          rp = ">",
          la = "\U2190",
          ra = "\U2192",
          l = "\U2190",
          r = "\U2192",
          xx = "",
          x = "")
  
  shapes <- c(c = 15,
              s = 15,
              m = 15,
              l = 15,
              n = 15,
              x = 15)


rob_psize = 3
tsize <- rob_psize * 0.3

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Make forest plot

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(x = dat$yi,
       sei = dat$sei,
       xlim=c(x_min, new_x_lim),
       atransf=exp,
       slab = paste0("  ", dat$author, " ", dat$year),
       cex=1.2,
       ylim=c(-1.5, y_max),
       rows=rows,
       textpos = textpos,
       mlab = "",
       header="Author(s) and Year",
       ...)

### set font expansion factor (as in forest() above) and use a bold font
op <- par(font=2)

### switch to bold italic font
par(font=2)

### add text for the subgroups
for (i in 1:nrow(dat_rob_vec)) {
  
text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$type[i], cex = 1.2)
}

### set par back to the original settings
par(op)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Add risk of bias data

headers <- c("D1", "D2", "D3", "D4", "D5","D6","D7", "O")

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
    pch = shapes[dat[[paste0("d", j,"j")]]],
    col = alpha(cols[dat[[paste0("d", j,"j")]]],0.6),
    cex = rob_psize
  )
  graphics::text(x_pos[j], rows, syms[dat[[paste0("d", j,"d")]]], cex = tsize)
}

graphics::points(
  rep(x_overall_pos, length(rows)),
  rows,
  pch = 15,
  col = alpha(cols[dat[["overall"]]],0.6),
  cex = rob_psize
)
# graphics::text(x_overall_pos, rows, syms[dat[["overall"]]], cex = tsize)
par(op)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Add sub-group, summary polygons & text

  rma_flexi <- function(x) {
    rma(
      yi,
      sei = sei,
      subset = (type == x),
      data = dat,
      method = "DL"
    )
  }


res <- purrr::map(dat_rob_vec$type, rma_flexi)

if (length(unique(dat$type))>1) {
  
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

# 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  
  # if (nrow(dat)>9) {
  #   
  #   # Fit meta-regression model to test for subgroup differences
  #   
  #   if (!is.null(sei)) {
  #     
  #     res <- rma(yi, vi, mods = ~ type, data = dat, method = "DL")
  #     
  #   } else {
  #     
  #     res <- rma(yi, sei=sei, mods = ~ type, data = dat, method = "DL")
  #     
  #   }
  #   
  #   ### add text for the test of subgroup differences
  #   text(x_min,-1.8, pos = 4, cex = 1.2, bquote(
  #     paste(
  #       "Test for Subgroup Differences: ",
  #       Q[M],
  #       " = ",
  #       .(formatC(
  #         res$QM, digits = 2, format = "f"
  #       )),
  #       ", df = ",
  #       .(res$p - 1),
  #       ", p = ",
  #       .(formatC(
  #         res$QMp, digits = 2, format = "f"
  #       ))
  #     )
  #   ))
  # }


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

if(!is.null(title)){
  par(font = 2)
  text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
  par(op)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

graphics::legend(
  legend_pos-1.3,
  -1.7,
  c(judgements),
  pch = c(15,15,15,16,50),
  xjust = 0.5,
  col = c(cols[1:3],"white","white"),
  xpd = TRUE,
  title = parse(text = "bold(\"Extent of bias\")"),
  title.adj = 0.05,
  cex = .8,
  pt.cex = .7,
  y.intersp = 0.7
)

graphics::legend(
  legend_pos+0.95,
  -1.7,
  c("\U2190  \U2192  Additive bias: ","  <   >   Proportional bias", "    ?     Unpredictable"),
  xjust = 0.5,
  xpd = TRUE,
  adj = 0.15,
  title = parse(text = "bold(\"Type of bias\")"),
  title.adj = 0.05,
  cex = .8,
  y.intersp = 0.7
  )

}



