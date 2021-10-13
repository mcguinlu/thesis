options(kableExtra.auto_format = FALSE, scipen = 999,
        knitr.kable.NA = '')

library(robvis)
library(medrxivr)
library(kableExtra)
library(dplyr)
library(data.table)
library(flextable)
library(ggplot2)
library(patchwork)
library(sf)

doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')

if(doc.type=="docx"){
  br <-"&nbsp;"
  pb <- "#####"
} 

if(doc.type=="html"){
  br <- "<br>"
  pb <- ""
}

if(doc.type=="latex"){
  br <- ""
  pb <- ""
}

knitr::opts_chunk$set(echo = TRUE)