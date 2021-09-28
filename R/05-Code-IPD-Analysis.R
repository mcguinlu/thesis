#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# primaryFigures ----

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



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- dataExcluded-table

dataExcluded_table <- read.csv(here::here("data/ipd/dataExcluded.csv"), stringsAsFactors = F) %>%
  arrange(Cohort)

if (doc_type == "docx") {
  apply_flextable(dataExcluded_table, caption = "(ref:dataExcluded-caption)")
} else{
  knitr::kable(
    dataExcluded_table,
    format = "latex",
    caption = "(ref:dataExcluded-caption)",
    caption.short = "(ref:dataExcluded-scaption)",
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortFlowchartSetup

library(DiagrammeR)

graph <- grViz(diagram = "digraph cfa {
      # define node aesthetics
      node [fontname = Arial,fixedsize = true,width = 3.5, height = 1.5,color = 'black',shape = rectangle, fontcolor = black] 
      
      tab0 [label = <<font> <b>Systematic review</b>  </font>>, height = 1, color = 'grey90',shape = rectangle, style = filled, fontcolor = black]
      tab1 [label = <<font> <b>Systematic review + DPUK </b>  </font>>, height = 1, color = 'grey90',shape = rectangle, style = filled, fontcolor = black]
      tab2 [label = <<font> <b>DPUK</b> </font>>, height = 1, color = 'grey90',shape = rectangle, style = filled, fontcolor = black]
      
      tab01 [label = 'N identified: ',height = 1]
      tab11 [label = 'N identified: ',height = 1]
      tab21 [label = 'N identified: ',height = 1]
      
      tab02 [label = 'N data obtained: \n\n Exclusion reasons:\n-No response (n= )\n-No access to data (n= )\n-Planned analysis (n= )',height = 2]
      tab12 [label = 'N data obtained: \n\n Exclusion reasons:\n-No response (n= )\n-No access to data (n= )\n-Planned analysis (n= )',height = 2]
      tab22 [label = 'N data obtained: \n\n Exclusion reasons:\n-Dementia cohort (n= )\n-No exposure variables (n= )\n-Planned analysis (n= )',height = 2]
      
      tab3 [label = 'N assessed for inclusion']
      tab4 [label = 'N excluded following investigation: \n\n Exclusion reasons:\n-No response (n= )']
      tab5 [label = 'Final number of \ncohorts included: ',height = 1]


      subgraph {
          rank = same; tab02;tab12;tab22
      }
      subgraph {
          rank = same; tab01;tab11;tab21
      }
      
      subgraph {
          rank = same; tab3;tab4
      }



# set up node layout

      tab0 -> tab01 [color = 'white', len = 1.1]
      tab1 -> tab11 [color = 'white'  ]
      tab2 -> tab21 [color = 'white'  ]

      tab01 -> tab02
      tab11 -> tab12
      tab21 -> tab22
      
      tab02 -> tab3
      tab12 -> tab3
      tab22 -> tab3
      
      tab3 -> tab4
      tab3 -> tab5

}")


htmltools::html_print(DiagrammeR::add_mathjax(graph), viewer = NULL) %>%
  webshot::webshot(file = "figures/ipd/cohortFlowchart.png", delay = 1,
                   # selector = '.html-widget-static-bound',
                   vwidth = 800,
                   vheight = 744,
                   cliprect = c(5,160, 660, 510),
                   zoom = 6)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortOverview-table

cohortOverview_table <- read.csv(here::here("data/ipd/cohortSummary.csv"), stringsAsFactors = F) %>%
  arrange(Cohort) %>%
  rename("Dementia events (all-cause)" = Dementia,
  "Age (mean)" = Age,
  "Male (%)" = Male)

if(doc_type == "docx") {
  apply_flextable(cohortOverview_table, caption = "(ref:cohortOverview-caption)")
} else{
  knitr::kable(
    cohortOverview_table,
    format = "latex",
    caption = "(ref:cohortOverview-caption)",
    caption.short = "(ref:cohortOverview-scaption)",
    align = c("lcccc"),
    booktabs = TRUE
  ) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    column_spec(1, width = paste0(7, "em")) %>%
    column_spec(2:5, width = paste0(6, "em"))
    
}


