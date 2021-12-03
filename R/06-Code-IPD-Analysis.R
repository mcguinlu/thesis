#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- cohortNumbers 

n_applied <- list()
n_applied$DPUK <- 16
n_applied$sysrev <- 20
n_applied$DPUK_sysrev <- 1
n_applied$total <- sum(unlist(n_applied))

n_accessed <- list()
n_accessed$DPUK <- 2
n_accessed$sysrev <- 0
n_accessed$DPUK_sysrev <- 1
n_accessed$total <- sum(unlist(n_accessed))

fmt_applied <- function(dat){
  paste0("n = ",dat,", ",comma((dat/n_applied$total)*100),"%")
}

fmt_accessed <- function(dat){
  paste0("n = ",dat,", ",comma((dat/n_accessed$total)*100),"%")
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
      
      tab01 [label = 'N identified: @@1-2',height = 1]
      tab11 [label = 'N identified: @@1-3',height = 1]
      tab21 [label = 'N identified: @@1-1',height = 1]
      
      tab02 [label = 'N data obtained: 0 \n\n Exclusion reasons:\n- No response (n= 15)\n- No access to data (n= 3)\n- Planned analysis (n= 2)',height = 2]
      tab12 [label = 'N data obtained: 1', height = 2]
      tab22 [label = 'N data obtained: 8\n\n Exclusion reasons:\n- No response (n= 7)\n- Planned analysis (n= 1)',height = 2]
      
      tab3 [label = 'N data inspected: 9',height = 1]
      tab4 [label = 'N excluded following investigation: 6 \n\n For exclusion reasons,\n see Table 6.1',height = 2]
      tab5 [label = 'Final number of \ncohorts included: @@2-4',height = 1]

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

}
      [1]:n_applied
      [2]:n_accessed

")

htmltools::html_print(DiagrammeR::add_mathjax(graph), viewer = NULL) %>%
  webshot::webshot(file = "figures/ipd/cohortFlowchart.png", delay = 1,
                   # selector = '.html-widget-static-bound',
                   vwidth = 800,
                   vheight = 744,
                   cliprect = c(5,140, 700, 510),
                   zoom = 6)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- dataExcluded-table

dataExcluded_table <- read.csv(here::here("data/ipd/dataExcluded.csv"), stringsAsFactors = F) %>%
  arrange(Cohort) %T>%
  write.csv("data/table_words/dataExcluded.csv")

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
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(2:nrow(dataExcluded_table) - 1, hline_after = TRUE) %>%

    column_spec(1, width = paste0(12, "em")) %>%
    column_spec(2, width = paste0(20, "em"))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- covariateSummary-table

covariateSummary_table <- rio::import(here::here("data/ipd/toc.csv")) %>%
  rename(" " = Variable)

if (doc_type == "docx") {
  apply_flextable(covariateSummary_table, caption = "(ref:covariateSummary-caption)")
} else{
  knitr::kable(
    covariateSummary_table,
    format = "latex",
    caption = "(ref:covariateSummary-caption)",
    caption.short = "(ref:covariateSummary-scaption)",
    booktabs = TRUE, 
    align = "lccc"
  ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(2:nrow(covariateSummary_table) - 1, hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = FALSE) %>%
    kable_styling(latex_options = c("HOLD_position"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- prepIPDFigures

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Main effects, dementia
main_effects <- rio::import(here::here("data/ipd/results_main.csv")) %>%
  filter(analysis == "Model 1",
         outcome == "Dementia") %>%
  mutate(slab = paste0(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))

png(here::here("figures/ipd/main_Dementia.png"), width = 800, height = 600)

metafor::forest(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  atransf = exp,
  ref = log(1),
  xlim = c(-4, 2.25),
  header = c("Lipids"),
  rows = c(2,3,4,8,9,10,14,15,16,20,21,22),
  ylim = c(0.5, 26),
  xlab = "OR per 1-SD increase in lipid",
  at = log(c(0.3, 1, 3)),
  cex = 1
)

graphics::text(-4, 5, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 11, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 17, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 23, pos=4, "Total cholesterol", cex=1,font=2)

res <- purrr::map(levels(main_effects$term), rma_flexi)

purrr::map2(res, c(1,7,13,19), ~add_subgroup(.x,.y))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Main effects, vascular dementia
main_effects <- rio::import(here::here("data/ipd/results_main.csv")) %>%
  filter(analysis == "Model 1",
         outcome == "vas_dem") %>%
  mutate(slab = paste0(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))
 
png(here::here("figures/ipd/main_vasdem.png"), width = 800, height = 600)

metafor::forest(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  atransf = exp,
  ref = log(1),
  xlim = c(-4, 2.25),
  header = c("Lipids"),
  rows = c(2,3,7,8,12,13,17,18),
  ylim = c(0.5, 22),
  xlab = "OR per 1-SD increase in lipid",
  at = log(c(0.3, 1, 3)),
  cex = 1
)

graphics::text(-4, 4, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 9, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 14, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 19, pos=4, "Total cholesterol", cex=1,font=2)

res <- purrr::map(levels(main_effects$term), rma_flexi)

vad_res <- unlist(res[1])

ipd_vasdem <- estimate(vad_res$b,vad_res$ci.lb,vad_res$ci.ub,type = "OR",exp = T)

purrr::map2(res, c(1,6,11,16), ~add_subgroup(.x,.y))
dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Main effects, model comparison
main_effects <- rio::import(here::here("data/ipd/results_main.csv")) %>%
  filter(cohort %in% c("Whitehall","EPIC"),
    outcome == "Dementia") %>%
  mutate(slab = paste(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort), desc(analysis))

png(here::here("figures/ipd/main_model_comparison.png"), width = 800, height = 700)

metafor::forest.default(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  ilab = main_effects$analysis,
  ilab.xpos = -2,
  atransf = exp,
  xref = 1,
  xlim = c(-4, 2.25),
  header = c("Lipid/Cohort"),
  ylim = c(-1,26),
  xlab = "OR per 1-SD increase in lipid",
  rows = c(1,2,3,4,7,8,9,10,13,14,15,16,19,20,21,22),
  at = log(c(0.3, 1, 3))
)

graphics::text(-2, 26, pos=1, "Analysis", cex=1,font=2)
graphics::text(-4, 5, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 11, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 17, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 23, pos=4, "Total cholesterol", cex=1,font=2)

graphics::text(-4, -.5, pos=4, "Model 1: Age, sex, smoking, alcohol, and presence of vascular disease", cex=0.9, font =2)
graphics::text(-4, -1.1, pos=4, "Model 2: Model 1 + BMI and education", cex=0.9, font = 2)

dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Main effects, Whitehall

tyk <-
  rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
              which = 2) %>%
  general_filters() %>%
  filter(study_id == 2140, 
         cohort == "WHITEHALL") %>%
  mutate(across(c(
    point_estimate, starts_with(c("number_", "cases_")), ends_with("_ci")
  ), as.numeric)) %>%
  mutate(point = log(point_estimate),
         SE = (log(upper_ci) - log(lower_ci)) / 3.92) %>%
  select(exposure, outcome, point, SE) %>%
  mutate(cohort = "Tynkkynen et al., 2018") %>%
  mutate(exposure = case_when(exposure == "TC" ~ "Total cholesterol",
                              exposure == "TG" ~ "Triglycerides",
                              T ~ exposure)) %>%
  rename(term =exposure, yi = point, sei = SE) %>%
  select(cohort, term, outcome, yi,sei) %>%
  filter(outcome == "Dementia")

main_effects <- rio::import(here::here("data/ipd/results_main.csv")) %>%
  filter(cohort == "Whitehall",
         analysis == "Model 2",
         outcome == "Dementia") %>%
  select(-analysis) %>%
  mutate(cohort = "IPD Analysis - Model 2") %>%
  rbind(tyk) %>%
  mutate(slab = paste(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))

png(here::here("figures/ipd/whitehall_comparison_dementia.png"), width = 700, height = 600)

metafor::forest.default(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  ilab = main_effects$analysis,
  ilab.xpos = -2,
  atransf = exp,
  xref = 1,
  xlim = c(-3, 2.25),
  header = c("Lipid/Cohort"),
  ylim = c(1,22),
  xlab = "OR per 1-SD increase in lipid",
  rows = c(2,3,7,8,12,13,17,18),
  at = log(c(0.3, 1, 3))
)

graphics::text(-3, 4, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-3, 9, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-3, 14, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-3, 19, pos=4, "Total cholesterol", cex=1,font=2)

dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Interaction effects, vascular dementia, age
main_effects <- rio::import(here::here("data/ipd/results_interaction.csv")) %>%
  filter(analysis == "age",
         outcome == "vas_dem") %>%
  mutate(slab = paste0(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))

png(here::here("figures/ipd/interaction_age_vasdem.png"), width = 800, height = 600)

metafor::forest(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  atransf = exp,
  ref = log(1),
  xlim = c(-4, 2.25),
  header = c("Lipids"),
  rows = c(2,3,7,8,12,13,17,18),
  ylim = c(0.5, 22),
  xlab = "OR per step increase in age group",
  at = log(c(0.3, 1, 3)),
  cex = 1
)

graphics::text(-4, 4, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 9, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 14, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 19, pos=4, "Total cholesterol", cex=1,font=2)

res <- purrr::map(levels(main_effects$term), rma_flexi)

purrr::map2(res, c(1,6,11,16), ~add_subgroup(.x,.y))
dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Interaction effects, dementia, sex
main_effects <- rio::import(here::here("data/ipd/results_interaction.csv")) %>%
  filter(analysis == "sex",
         outcome == "Dementia") %>%
  mutate(slab = paste0(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))

png(here::here("figures/ipd/interaction_sex_dementia.png"), width = 800, height = 600)

metafor::forest(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  atransf = exp,
  ref = log(1),
  xlim = c(-4, 2.25),
  header = c("Lipids"),
  rows = c(2,3,7,8,12,13,17,18),
  ylim = c(0.5, 22),
  xlab = "OR",
  at = log(c(0.3, 1, 3)),
  cex = 1
)

graphics::text(-4, 4, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 9, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 14, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 19, pos=4, "Total cholesterol", cex=1,font=2)

res <- purrr::map(levels(main_effects$term), rma_flexi)

purrr::map2(res, c(1,6,11,16), ~add_subgroup(.x,.y))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Interaction effects, age, dementia
main_effects <- rio::import(here::here("data/ipd/results_interaction.csv")) %>%
  filter(analysis == "age",
         outcome == "Dementia") %>%
  mutate(slab = paste0(cohort)) %>%
  mutate(slab = paste0("    ", slab)) %>%
  mutate(term = factor(term, levels = c("Triglycerides","HDL-c","LDL-c","Total cholesterol"))) %>%
  arrange(term,desc(cohort))

png(here::here("figures/ipd/interaction_age_dementia.png"), width = 800, height = 600)

metafor::forest(
  x = main_effects$yi,
  sei = main_effects$sei,
  slab = main_effects$slab,
  atransf = exp,
  ref = log(1),
  xlim = c(-4, 2.25),
  header = c("Lipids"),
  rows = c(2,3,4,8,9,10,14,15,16,20,21,22),
  ylim = c(0.5, 26),
  xlab = "OR per step increase in age group",
  at = log(c(0.3, 1, 3)),
  cex = 1
)

graphics::text(-4, 5, pos=4, "Triglycerides", cex=1,font=2)
graphics::text(-4, 11, pos=4, "HDL-c", cex=1,font=2)
graphics::text(-4, 17, pos=4, "LDL-c", cex=1,font=2)
graphics::text(-4, 23, pos=4, "Total cholesterol", cex=1,font=2)

res <- purrr::map(levels(main_effects$term), rma_flexi)

purrr::map2(res, c(1,7,13,19), ~add_subgroup(.x,.y))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# ---- ipdSummStat

n_total_ipd <- comma(8208+1115+2512)

n_ipd_dementia <- 287+8+247
n_ipd_vasdem <- 37+77

