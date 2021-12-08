# Load key libraries
library(magrittr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(metafor)
library("png")
library(robvis)
library(medrxivr)
library(kableExtra)
library(data.table)
library(flextable)
library(sf)

# Sort out conflicts in function names
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("last", "data.table")

# Register fonts with R
library(grDevices)

extrafont::loadfonts(device = "win")
grDevices::windowsFonts("Fira Sans" = grDevices::windowsFont("Fira Sans"))

# Load forest plotting functions
source(here::here("R/forester.R"))
source(here::here("R/forest flexi.R"))

# Make sure plotting device is closed
try(dev.off())

# Set options

options(kableExtra.auto_format = FALSE,
        scipen = 999,
        knitr.kable.NA = '')

knitr::opts_chunk$set(echo = TRUE)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# General ----

# Clean output of metafor using sensible defaults
broom_ma <- function(metafor_obj, exp = TRUE) {
  return(
    broom::tidy(
      metafor_obj,
      conf.int = TRUE,
      include_studies = TRUE,
      exponentiate = exp
    ) %>%
      rename("Study" = term) %>%
      mutate(
        Study = ifelse(Study == "overall", "Overall", Study),
        Study = stringr::str_replace(Study, c(".1$", ".2$", ".3$"), c("a", "b", "c"))
      )
  )
}


# Generates an line pandoc citation string for all packages used in the thesis
gen_rmd_citation <- function(filename = "packages.bib") {
  # Read in bib file
  pkgs <- readLines(here::here("bibliography", filename))
  
  # Extract bibtex keys to a string
  pkg_names <- unique(pkgs[which(grepl("@", pkgs))])
  
  pkg_names <- unique(gsub("\\{|,", "", stringr::str_extract(pkg_names, "\\{.+")))
  
  # Read in list of packages that I need to have a citation for, but shouldn't
  # go in here, and exclude these from the vector
  
  # Format string as pandoc citation
  pkg_cit <- paste0("[@", paste(pkg_names, collapse = "; @"), "]")
  
  return(pkg_cit)
  
}

# Place comma at thousand position
# Copied from https://github.com/thomasbattram/thesis
comma <- function(x) {
  format(x, digits = 2, big.mark = ",")
}

comma_tight <- function(x) {
  format(x, digits = 2, big.mark = ",") %>%
    stringr::str_squish() %>%
    return()
  
}

# Apply comma to all values in a table
# Copied from https://github.com/thomasbattram/thesis
tidy_nums <- function(df)
{
  df[] <- lapply(df, comma)
  return(df)
}

# Convert nub
# Copied from https://github.com/thomasbattram/thesis
num_to_text <- function(x, start_of_sentence = FALSE)
{
  if (!x %% 1 == 0)
    warning("X is not an integer")
  if (start_of_sentence) {
    out <- xfun::numbers_to_words(x)
    out <- stringr::str_to_sentence(out)
  } else {
    if (x < 11 & x > -1) {
      out <- xfun::numbers_to_words(x)
    } else {
      out <- comma(x)
    }
  }
  return(out)
}


# Get word count for a given markdown, and set new target
word_check <- function(fp, words = 100) {
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ", fp)
  }
  
  w <- wordcountaddin::word_count(fp)
  n <- w + words
  
  message("Start:   ",
          format(Sys.time(), "%H:%M"),
          "\nCurrent: ",
          w,
          "\nNext:    ",
          n,
          " <------\n")
}


# Create nice estimate with consistent handling across thesis
estimate <-
  function(estimate,
           lci,
           uci,
           type = "OR",
           sep = ",",
           to = "-",
           exp = F) {
    if (!hasArg(estimate)) {
      stop("Estimate missing")
    }
    
    if (!hasArg(lci)) {
      stop("LCI missing")
    }
    
    if (!hasArg(uci)) {
      stop("UCI missing")
    }
    
    if (exp == TRUE) {
      estimate <- exp(estimate)
      lci <- exp(lci)
      uci <- exp(uci)
    }
    
    if (type != "") {
      type <- paste0(type, ": ")
    }
    
    if (sep == ",") {
      start <- ", "
      end <- ""
    } else {
      start <- " ("
      end <- ")"
    }
    
    if (estimate > uci | estimate < lci) {
      stop("Estimate outside CI bounds")
    }
    
    if (lci > uci) {
      stop("Lower CI is greater than upper CI")
    }
    
    estimate <- stringr::str_trim(sprintf("%7.2f", estimate))
    lci <- stringr::str_trim(sprintf("%7.2f", lci))
    uci <- stringr::str_trim(sprintf("%7.2f", uci))
    z <- paste0(type, estimate, start, "95%CI: ", lci, to, uci, end)
    return(z)
  }

# Run make commands from the console
make <- function(arg = "pdf") {
  system(paste0("make ", arg))
}

# Generate word count report
covering <- function() {
  rmarkdown::render("scripts_and_filters/covering.Rmd")
  browseURL("scripts_and_filters/covering.html")
}


# Render PDF and Word versions, clean repo, then push changes to GitHub
end_of_day <- function(words = NULL) {
  # Make PDF and Word, and clean
  system("make pdf-quiet")
  system("make word")
  system("make clean")
  system("make clean-knits")
  
  gert::git_add(files = ".")
  gert::git_commit(paste0("End of day: ", words))
  gert::git_push()
}

#' Function that returns some text
#' @description Used to insert a inline placeholder function when writing a
#'   paragraph, to be replaced at a later date with results.
hold <- function() {
  "**PLACEHOLDER**"
}

#' Run todor for active document, or specify file path
#'
#' @param fp File path (optional)

todo <- function(fp,...) {
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ", fp)
  }
  
  todor::todor_file(fp,...)
  
}

todo_report <- function(fp){
  
  if (!hasArg(fp)) {
    text <- todor::todor(output = "markdown")
    
  } else {
    text <- todor::todor_file(fp,output = "markdown")
  }
  
  
  
  tmp <- tempfile()
  
  writeLines(text,paste0(tmp,".Rmd"))
  
  rmarkdown::render(paste0(tmp,".Rmd"))
  
  browseURL(paste0(tmp,".html"))
}




#' Open all markdown and R file associated with a given chapter
#'
#' @param N Chapter number

chapter_edit <- function(N, all = TRUE) {
  
  # If character, search front and back matter
  if (is.character(N)) {
    
    chapters <- list.files(path = "front-and-back-matter/",pattern = ".Rmd",full.names = T)

  } else{
    
   chapters <- list.files(pattern = ".Rmd")
  
  }
  
  # Open Chapter RMarkdown file
  chapter <- chapters[which(data.table::like(chapters, N))]
  
  file.edit(chapter)
  
  if (all) {
    # Open associated R file
  rfiles <-
    list.files(path = "R",
               pattern = ".R",
               full.names = TRUE)
  
  file.edit(rfiles[which(data.table::like(rfiles, N))])
  }
  
  # Put focus back on RMarkdown
  rstudioapi::navigateToFile(chapter)
  
}


# Create mindmap of chapter to help with layout of chapter subsections
mindmap <- function(fp) {
  if (!hasArg(fp)) {
    fp <-
      R.utils::getRelativePath(rstudioapi::getSourceEditorContext()$path)
    message("File: ", fp)
  }
  
  input <-
    mindr::outline(fp, remove_curly_bracket = TRUE, savefile = FALSE)
  
  mindr::mm(from = input,
            type = "text",
            root = " ")
  
}

# Get list of people contribution to thesis R packages
github_thanks <-
  function(packages = c("mcguinlu/medrxivr", "mcguinlu/robvis")) {
    authors <-
      data.frame(authors = unlist(purrr::map(
        packages, ~ usethis::use_tidy_thanks(.x, to = "2021-12-01")
      )),
      stringsAsFactors = FALSE) %>%
      dplyr::filter(authors != "mcguinlu") %>%
      dplyr::distinct() %>%
      dplyr::arrange(authors) %>%
      dplyr::mutate(authors = glue::glue("[&#x0040;{authors}](https://github.com/{authors})"))
    
    text <-
      glue::glue_collapse(authors$authors, sep = ", ", last = ", and ") + glue::glue(".")
    
    return(text)
  }

# Convienence function for making relatively nice word tables for review
apply_flextable <- function(data, caption = NULL) {
  replace_newline <- function(x) {
    x <- gsub(pattern = "\\\\newline",
              replacement = "",
              x = x)
    
    return(x)
  }
  
  data[] <- lapply(data, replace_newline)
  
  ft <- flextable::flextable(data)  %>%
    flextable::bg(bg = "#A6A6A6", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::bold(j = 1, part = "body") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left") %>%
    flextable::bg(
      i = ~ seq(from = 1, to = nrow(data)) %% 2 == 0,
      bg = "#DDDDDD",
      part = "body"
    ) %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_table_properties(layout = "autofit")
  
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }
  
  return(ft)
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# CPRD ----

create_title_row <-
  function(top_title = "Any dementia", first_col, title) {
    if (title == top_title) {
      temp <- data.frame(
        tmp = title,
        HR = NA,
        ci_lower = NA,
        ci_upper = NA,
        N_sub = NA,
        N_fail = NA
      )
    } else{
      temp <- data.frame(
        tmp = c(" ", title),
        HR = c(NA, NA),
        ci_lower = c(NA, NA),
        ci_upper = c(NA, NA),
        N_sub = c(NA, NA),
        N_fail = c(NA, NA)
      )
    }
    
    colnames(temp)[1] <- first_col
    
    return(temp)
    
  }

generate_forester_plot <-
  function(results,
           filepath,
           top_title = "Any dementia",
           first_col = "drug",
           outcome_levels = c("Any dementia",
                              "Probable AD",
                              "Possible AD",
                              "Vascular dementia",
                              "Other dementia"),
           xlimits = c(0.3, 3),
           display = FALSE,
           ...) {
    results$outcome <-
      factor(results$outcome,
             levels = outcome_levels)
    
    results$drug <-
      forcats::fct_rev(factor(
        results$drug,
        levels = c(
          "Any",
          "Statins",
          "Omega-3 Fatty Acid Groups",
          "Fibrates",
          "Ezetimibe",
          "Bile acid sequestrants"
        )
      ))
    
    results <- results[order(results$outcome, results[first_col]),]
    
    results <- results %>%
      group_by(outcome) %>%
      arrange(desc(drug), .by_group = T) %>%
      select(all_of(first_col),
             drug,
             HR,
             ci_lower,
             ci_upper,
             N_sub,
             N_fail,
             outcome) %>%
      ungroup()
    
    if (first_col == "drug") {
      results <-
        mutate(results,
               drug = ifelse(drug == "Any", "Any drug class", as.character(drug)))
    }
    
    levels <- levels(results$outcome)
    
    subset <-
      lapply(levels, function(level) {
        dplyr::filter(results, !!as.symbol("outcome") == level)
      })
    names(subset) <- levels
    
    subset_tables <-
      lapply(levels, function(level) {
        rbind(
          create_title_row(top_title, first_col, as.character(level)),
          dplyr::select(
            subset[[level]],
            all_of(first_col),
            .data$HR,
            .data$ci_lower,
            .data$ci_upper,
            .data$N_sub,
            .data$N_fail
          )
        )
      })
    
    
    subset_table <-
      do.call("rbind", lapply(subset_tables, function(x)
        x))
    
    colnames(subset_table)[1] <- "analysis"
    
    subset_table$analysis <- as.character(subset_table$analysis)
    
    subset_table$analysis <-
      ifelse(
        !(subset_table$analysis %in% levels),
        paste0("   ", subset_table$analysis),
        subset_table$analysis
      )
    
    colnames(subset_table)[1] <- "Analysis"
    colnames(subset_table)[5] <- " Participants"
    colnames(subset_table)[6] <- " Events"
    
    bold_vec <-
      ifelse(stringr::str_trim(as.vector(subset_table$Analysis)) %in% c(levels),
             "bold.italic",
             "plain")
    
    subset_table$` Participants` <-
      ifelse(
        !is.na(subset_table$` Participants`),
        paste0(" ", comma(subset_table$` Participants`)),
        subset_table$` Participants`
      )
    
    subset_table$` Events` <- ifelse(!is.na(subset_table$` Events`),
                                     paste0(" ", comma(subset_table$` Events`), "   "),
                                     subset_table$` Events`)
    
    subset_table$point_shape <-
      ifelse(subset_table$Analysis == "   Any drug class",
             16,
             16)
    
    if ("   Any drug class" %in% subset_table$Analysis) {
      point_colour <-
        ifelse(subset_table$Analysis == "   Any drug class",
               "black",
               "grey50")
    } else {
      point_colour <- rep("black", nrow(subset_table))
    }
    
    
    xbreaks <- c(xlimits[1], 1, xlimits[2])
    
    suppressMessages(
      forester_thesis(
        left_side_data = subset_table[, c(1, 5, 6)],
        estimate = subset_table$HR,
        ci_low = subset_table$ci_lower,
        ci_high = subset_table$ci_upper,
        display = display,
        estimate_precision = 2,
        estimate_col_name = "Hazard ratio",
        x_scale_linear = FALSE,
        xlim = xlimits,
        xbreaks = xbreaks,
        file_path = here::here(filepath),
        point_sizes = rep(1.5, times = nrow(subset_table)),
        stripe_colour = "white",
        font_family = "Fira Sans",
        null_line_at = 1,
        arrows = TRUE,
        arrow_labels = c("Lower risk on drug", "Higher risk on drug"),
        point_shapes = subset_table$point_shape,
        bold_vec = bold_vec,
        colour_vec = point_colour,
        ...
      )
    )
}




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Appendix ----

# Used to load multiple sheets for the tables in the appendices
read_excel_allsheets <-
  function(filename,
           tibble = FALSE,
           col_names = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    
    sheets <- readxl::excel_sheets(filename)
    
    
    
    x <-
      lapply(sheets, function(X)
        readxl::read_excel(filename, sheet = X, col_names = col_names) %>% janitor::clean_names())
    if (!tibble)
      x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Systematic review ----

# Generate yi/sei from reported data
clean_effects <- function(data) {
  data <- data %>%
    mutate(
      yi = case_when(measure == "beta" ~ point_estimate,
                     T ~ log(point_estimate)),
      sei = case_when(measure == "beta" ~ se,
                      T ~ (log(upper_ci) - log(lower_ci)) / 3.92)
    )
  
  return(data)
}


# Combine data on two groups to get summary age/SD, given group sizes
combine_age <- function(age, group_size) {
  age <- stringr::str_remove_all(age, " ")
  group_size <- stringr::str_remove_all(group_size, " ")
  
  # Find parentheses
  re <- "\\(([^()]+)\\)"
  
  # Extract sd
  sd <- c(stringr::str_extract_all(age, re, simplify = T))
  sd <- as.numeric(substring(sd, 2, nchar(sd) - 1))
  
  mean <- c(stringr::str_split(age, "\\|", simplify = T))
  mean <- as.numeric(stringr::str_remove_all(mean, re))
  
  n <-
    as.numeric(stringr::str_split(group_size, "\\|", simplify = T))
  
  t <- utilities::sample.decomp(
    n = n,
    sample.mean = mean,
    sample.sd = sd,
    include.sd = TRUE
  ) %>%
    slice_tail() %>%
    tidy_nums()
  
  return(paste0(t$sample.mean, " (", t$sample.sd, ")"))
  
}


# Combine data on two groups to get summary female %, given group sizes
combine_female <- function(female, group_size) {
  age <- stringr::str_remove_all(female, " ")
  age <- stringr::str_remove_all(group_size, " ")
  
  percentages <-
    as.numeric(stringr::str_split(female, "\\|", simplify = T))
  
  n <-
    as.numeric(stringr::str_split(group_size, "\\|", simplify = T))
  
  t <- data.frame(percentages, n) %>%
    mutate(proportion = percentages / 100,
           n_female = n * proportion) %>%
    summarise(female = sum(n_female),
              percentage = female / sum(n) * 100) %>%
    slice_tail() %>%
    tidy_nums()
  
  return(t$percentage)
  
}

# Convert estimate and p value to estimate and SE
get_confidence_from_p <- function(est, p) {
  # From https://www.bmj.com/content/343/bmj.d2090
  
  z = -0.862 + sqrt(0.743 - 2.404 * log(p))
  
  log_est = log(est)
  
  log_SE = abs(log_est / z)
  
  list(lower = exp(log_est - 1.96 * log_SE),
       upper = exp(log_est + 1.96 * log_SE)) %>%
    tidy_nums() %>%
    return()
  
  
}

n_effect <- function(x, N = TRUE){
  
  if (N) {
  return(paste0("N = ",x$n,"; ",x$estimate))
  } else {
  return(paste0(x$estimate))
  }
}

get_study_n <- function(data){
  
  if (is.grouped_df(data)) {
    data <- data %>%
      ungroup()
  }
  
  data %>%
    mutate(study = stringr::str_remove(result_id,"-.+")) %>%
    distinct(study) %>%
    nrow() %>%
    return()
}

get_all_citations <- function() {
  
  citations <-
    rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                which = 1) %>%
    janitor::clean_names() %>%
    filter(!is.na(citation)) %>%
    mutate(type = factor(subtype, levels = c("RCT", "NRSI", "NRSE", "MR"))) %>%
    arrange(type, author) %>%
    select(citation) %>%
    distinct() %>%
    pull(citation)
  
  
  paste0("[@",paste0(citations,collapse = "; @"),"]") %>%
    return()
  
  
}

get_citations_per_analysis <- function(data){
  
  if (is.grouped_df(data)) {
    data <- data %>%
      ungroup()
  }
  
  citations <-
    rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                which = 1) %>%
    janitor::clean_names() %>%
    filter(!is.na(citation)) %>%
    select(study_id, citation) %>%
    group_by(study_id) %>%
    distinct()
  
  
  citation_vec <- data %>%
    mutate(study = as.numeric(stringr::str_remove(result_id,"-.+"))) %>%
    distinct(study,.keep_all = T) %>%
    left_join(citations, by = c("study"="study_id")) %>%
    mutate(citation = paste0("@",citation)) %>%
    pull(citation)
  
  citation_vec<- citation_vec[which(citation_vec != "@NA")]
  
  paste0("[",paste0(citation_vec,collapse = "; "),"]") %>%
    return()
}


# Generate nice looking forest plots
save_fp <- function(dat, design = "obs", preface = NULL, ...) {
  # Don't perform meta-analysis if only one result
  
  if (nrow(dat) < 2) {
    return()
  }
  
  dat_rob <-
    rio::import(here::here("data/sys-rev/data_extraction_main.xlsx"),
                which = 3) %>%
    janitor::clean_names() %>%
    filter(result_id %in% dat$result_id)
  
  
  height <- max(nrow(dat) * 100, 700)
  
  if (design == "obs") {
    tool <- "ROBINS-I"
    dat_rob <-
      select(dat_rob,-c(type,result, summary_of_biases, comments))
  } else {
    tool <- "ROB2"
    dat_rob <-
      select(dat_rob,-c(type,d6, d7, result, summary_of_biases, comments))
    
  }
  
  if (!is.null(preface)) {
    path_preface <- preface
  } else {
    path_preface <- dat$exposure[1]
  }
  
  
  fp <-
    stringr::str_remove_all(paste0("fp_", design, "_", path_preface, "_", dat$outcome[1], ".png"), 
                            " ")
  
  png(
    here::here("figures", "sys-rev", fp),
    width = 1750,
    height = height,
    pointsize = 15,
    res = 100
  )
  
  forest_strata_rob(dat, dat_rob, rob_tool = tool, sei = sei, ...)
  
  
  dev.off()
}


meta_grouped <- function(data) {
  # Don't perform meta-analysis if only one result
  
  if (nrow(data) == 1) {
    return()
  }
  
  t <- metafor::rma(
    data = data,
    yi = yi,
    sei = sei,
    slab = paste(study_id, author, year)
  )
  
  
  estimate(res$beta, res$ci.lb, res$ci.ub, exp = T)
  
  details <- data.frame(
    stringsAsFactors = FALSE,
    exposure = data$exposure[1],
    outcome = data$outcome[1],
    studies = length(unique(data$study_id)),
    bias = data$bias[1],
    I2 = t$I2
  )
  
  results <- predict.rma(t, transf = exp)
  
  return(cbind(details, results))
  
}

# Get nicely formatted random effects estimate
meta_estimate <- function(dat, ...) {
  
  t <- metafor::rma(data = dat,
                    yi = yi,
                    sei = sei,
                    method = "DL")
  
  dat_n <- dat %>%
    mutate(result_id = stringr::str_remove(result_id,"-.+")) %>%
    distinct(result_id) %>%
    nrow()
  
  citations <- get_citations_per_analysis(dat)
  
  return(list(
    n = dat_n,
    estimate =  estimate(t$beta, t$ci.lb, t$ci.ub, exp = T, ...),
    citations = citations
  ))
}

# General filters applied to imported data
general_filters <- function(data) {
  data %>%
    janitor::clean_names() %>%
    filter(
      exclude != "Y",
      study_id != 99999,
      point_estimate != "Missing",
      !is.na(point_estimate)
    ) %>%
    return()
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Word counting ----

# Count words in tables
table_words <- function() {
  files <- list.files(here::here("data/table_words"))
  
  get_words_from_table <- function(fp) {
    t <- readLines(here::here("data/table_words", fp))
    sum(stringr::str_count(t, "\\w+"))
    
  }
  return(sum(as.numeric(
    purrr::map_chr(files, get_words_from_table)
  )))
}

# Count words in chapters
get_words <- function() {
  files <- list.files(path = here::here(), pattern = "\\.Rmd$")
  
  files <-
    files[which(!(files %in% c("_main.Rmd",
                               "index.Rmd",
                               "tmp.Rmd")))]
  
  words_v <- c()
  
  for (file in 1:length(files)) {
    tmp <- wordcountaddin::word_count(here::here(files[file]))
    words_v <- c(words_v, tmp)
  }
  
  table_words <- table_words()
  
  words <- sum(words_v, table_words)
  
  date <- as.character(format(Sys.time(), "%Y%m%d"))
  time <- as.character(format(Sys.time(), "%H%M"))
  
  tmpWords <- data.frame(words = words,
                         date = date,
                         time = time)
  
  masterWords <- read.csv(here::here("data", "words", "words.csv"),
                          header = TRUE)
  
  masterWords <- rbind(masterWords,
                       tmpWords) %>%
    dplyr::group_by(date) %>%
    slice(which.max(time)) %>%
    dplyr::ungroup()
  
  return(masterWords)
  
}


save_dr <- function(dat, title, xref, preface){
  
  if (length(unique(dat$study_id)) < 3) {
    return()
  }
  
  if (title == "Low-density lipoprotein cholesterol") {
    x_adjust <- 125
  } else {
    x_adjust <- 115
  }

  
  fp <- paste0("dr_",preface,".png")
  
  png(here::here("figures/sys-rev/",fp))
  knots <-
    round(with(dat, quantile(dose, probs = c(.25, .5, .75))), 2)
  
  spl <-
    dosresmeta::dosresmeta(
      formula = loghr ~ rms::rcs(dose, knots),
      id = study_id,
      se = se,
      type = type,
      cases = cases,
      n = n,
      data = dat
    )
  
  newdata = data.frame(dose = seq(0, xref+150, 10))
  
    with(predict(spl, newdata, xref = xref), {
    plot(
      get("rms::rcs(dose, knots)dose"),
      pred,
      type = "l",
      ylab = "Log HR",
      ylog = F,
      ylim = c(-1, 1),
      las = 1,
      xlab = paste0(title,", mg/dl"),
      bty = "l",
    )
    lines(get("rms::rcs(dose, knots)dose"), ci.lb, lty = "dashed")
    lines(get("rms::rcs(dose, knots)dose"), ci.ub, lty = "dashed")
  })
    legend(x = "topright",          # Position
           legend = c("Estimate", "95% CI",""),  # Legend texts
           lty = c(1, 2,2),           # Line types
           col = c("black", "black","transparent"),           # Line colors
           lwd = 2)
    text(xref+x_adjust, .81, paste0("N studies = ",length(unique(dat$study_id))))
    dev.off()
}

# Rapid navigation via bookmarks

bm_set <- function() {
  
  context <- rstudioapi::getSourceEditorContext()
  
  rstudio_bookmark <- list(start = rstudioapi::primary_selection(context)[["range"]]$start,
                            path = context[["path"]],
                            id = context[["id"]])
  
  options(rstudio_bookmark = rstudio_bookmark)
}

bm_go <- function(){
  
  bookmark <- getOption("rstudio_bookmark")
  
  if (is.null(bookmark)) {
    stop("No bookmark defined!")
    
  }
  
  rstudioapi::navigateToFile(bookmark[["path"]])
  
  rstudioapi::setCursorPosition(bookmark[["start"]],
                                bookmark[["id"]])
  
  rstudioapi::setSelectionRanges(c(bookmark[["start"]][1],
                                   1,
                                   bookmark[["start"]][1],
                                   bookmark[["start"]][2]),
                                 bookmark[["id"]])
  
}


get_max_domain <- function(data) {
  
  data %>%
    janitor::clean_names() %>%
    select(starts_with("d")) %>%
    colnames() %>%
    stringr::str_extract("[0-9].*") %>%
    as.numeric() %>%
    max() %>%
    return()
  
}  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Triangulation Functions

source(here::here("R/forest tri.R"))

rob_to_long <- function(data){
  
  data %>%
    # Clean and covert to long format
    tidyr::pivot_longer(
      matches("d[0-9]+(d|j|t)"),
      names_to = c("domain", ".value"),
      names_pattern = "(d[0-9]+)(d|j|t)"
    ) %>%
    mutate(across(c(j, d, t), stringr::str_to_lower)) %>%
    return()
}

append_values_bias <- function(data, values, common = T) {
  
  # Define criteria on which to join with values#
  # If common = T, means that the values of Serious/Moderate are consistent across domains
  if (common == T) {
    by = c("j")
    
    if ("domain" %in% colnames(values)) {
      values <- select(values, -domain)
    }
    
  } else {
    by = c("domain","j")
  }
  
  data %>%
    # Convert to long format
    rob_to_long() %>%
    
    # Add basic values
    left_join(values, by = by) %>%
    
    # Set values to 0 when judgement is NA
    # Most common when one tool has less domains than another
    mutate(across(matches("bias_._add|bias_._prop"),
                  ~ case_when(is.na(j) == T ~ 0,
                              j == "NA" ~ 0,
                              T ~ .))) %>%
    
    # Set additive biases mean/var to 0 when type is "prop"
    mutate(across(matches("bias_._add"),
                  ~ case_when(t == "prop" ~ 0,
                              T ~ .))) %>%
    
    # Set proportional biases mean/var to 0 when type is "add"
    mutate(across(matches("bias_._prop"),
                  ~ case_when(t == "add" ~ 0,
                              T ~ .))) %>%
    
    # Adjust signs of bias based on d (direction)
    # Where d is unpredictable, set mean to 0, but keep variance estimate
    mutate(
      bias_m_add = case_when(d == "left" ~ bias_m_add * -1,
                             d == "unpredictable" ~ 0,
                             T ~ bias_m_add),
      bias_m_prop = case_when(d == "left" ~ bias_m_prop * -1,
                              d == "unpredictable" ~ 0,
                              T ~ bias_m_prop)
    ) %>%
    
    return()
}  


append_values_indirect <- function(data, values, common = T) {
  
  # Define criteria on which to join with values#
  # If common = T, means that the values of Serious/Moderate are consistent across domains
  if (common == T) {
    by = c("j")
    
    if ("domain" %in% colnames(values)) {
      values <- select(values, -domain)
    }
    
  } else {
    by = c("domain","j")
  }
  
  data %>%
    # Convert to long format
    rob_to_long() %>%
    
    # Add basic values
    left_join(values, by = by) %>%
    
    # Set values to 0 when judgement is NA
    # Most common when one tool has less domains than another
    mutate(across(matches("ind_._add|ind_._prop"),
                  ~ case_when(is.na(j) == T ~ 0,
                              j == "NA" ~ 0,
                              T ~ .))) %>%
    
    # Set additive biases mean/var to 0 when type is "prop"
    mutate(across(matches("ind_._add"),
                  ~ case_when(t == "prop" ~ 0,
                              T ~ .))) %>%
    
    # Set proportional biases mean/var to 0 when type is "add"
    mutate(across(matches("ind_._prop"),
                  ~ case_when(t == "add" ~ 0,
                              T ~ .))) %>%
    
    # Adjust signs of bias based on d (direction)
    # Where d is unpredictable, set mean to 0, but keep variance estimate
    mutate(
      ind_m_add = case_when(d == "left" ~ ind_m_add * -1,
                            d == "unpredictable" ~ 0,
                            T ~ ind_m_add),
      ind_m_prop = case_when(d == "left" ~ ind_m_prop * -1,
                             d == "unpredictable" ~ 0,
                             T ~ ind_m_prop)
    ) %>%
    
    return()
} 


calculate_adjusted_estimates <- function(data) {
  data %>%
    mutate(
      yi_adj = (yi - addimn - propimn * addemn) / (propimn * propemn),
      vi_adj = ((((propimn ^ 2) + propivar) * (propevar * (yi_adj ^
                                                             2) + addevar) + propivar * ((propemn * yi_adj + addemn) ^ 2) + addivar + vi
      ) / ((propimn * propemn) ^ 2))
    ) %>%
    return()
  
}

prep_tri_data <- function(dat_rob, dat_ind, bias_values, indirect_values){
  
  dat_rob_long <- dat_rob %>%
    append_values_bias(bias_values) 
  
  dat_ind_long <- dat_ind %>%
    append_values_indirect(indirect_values)
  
  i_add <- dat_rob_long %>%
    group_by(result_id) %>%
    summarise(addimn = sum(bias_m_add, na.rm = T),
              addivar = sum(bias_v_add, na.rm = T)) %>%
    select(result_id, starts_with("add"))
  
  e_add <- dat_ind_long %>%
    group_by(result_id) %>%
    summarise(addemn = sum(ind_m_add, na.rm = T),
              addevar = sum(ind_v_add, na.rm = T)) %>%
    select(result_id, starts_with("add"))
  
  i_prop <- dat_rob_long %>%
    group_by(result_id) %>%
    summarise(sumlogmn = sum(bias_m_prop, na.rm = T), 
              sumlogvr = sum(bias_v_prop, na.rm = T), 
              propimn = exp(sumlogmn+sumlogvr/2),
              propivar = (exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1))) %>%
    select(result_id, starts_with("prop"))
  
  e_prop <- dat_ind_long %>%
    group_by(result_id) %>%
    summarise(sumlogmn = sum(ind_m_prop, na.rm = T), 
              sumlogvr = sum(ind_v_prop, na.rm = T), 
              propemn = exp(sumlogmn+sumlogvr/2),
              propevar = (exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1))) %>%
    select(result_id, starts_with("prop"))
  
  dat_final <- dat_rob %>%
    select(result_id,author,year,yi,vi) %>%
    left_join(i_add) %>%
    left_join(i_prop)%>%
    left_join(e_add) %>%
    left_join(e_prop) %>%
    calculate_adjusted_estimates() %>%
    return()
  
}

### Helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (",
                    # " Q = ", .(formatC(res$QE, digits=2, format="f")),
                    # ", df = ", .(res$k - res$p),
                    "p ", .(metafor:::.pval(res$pval, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}



annotate_poly <- function(yi, ci.lb, ci.ub, atransf = exp, textpos = 2, width, rows, cex=1.2){
  
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
  text(x=textpos, rows, labels=annotext, pos=2, cex=cex)
  
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

add_subgroup <- function(res, row){
  
  addpoly(
    res,
    fonts = 4,
    row = row,
    cex = 1,
    textpos = -4,
    atransf = exp,
    annotate = F,
    mlab = mlabfun("RE Model for lipid fraction", res)
  )
  
  annotate_poly(res["b"],
                res["ci.lb"],
                res["ci.ub"],
                rows = row,
                textpos = 2.25,
                cex = 1)
}

rma_flexi <- function(x) {
  rma(
    yi,
    sei = sei,
    subset = (term == x),
    data = main_effects,
    method = "FE"
  )
}
