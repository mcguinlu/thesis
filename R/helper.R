library(magrittr)
library(dplyr)
library(ggplot2)
library(patchwork)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("last", "data.table")

# Register fonts with R
library(grDevices)

extrafont::loadfonts(device = "win")
grDevices::windowsFonts("Fira Sans" = grDevices::windowsFont("Fira Sans"))

# Load forester function
source(here::here("R/forester.R"))


# Clean output of metafor using sensible defaults
broom_ma <- function(metafor_obj, exp = TRUE) {
  return(broom::tidy(
    metafor_obj,
    conf.int = TRUE,
    include_studies = TRUE,
    exponentiate = exp
  ) %>%
    rename("Study" = term) %>%
    mutate(Study = ifelse(Study == "overall", "Overall",Study),
           Study = stringr::str_replace(Study,c(".1$",".2$",".3$"),c("a","b","c"))))
}


# Generates an line pandoc citation string for all packages used in the thesis

gen_rmd_citation <- function(filename = "pkg-refs.bib") {
  
  # Read in bib file
  pkgs <- readLines(here::here("bibliography",filename))
  
  # Extract bibtex keys to a string
  pkg_names <- unique(pkgs[which(grepl("@",pkgs))])
  
  pkg_names <- unique(gsub(
    "\\{|,", "", stringr::str_extract(pkg_names, "\\{.+")
  ))
  
  # Read in list of packages that I need to have a citation for, but shouldn't
  # go in here, and exclude these from the vector
  extra_packages <- readLines(file.path("data","extra_packages.txt"))
  
  pkg_names <- pkg_names[which(!(pkg_names %in% extra_packages))]
  
  # Format string as pandoc citation
  pkg_cit <- paste0("[@", paste(pkg_names, collapse = "; @"),"]")
  
  return(pkg_cit)
  
}

# Place comma at thousand position
# Copied from https://github.com/thomasbattram/thesis
comma <- function(x){
  
  format(x, digits = 2, big.mark = ",")
  
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
  if (!x%%1 == 0) warning("X is not an integer")
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


# Get word count for a given file, and set new target
word_check <- function(fp, words = 100){
  
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ",fp)
  }
  
  w <- wordcountaddin::word_count(fp)
  n <- w + words
  
  message("Start:   ",format(Sys.time(),"%H:%M"),"\nCurrent: ",w,"\nNext:    ", n," <------\n")
}


# Create nice estimate with consistent handling across thesis
estimate <- function(estimate, lci, uci, type = "OR", sep = ",", to = "-"){
  
  if (!hasArg(estimate)) {
    stop("Estimate missing")
  }
  
  if (!hasArg(lci)) {
    stop("LCI missing")
  }
  
  if (!hasArg(uci)) {
    stop("UCI missing")
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
  
  if(estimate > uci | estimate < lci){
    stop("Estimate outside CI bounds")
  }
  
  if(lci > uci){
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
covering <- function(){
  rmarkdown::render("front-and-back-matter/_00-introduction.Rmd")
  browseURL("front-and-back-matter/_00-introduction.html")
}


# Render PDF and Word, then push changes to GitHub
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
hold <- function(){
  "**PLACEHOLDER**"
}



#' Run todor for active document, or specify file path
#'
#' @param fp File path (optional)

todo <- function(fp){
  
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ",fp)
  }
  
  todor::todor_file(fp)
  
}



#' Open all files associated with a given chapter
#'
#' @param N Chapter number

chapter_edit <- function(N){
  
  # Open Chapter RMarkdown file
  chapters <- list.files(pattern = ".Rmd")
  
  chapter <- chapters[which(data.table::like(chapters, N))]
  
  file.edit(chapter)
  
  # Open associated R file
  rfiles <- list.files(path = "R", pattern = ".R", full.names = TRUE)
  
  file.edit(rfiles[which(data.table::like(rfiles, N))])  
  
  # Put focus back on RMarkdown
  rstudioapi::navigateToFile(chapter)
  
}


# m
mindmap <- function(fp) {
  
  if (!hasArg(fp)) {
    fp <- R.utils::getRelativePath(rstudioapi::getSourceEditorContext()$path)
    message("File: ",fp)
  }
  
  input <- mindr::outline(fp, remove_curly_bracket = TRUE, savefile = FALSE)
  
  mindr::mm(
    from = input, type = "text", root = " "
  )
  
}


github_thanks <-
  function(packages = c("mcguinlu/medrxivr", "mcguinlu/robvis")) {
    
    authors <-
      data.frame(authors = unlist(purrr::map(
        packages, ~ usethis::use_tidy_thanks(.x,to = "2021-12-01")
      )), stringsAsFactors = FALSE) %>%
      dplyr::filter(authors != "mcguinlu") %>%
      dplyr::distinct() %>%
      dplyr::arrange(authors) %>%
      dplyr::mutate(authors = glue::glue("[&#x0040;{authors}](https://github.com/{authors})")) 
    
    text <-
      glue::glue_collapse(authors$authors, sep = ", ", last = ", and ") + glue::glue(".")
    
    return(text)
    
  }


apply_flextable <- function(data, caption = NULL) {
  
  replace_newline <- function(x){
    x<- gsub(pattern = "\\\\newline",replacement = "", x = x)
    
    return(x)
  }
  
  data[] <- lapply(data, replace_newline)
  
  ft <- flextable::flextable(data)  %>%
    flextable::bg(bg = "#A6A6A6", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::bold(j=1, part = "body") %>%
    flextable::align(align = "center", part = "all" ) %>%
    flextable::align(j = 1, align = "left") %>%
    flextable::bg(i = ~ seq(from = 1, to = nrow(data)) %% 2 == 0, bg = "#DDDDDD", part = "body") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_table_properties(layout = "autofit")
  
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }
  
  return(ft)
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# The next two functions are for use with the CPRD analysis

create_title_row <- function(top_title = "Any dementia", first_col, title) {
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
           outcome_levels = c(
             "Any dementia",
             "Probable AD",
             "Possible AD",
             "Vascular dementia",
             "Other dementia"
           ),
           xlimits = c(0.3,3),
           display = FALSE,
           ...) {
    
    results$outcome <-
      factor(
        results$outcome,
        levels = outcome_levels
      )
    
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
    
    results <- results[order(results$outcome, results[first_col]), ]
    
    results <- results %>%
      group_by(outcome) %>%
      arrange(desc(drug), .by_group = T) %>%
      select(all_of(first_col), drug, HR, ci_lower, ci_upper, N_sub, N_fail, outcome) %>%
      ungroup()
    
    if (first_col == "drug") {
      results <- mutate(results,drug = ifelse(drug == "Any", "Any drug class", as.character(drug)))
    }
    
    levels <- levels(results$outcome)
    
    subset <-
      lapply(levels, function(level) {
        dplyr::filter(results,!!as.symbol("outcome") == level)
      })
    names(subset) <- levels
    
    subset_tables <-
      lapply(levels, function(level) {
        rbind(
          create_title_row(top_title,first_col,as.character(level)),
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
    
    subset_table$` Events` <- ifelse(
      !is.na(subset_table$` Events`),
      paste0(" ", comma(subset_table$` Events`),"   "),
      subset_table$` Events`
    )
    
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
      point_colour <- rep("black",nrow(subset_table))
    }
    
    
    xbreaks <- c(xlimits[1],1,xlimits[2])
    
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
        arrow_labels = c("Lower risk on drug","Higher risk on drug"),
        point_shapes = subset_table$point_shape,
        bold_vec = bold_vec, 
        colour_vec = point_colour,
        ...
      )
    )
    
  }




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

read_excel_allsheets <- function(filename, tibble = FALSE, col_names = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  
  sheets <- readxl::excel_sheets(filename)
  
  
  
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = col_names) %>% janitor::clean_names())
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


combine_age <- function(age, group_size) {
  
  age <- stringr::str_remove_all(age, " ")
  age <- stringr::str_remove_all(group_size, " ")
  
  # Find parentheses
  re <- "\\(([^()]+)\\)"
  
  # Extract sd
  sd <- c(stringr::str_extract_all(age,re, simplify = T))
  sd <- as.numeric(substring(sd, 2, nchar(sd)-1))
  
  mean <- c(stringr::str_split(age,"\\|", simplify = T))
  mean <- as.numeric(stringr::str_remove_all(mean,re))
  
  n <- as.numeric(stringr::str_split(group_size,"\\|", simplify = T))
  
  t <- utilities::sample.decomp(
    n = n,
    sample.mean = mean,
    sample.sd = sd,
    include.sd = TRUE
  ) %>%
    slice_tail() %>%
    tidy_nums()
  
  return(paste0(t$sample.mean," (",t$sample.sd,")"))
  
}


combine_female <- function(female, group_size) {
  
  age <- stringr::str_remove_all(female, " ")
  age <- stringr::str_remove_all(group_size, " ")
  
  percentages <- as.numeric(stringr::str_split(female,"\\|", simplify = T))
  
  n <- as.numeric(stringr::str_split(group_size,"\\|", simplify = T))
  
  t <- data.frame(percentages,n) %>%
    mutate(proportion = percentages/100,
           n_female = n*proportion) %>%
    summarise(female = sum(n_female), 
              percentage = female/sum(n)*100) %>%
    slice_tail() %>%
    tidy_nums()
  
  return(t$percentage)
  
}
