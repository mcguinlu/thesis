library(magrittr)

# Generates an line pandoc citation string for all packages used in the 

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
check_words <- function(fp, words = 100){
  
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
  z <- paste0(type, estimate, start, "95% CI: ", lci, to, uci, end)
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



