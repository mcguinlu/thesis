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
# Taken from https://github.com/thomasbattram/thesis
comma <- function(x){format(x, digits = 2, big.mark = ",")}

# Apply comma to all values in a table
# Taken from https://github.com/thomasbattram/thesis
tidy_nums <- function(df) 
{
  df[] <- lapply(df, comma)
  return(df)
}

# Convert nub
# Taken from https://github.com/thomasbattram/thesis
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
      out <- x
    }
  }
  return(out)
}

check_words <- function(fp, words = 100){
  
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ",fp)
  }
  
  w <- wordcountaddin::word_count(fp)
  n <- w + words
  
  message("Start:   ",format(Sys.time(),"%H:%M"),"\nCurrent: ",w,"\nNext:    ", n," <------\n")
}

pomodoro <- function(fp){
  
  # Get file path ----
    
  if (!hasArg(fp)) {
    fp <- rstudioapi::getSourceEditorContext()$path
    message("File: ",fp)
  }

  # Fail early if wrong file type
  sink <- wordcountaddin::word_count(filename = fp)
  
  # Add job
  # job_id <- rstudioapi::jobAdd("Long Pomodoro", progressUnits = 25L)   
      
  i <- 0
  
  # rstudioapi::jobAddOutput(job_id, paste0("File: ",fp))
  
  # Run Long Pomodoro
  
  while (i < 26) {
    
    w <- wordcountaddin::word_count(filename = fp)
    
    # rstudioapi::jobAddOutput(job_id, paste0(i," - Current: ",w,"\n"))
    
    message(i," - Current: ",w)
    
    i <- i +1
    
    Sys.sleep(60)
    
    # rstudioapi::jobSetProgress(job_id, i-1)
  }
  
  beepr::beep(sound = "fanfare")
  
  rstudioapi::showDialog("Pomodoro done!", paste0("Take a break . . . \n Final count: ", w))
}


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


make <- function(arg = "pdf") {
  system(paste0("make ", arg))
}

covering <- function(){
  rmarkdown::render("front-and-back-matter/_00-introduction.Rmd")
  browseURL("front-and-back-matter/_00-introduction.html")
}

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

