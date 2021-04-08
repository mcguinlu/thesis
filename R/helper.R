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

add_words <- function(words = 100){
  
  t <- wordcountaddin::text_stats()
  w <- as.numeric(stringr::str_extract(t[3], "[0-9]+"))
  n <- w + words
  
  message("Start:   ",format(Sys.time(),"%H:%M"),"\nNext:    ", n," <------\n")
}


check_words <- function(fp){
  
  i <- 0
  
  while (i < 26) {
    
    t <- wordcountaddin::text_stats(filename = fp)
    
    w <- as.numeric(stringr::str_extract(t[3], "[0-9]+"))
    
    message(i," - Current: ",w)
    
    i <- i +1
    
    Sys.sleep(60)
  }
  
  beepr::beep(sound = "fanfare")
  
  rstudioapi::showDialog("Pomodoro done!", "Take a break . . . ")

}
