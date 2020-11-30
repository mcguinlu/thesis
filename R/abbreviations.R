abbr <-
  dplyr::arrange(read.csv(
    here::here("data", "abbreviations", "abbreviations.csv"),
    stringsAsFactors = FALSE
  ), Acronym)

acron <- abbr$Acronym
def <- abbr$Definition

abbr_text <- c(
  "% do not edit by hand - add to abbreviations.csv instead",
  "% First parameter can be changed eg to \"Glossary\" or something.",
  "% Second parameter is the max length of bold terms.",
  "\\begin{mclistof}{List of Abbreviations}{3.2cm}",
  unlist(lapply(1:length(acron), function(i) {
    glue::glue("\\item[{acron[i]}] {def[i]}")
  })),
  "\\end{mclistof}"
)

writeLines(abbr_text, here::here("front-and-back-matter","abbreviations.tex"))
