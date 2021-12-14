# Import, sort and arrange abbreviations by alpha order

abbr <-
  dplyr::arrange(read.csv(
    here::here("data", "abbreviations", "abbreviations.csv"),
    stringsAsFactors = FALSE
  ), Acronym)

# Save it back to the file in the sorted order
write.csv(abbr, "data/abbreviations/abbreviations.csv", row.names = FALSE, quote = F)

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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Render the acknowledgements section from Rmd to Md and paste in the output of
# github_thanks()

source(here::here("R","helper.R"))

words <- read.csv(here::here("data/words/words.csv")) %>% dplyr::pull(1) %>% dplyr::last() %>% comma()

writeLines(glue::glue(
  paste(c("<!-- do not edit by hand - make changes to _acknowledgements.Rmd instead -->\n",
          readLines("front-and-back-matter/_acknowledgements.Rmd")),collapse = "\n"),
  github = github_thanks(),
  words = words,
  .open = "<<",
  .close = ">>"
),
here::here("front-and-back-matter","_acknowledgements.md"))

