gen_rmd_citation <- function(filename = "pkg-refs.bib") {

pkgs <- readLines(here::here("bibliography",filename))

pkg_names <- unique(pkgs[which(grepl("@",pkgs))])

pkg_cit <- paste0("[@", paste(unique(gsub(
  "\\{|,", "", stringr::str_extract(pkg_names, "\\{.+")
)), collapse = "; @"),"]")

return(pkg_cit)

}
