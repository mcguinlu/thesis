pdf:
	Rscript -e 'source(here::here("R","abbreviations.R"))'
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux
	Rscript -e 'browseURL(here::here("docs","_main.pdf"))'
	
pdf-quiet:
	Rscript -e 'source(here::here("R","abbreviations.R"))'
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux

both:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs")'
	Rscript -e 'browseURL(here::here("docs","index.html"))'

gitbook:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs")'
	Rscript -e 'browseURL(here::here("docs","index.html"))'
	
see-pdf:
	Rscript -e 'browseURL(here::here("docs","_main.pdf"))'

word:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::word_document2")'

clean:
	rm -f *.log *.mtc* *.maf *.aux
	
clean-knits:
	rm -f *.docx *.tex *.html *.pdf *.log *.maf *.mtc*
	rm -R *_files
	rm -R *_cache

clean-docs:
	rm -f docs/*
	rm -R docs/*
