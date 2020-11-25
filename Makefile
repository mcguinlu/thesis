both:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs")'
	Rscript -e 'browseURL(here::here("docs","index.html"))'

pdf:
	Rscript -e 'grateful::get_citations(grateful::scan_packages(include.Rmd = TRUE),out.dir = "bibliography")'
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux
	Rscript -e 'browseURL(here::here("docs","_main.pdf"))'
	
see-pdf:
	Rscript -e 'browseURL(here::here("docs","_main.pdf"))'

gitbook:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs")'
	Rscript -e 'browseURL(here::here("docs","index.html"))'
	
word:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::word_document2")'
	Rscript -e 'browseURL("docs/_main.docx")'

clean:
	rm -f *.log *.mtc* *.maf *.aux
	
clean-knits:
	rm -f *.docx *.html *.pdf *.log *.maf *.mtc*
	rm -R *_files
	rm -R *_cache

clean-docs:
	rm -f docs/*
	rm -R docs/*
