both:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs/5abd0cd3e96bf9459398f146495c15129ee56682")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs/5abd0cd3e96bf9459398f146495c15129ee56682")'

pdf:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", output_dir = "docs/5abd0cd3e96bf9459398f146495c15129ee56682")'
	rm -f *.log *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front-and-back-matter/abbreviations.aux

gitbook:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook", output_dir = "docs/5abd0cd3e96bf9459398f146495c15129ee56682")'

word:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::word_document2")'
	Rscript -e 'browseURL("docs/_main.docx")'

clean:
	rm -f *.log *.mtc* *.maf *.aux
	
clean-knits:
	rm -f *.docx *.html *.pdf *.log *.maf *.mtc*
	rm -R *_files
	rm -R *_cache

