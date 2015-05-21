all: slides purl handout

slides: vegan-slides.Rmd vegan-constrained-slides.Rmd
	Rscript -e "rmarkdown::render(\"vegan-slides.Rmd\")"
	Rscript -e "rmarkdown::render(\"vegan-constrained-slides.Rmd\")"

purl: vegan-slides.Rmd
	Rscript -e "knitr::purl(\"vegan-slides.Rmd\")"
	Rscript -e "knitr::purl(\"vegan-constrained-slides.Rmd\")"


handout: vegan-slides.pdf
	pdfnup vegan-slides.pdf --frame true --outfile handout-vegan-slides.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95
	pdfnup vegan-slides.pdf --frame true --outfile handout-vegan-constrained-slides.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95	
