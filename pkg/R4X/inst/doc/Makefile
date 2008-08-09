all: cleanBefore r4xslides.pdf user2008_abstract.pdf clean
r4xslides.pdf: 
	Rscript --vanilla -e "Sweave('r4xslides.Rnw')"
	sed "s/begin{frame}/begin{frame}[fragile]/" r4xslides.tex > tmp.tex
	sed "s/\[fragile\]\[plain\]/[plain]/" tmp.tex > r4xslides.tex
	pdflatex r4xslides.tex
	pdflatex r4xslides.tex
clean:
	rm -fr r4xslides.toc r4xslides.out r4xslides.nav r4xslides.vrb 
cleanBefore: 
	rm -fr r4xslides.pdf
	rm -fr user2008_abstract.pdf
user2008_abstract.pdf:
	Rscript --vanilla -e "Sweave('user2008_abstract.Rnw')"
	pdflatex user2008_abstract.tex
	pdflatex user2008_abstract.tex
  
