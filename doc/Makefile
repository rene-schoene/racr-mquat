MAIN  = racr-mquat.tex
BIB   = refs.bib
PDF   = racr-mquat.pdf
LATEX = lualatex --synctex=1

IMAGES = 	opt_problem.pdf     \
			opt_problem_sol.pdf \
			gen_sol_arch.pdf    \
			gen.pdf             \
			sol.pdf             \
			comp_sol.pdf

CROPPED = $(IMAGES:%.pdf=images/$(basename %.pdf).pdf)

.PHONY: all clean quick images

all: $(PDF) presentation.md

images: $(CROPPED)
	cp -a images/ /home/rschoene/Dokumente/HAEC/HAEC-ENASA-B01/Poster/output15

presentation.tex: presentation.md
	pandoc --latex-engine=xelatex -t beamer presentation.md -o presentation.tex

$(PDF): presentation.tex $(MAIN) $(BIB) $(CROPPED)
	$(LATEX) $(MAIN)
#	lualatex $(MAIN)
	bibtex racr-mquat
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

quick: presentation.tex $(MAIN) $(BIB) $(CROPPED)
	$(LATEX) $(MAIN)

images/gen.pdf: gen.pdf
	pdfcrop --margins='0 0 100 0' $< $@

images/sol.pdf: sol.pdf
	pdfcrop --margins='0 0 100 0' $< $@

images/%.pdf: %.pdf
	pdfcrop $< $@

clean:
	rm -f *~
	rm -f *.{aux,log,bbl,blg}
	rm -f $(PDF)
