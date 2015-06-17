MAIN = racr-mquat.tex
BIB  = refs.bib
PDF  = racr-mquat.pdf

.PHONY: all clean

all: $(PDF) Makefile

presentation.tex: presentation.md
	pandoc --latex-engine=xelatex -t beamer presentation.md -o presentation.tex

$(PDF): presentation.tex $(MAIN) $(BIB)
	lualatex $(MAIN)
	bibtex racr-mquat
	lualatex $(MAIN)
	lualatex $(MAIN)

clean:
	rm -f *~
	rm -f *.{aux,log,bbl,blg}
	rm -f $(PDF)
