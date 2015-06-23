MAIN = racr-mquat.tex
BIB  = refs.bib
PDF  = racr-mquat.pdf

IMAGES = 	opt_problem.pdf     \
			opt_problem_sol.pdf \
			../gen.pdf          \
			../sol.pdf

.PHONY: all clean

all: $(PDF) presentation.md Makefile

presentation.tex: presentation.md
	pandoc --latex-engine=xelatex -t beamer presentation.md -o presentation.tex

$(PDF): presentation.tex $(MAIN) $(BIB) $(IMAGES)
	lualatex $(MAIN)
	bibtex racr-mquat
	lualatex $(MAIN)
	lualatex $(MAIN)

clean:
	rm -f *~
	rm -f *.{aux,log,bbl,blg}
	rm -f $(PDF)
