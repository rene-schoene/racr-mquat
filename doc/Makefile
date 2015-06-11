all: presentation.tex racr-mquat.pdf

presentation.tex: presentation.md
	pandoc --latex-engine=xelatex -t beamer presentation.md -o presentation.tex

racr-mquat.pdf: presentation.tex racr-mquat.tex presentation.md
	lualatex --output-directory=./tmp racr-mquat.tex
	bibtex tmp/racr-mquat.aux
	lualatex --output-directory=./tmp racr-mquat.tex
	lualatex --output-directory=./tmp racr-mquat.tex
	mv tmp/racr-mquat.pdf .
