all: presentation.md.pdf

presentation.md.pdf: presentation.md
	pandoc --latex-engine=xelatex -t beamer presentation.md -o presentation.md.pdf
