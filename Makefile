paper: paper/paper.pdf

paper/paper.pdf: paper/main.tex paper/reference.bib
	(cd paper; make)

clean-paper:
	(cd paper; make clean)

