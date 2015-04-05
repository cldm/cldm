latex:
	sbcl --load make.lisp --eval '(cldm.doc:generate)' --quit

pdf: latex
	pdflatex cldm.tex
	makeindex cldm.idx
	pdflatex cldm.tex
