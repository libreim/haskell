all: pdfs execs

pdfs: bintree.pdf hask.pdf fibonacci.pdf monoides.pdf quicksort.pdf
execs: bintree hask fibonacci monoides quicksort

%.pdf: %.lhs
	pandoc --to latex --from markdown+lhs $< -o $@

%: %.lhs
	ghc --make $< -o $@
