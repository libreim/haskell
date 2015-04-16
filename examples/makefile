%.pdf: %.lhs
	pandoc --to latex --from markdown+lhs $< -o $@

%.out: %.lhs
	ghc --make $< -o $@
