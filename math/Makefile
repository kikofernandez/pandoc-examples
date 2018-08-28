all: math

math:
	pandoc math.tex -s --mathml  -o mathMathML.html

clean: mathMathML.html
	rm mathMathML.html

.PHONY: math
