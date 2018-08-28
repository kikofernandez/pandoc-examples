all: paper

paper:
	pandoc -s -F pandoc-crossref -F pandoc-citeproc meta.yaml --data-dir=data-dir --template=mytemplate.tex -N \
	 -f markdown -t latex+raw_tex+tex_math_dollars+citations -o main.pdf main.md
