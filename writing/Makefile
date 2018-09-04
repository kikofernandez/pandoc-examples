all: intro

intro:
	pandoc -s -f markdown -t docx+grid_tables -o introduction.docx introduction.md

clean:
	rm *.docx

.PHONY: all clean intro
