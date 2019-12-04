DOCS=README.md
META=meta.yaml
NUMBER_SECTION_HEADINGS=-N

.PHONY: all doc clean

all: doc

doc: $(DOC)
	pandoc -s $(META) $(DOCS) --listings --pdf-engine=xelatex -c assets/pandoc.css -o $(DOCS:md=pdf)
	pandoc -s $(META) $(DOCS) --self-contained -c assets/pandoc.css -o $(DOCS:md=html)

clean:
	rm $(DOCS:md=pdf) $(DOCS:md=html)
