DOCS=contract-agreement.md
CONTENT_ONLY=contract-agreement-content-only.md
META=meta.yaml

all: generate metadata

generate: $(DOCS)
	pandoc -s $(DOCS) -o $(DOCS:md=pdf)
	pandoc -s $(DOCS) -o $(DOCS:md=docx)
	pandoc -s $(DOCS) -o $(DOCS:md=odt)

metadata: $(CONTENT_ONLY)
	pandoc -s $(META) $(CONTENT_ONLY) -o $(CONTENT_ONLY:md=pdf)
	pandoc -s $(META) $(CONTENT_ONLY) -o $(CONTENT_ONLY:md=docx)
	pandoc -s $(META) $(CONTENT_ONLY) -o $(CONTENT_ONLY:md=odt)

clean:
	rm *.pdf *.docx *.odt

.PHONY: all clean generate
