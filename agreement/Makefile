DOCS=contract-agreement.md

all: $(DOCS)
	pandoc -s $(DOCS) -o contract-agreement.pdf
	pandoc -s $(DOCS) -o contract-agreement.docx
	pandoc -s $(DOCS) -o contract-agreement.odp

clean:
	rm contract-agreement.pdf contract-agreement.docx contract-agreement.odp
