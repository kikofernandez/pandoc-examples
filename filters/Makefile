FORMATS=pdf
OUTPUT=output

all: $(FORMATS)

$(FORMATS):
	@mkdir -p $(OUTPUT)
	@FILTERING_MODE=NO_COMMENTS pandoc -s -f markdown -F pandoc-crossref \
	-F filters meta.yaml -o $(OUTPUT)/TEST.$@ TEST.md
	@FILTERING_MODE=WITH_COMMENTS pandoc -s -f markdown \
	-F filters pandoc-crossref -F filters meta.yaml -o $(OUTPUT)/TEST_COMMENTS.$@ TEST.md

clean:
	@rm output/*

.PHONY: all clean $(FORMATS)
