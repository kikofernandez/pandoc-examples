all: generate

generate:
	pandoc -t revealjs -s --self-contained SLIDES -V theme=white \
	-V progress=true -V center=true -V slideNumber=true -o index.html

clean: index.html
	rm index.html

.PHONY: generate
