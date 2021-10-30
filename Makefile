RULES := pdf print
CONTENT := frontmatter fundamentals restrictions computing appendices new
IMAGES := $(addprefix build/,$(wildcard images/*.png))
DESIGN_IMAGES := $(addprefix build/,$(wildcard .design-tools/*.png))

PANDOC_OPTS := --highlight-style theme/highlighting.theme \
               --syntax-definition=theme/haskell.xml \
               --filter design-tools-exe \
               -F pandoc-crossref \
               -F pandoc-citeproc \
               --from markdown+fancy_lists \
               -s \
               --top-level-division=part

               # --bibliography=prose/bib.bib \

PANDOC_PDF_OPTS := --template format/tex/template.tex \
                   -t latex

$(RULES): %: build/%.pdf
sample: build/sample.pdf
epub: build/epub.epub
all: $(RULES) sample epub
lp: epub pdf sample

build/images:
	mkdir build/images

build/.design-tools: $(prose)
	mkdir build/.design-tools || echo ""
	# pandoc $(PANDOC_OPTS) $(PANDOC_PDF_OPTS) -o /tmp/blah $(filter %.markdown,$(prose))
	# cp .design-tools/*.png build/.design-tools

# $(IMAGES): build/images/%.png: images/%.png build/images
# 	cp $(filter %.png,$^) $@

targets = $(addsuffix .pdf,$(addprefix build/,$(RULES)))
$(targets): build/%.pdf: build/tex/%.tex
	make -C build $*.pdf

build/missing-from-sample.pdf:
	make -C build missing-from-sample.pdf

sources = $(addsuffix .tex,$(addprefix build/tex/,$(RULES)))
prose = $(addsuffix /*.markdown,$(addprefix prose/,$(CONTENT)))
$(sources): build/tex/%.tex: prose/metadata.markdown prose/%.markdown $(prose) format/tex/template.tex theme/* format/tex/cover.pdf
	pandoc $(PANDOC_OPTS) $(PANDOC_PDF_OPTS) -o $@ $(filter %.markdown,$^)
	# cp .design-tools/*.png build/.design-tools
	sed -i 's/\CommentTok{{-}{-} ! \([0-9]\)}/annotate{\1}/g' $@
	sed -i 's/\CommentTok{{-}{-} .via \([^}]\+\)}/reducevia{\1}/g' $@
	sed -i 's/\(\\KeywordTok{law} \\StringTok\){"\([^"]\+\)"}/\1{\\lawname{\2}}/g' $@

build/epub.epub: build/%.epub: prose/metadata.markdown prose/%.markdown $(prose) theme/* prose/bib.bib $(IMAGES) format/epub.css
	pandoc $(PANDOC_OPTS) --epub-embed-font=Katibeh.ttf -t epub -o $@ $(filter %.markdown,$^)

.PHONY: clean clean-images very-clean all $(RULES) epub lp sketches

sketches:
	./scripts/sync-httw.sh

clean:
	make -C build clean

clean-images:
	grep -l .png .design-tools/* | xargs rm
	rm .design-tools/*.png

very-clean: clean
	rm -r .design-tools
