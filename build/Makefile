rules = pdf print

targets = $(addsuffix .pdf,$(rules))

$(targets): %.pdf: tex/%.tex .current-revision
	xelatex tex/$*.tex
	xelatex tex/$*.tex

missing-from-sample.pdf: ../format/missing-from-sample.tex
	xelatex ../format/missing-from-sample.tex


.current-revision: git-revision
	git rev-parse --short HEAD | tr -d '\n' > .current-revision
	echo -n "+" >> .current-revision
	git rev-list --count HEAD >> .current-revision

.PHONY: git-revision clean

clean:
	rm tex/*.tex || echo ""
	rm *.aux || echo ""
	rm *.pdf || echo ""
	rm *.epub || echo ""

