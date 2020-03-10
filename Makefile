targets = book sample print
langs = en fr

define TARGET_RULE =
# argument 1 is the language
# argument 2 is the target

$(1)-quick:
	xelatex -shell-escape '\newcommand{\lang}{$(1)}\input{$(1)/book.tex}' || echo ""

$(1)-$(2).pdf: $(1)-%.pdf: $(1)/%.tex
	xelatex -shell-escape '\newcommand{\lang}{$(1)}\input{$(1)/$$*.tex}' || echo ""
	makeglossaries $(1)/$$* || echo ""
	xelatex -shell-escape '\newcommand{\lang}{$(1)}\input{$(1)/$$*.tex}' || echo ""
	makeglossaries $(1)/$$* || echo ""
	xelatex -shell-escape '\newcommand{\lang}{$(1)}\input{$(1)/$$*.tex}' || echo ""
	xelatex -shell-escape '\newcommand{\lang}{$(1)}\input{$(1)/$$*.tex}' || echo ""
	mv $$*.pdf $(1)-$(2).pdf
endef

$(foreach lang,$(langs),$(foreach target,$(targets),$(eval $(call TARGET_RULE,$(lang),$(target)))))

snippets:
	mkdir -p .latex-live-snippets/repl
	xelatex -shell-escape '\newcommand{\lang}{en}\newcommand{\updatesnippets}{}\input{en/book.tex}'

quick:
	xelatex -shell-escape en/print

clean:
	-rm *.aux
	-rm *.idx
	-rm *.ilg
	-rm *.ind
	-rm *.log
	-rm *.toc
	-rm *.gl*
	# -rm -r _minted-*
	# -rm -r .latex-live-snippets
	-rm *.pdf
	-rm *.exc.tex
	-rm *.fc.tex
	-rm *.sol.tex
	-rm *.ist
	-rm *.pyg
	-git checkout solutions.pdf

cover:
	convert ebook-cover.png -quality 100 -units PixelsPerInch -density 300x300 cover.pdf

ebook:
	pandoc --toc --toc-depth=2 -f markdown --epub-metadata=metadata.xml --css=base.css --highlight-style pygments --epub-cover-image=ebook-cover.png -o book.epub book.tex

.PHONY: snippets quick clean cover ebook

