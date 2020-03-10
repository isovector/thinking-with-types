targets = book sample print
langs = en fr

define TARGET_RULE =
$(1)-$(2).pdf: $(1)-%.pdf: $(1)/%.tex
	xelatex -shell-escape $(1)/$$* || echo ""
	mv $$*.pdf $(1)-$(2).pdf
endef

$(foreach lang,$(langs),$(foreach target,$(targets),$(eval $(call TARGET_RULE,$(lang),$(target)))))

snippets:
	mkdir -p .latex-live-snippets/repl
	xelatex -shell-escape '\newcommand{\updatesnippets}{}\input{en/book.tex}'

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
	git checkout cover.pdf
	git checkout solutions.pdf

cover:
	convert ebook-cover.png -quality 100 -units PixelsPerInch -density 300x300 cover.pdf

ebook:
	pandoc --toc --toc-depth=2 -f markdown --epub-metadata=metadata.xml --css=base.css --highlight-style pygments --epub-cover-image=ebook-cover.png -o book.epub book.tex

.PHONY: snippets quick clean cover ebook

