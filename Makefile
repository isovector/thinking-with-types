snippets:
	mkdir -p .latex-live-snippets/repl
	xelatex -shell-escape '\newcommand{\updatesnippets}{}\input{en/book.tex}'

quick:
	xelatex -shell-escape en/print

targets = book.pdf sample.pdf print.pdf

$(targets): %.pdf: en/%.tex
	xelatex -shell-escape en/$* || echo ""
	makeglossaries en/$* || echo ""
	xelatex -shell-escape en/$* || echo ""
	makeglossaries en/$* || echo ""
	xelatex -shell-escape en/$* || echo ""
	xelatex -shell-escape en/$* || echo ""

clean:
	-rm *.aux
	-rm *.idx
	-rm *.ilg
	-rm *.ind
	-rm *.log
	-rm *.toc
	-rm *.gl*
	-rm -r _minted-*
	# -rm -r .latex-live-snippets
	-rm *.pdf
	git checkout cover.pdf
	git checkout solutions.pdf

cover:
	convert ebook-cover.png -quality 100 -units PixelsPerInch -density 300x300 cover.pdf

ebook:
	pandoc --toc --toc-depth=2 -f markdown --epub-metadata=metadata.xml --css=base.css --highlight-style pygments --epub-cover-image=ebook-cover.png -o book.epub book.tex
