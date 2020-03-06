quick:
	echo Q | xelatex -shell-escape print

book:
	xelatex -shell-escape prose/book || echo ""
	# makeglossaries prose/book || echo ""
	# echo Q | xelatex -shell-escape prose/book || echo ""
	# makeglossaries prose/book || echo ""
	# echo Q | xelatex -shell-escape prose/book || echo ""
	# echo Q | xelatex -shell-escape prose/book || echo ""

sample.pdf:
	xelatex -shell-escape prose/sample || echo ""
	# makeglossaries prose/sample || echo ""
	# xelatex -shell-escape prose/sample || echo ""
	# makeglossaries prose/sample || echo ""
	# xelatex -shell-escape prose/sample || echo ""
	# xelatex -shell-escape prose/sample || echo ""

print:
	echo Q | xelatex -shell-escape print || echo ""
	makeglossaries print || echo ""
	echo Q | xelatex -shell-escape print || echo ""
	makeglossaries print || echo ""
	echo Q | xelatex -shell-escape print || echo ""
	echo Q | xelatex -shell-escape print || echo ""

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
