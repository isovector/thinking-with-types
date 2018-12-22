quick:
	echo Q | xelatex -shell-escape print

book:
	echo Q | xelatex -shell-escape book || echo ""
	makeglossaries book || echo ""
	echo Q | xelatex -shell-escape book || echo ""
	makeglossaries book || echo ""
	echo Q | xelatex -shell-escape book || echo ""
	echo Q | xelatex -shell-escape book || echo ""

sample:
	echo Q | xelatex -shell-escape sample || echo ""
	makeglossaries sample || echo ""
	echo Q | xelatex -shell-escape sample || echo ""
	makeglossaries sample || echo ""
	echo Q | xelatex -shell-escape sample || echo ""
	echo Q | xelatex -shell-escape sample || echo ""

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
	-rm -r .latex-live-snippets
	-rm *.pdf
	git checkout book.pdf
	git checkout cover.pdf

cover:
	convert ebook-cover.png -quality 100 -units PixelsPerInch -density 300x300 cover.pdf

ebook:
	pandoc --toc --toc-depth=2 -f markdown --epub-metadata=metadata.xml --css=base.css --highlight-style pygments --epub-cover-image=ebook-cover.png -o book.epub book.tex
