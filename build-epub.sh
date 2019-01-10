#! /usr/bin/env bash

set -ex
rm -r /tmp/ebook || echo ""
mkdir /tmp/ebook
cp *.shiatsu /tmp/ebook
cp *.tex /tmp/ebook
cp *.html /tmp/ebook
cp -r static/ /tmp/ebook
cp -r .latex-live-snippets/ /tmp/ebook
cp -r .fracked/ /tmp/ebook
cd /tmp/ebook


# for FILE in .latex-live-snippets/*.tex
# do
#   NAME=$(basename -s .tex $FILE)
#   shiatsu mixins.shiatsu $FILE ".latex-live-snippets/$NAME.txt"
# done


sed -i 's/\\input{\([^}]\+\)}/include([[[[[\1]]]]])/g' *.tex
sed -i 's/\\subfile{\([^}]\+\)}/include([[[[[\1.tex]]]]])/g' *.tex
sed -i 's/``/\&ldquo;/g' *.tex
sed -i "s/''/\&rdquo;/g" *.tex
sed -i 's/\\Gls{\([^}]\+\)}/\u\1/g' *.tex
sed -i 's/\\Gls{\([^}]\+\)}/\u\1/g' *.tex
sed -i 's/^%.*//' *.tex
m4 print.tex > book.txt || echo ""
shiatsu includes.shiatsu book.txt book2.txt

sed -i 's/\\input{\([^}]\+\)}/include(`\1'"'"')/g' book2.txt

m4 book2.txt > book3.txt || echo ""
sed -i 's_<_\&lt;_g' book3.txt
sed -i 's_>_\&gt;_g' book3.txt
# TODO(sandy): this doesn't work for repls

awk 'OFS= /^\\begin/ {env=1} /^\\end/ {env=0} /^[A-Za-z]|^\\(ty|defn|hs|emph|ext|kind)/ { if (env == 0 && inblock == 0) { inblock = 1; print "<p>"; }} /^$/ {if (env == 0 && inblock == 1) { inblock = 0; print "</p>" }  } {print}' book3.txt > book4.txt

mkdir fracked
math-frack injection.shiatsu "fracked/taken" "fracked/given" book4.txt > boook.txt
rm -r fracked
mv .fracked fracked

# for FILE in fracked/taken*.Dollar.tex; do
#   FILE2=$(basename $FILE)
#   DEST=fracked/given${FILE2#taken}
#   mjpage --dollars --extensions 'TeX/AMSmath' --noGlobalSVG --fragment  < $FILE > $FILE.svg
#   sed -i 's_</\?span[^>]*>__g' $FILE.svg
#   convert -resize 6.5% $FILE.svg $FILE.png
#   echo "<img src='$FILE.png' />" > $DEST
# done

# for FILE in fracked/taken*.DoubleDollar.tex; do
#   FILE2=$(basename $FILE)
#   DEST=fracked/given${FILE2#taken}
#   mjpage --extensions 'TeX/AMSmath' --noGlobalSVG --fragment  < $FILE > $FILE.svg
#   sed -i 's_</\?span[^>]*>__g' $FILE.svg
#   convert -resize 6.5% $FILE.svg $FILE.png
#   echo "<div class='doubledollar'><img src='$FILE.png' /></div>" > $DEST
# done

shiatsu mixins.shiatsu includes.shiatsu boook.txt book5.txt

awk 'OFS='' /!BEGIN_NEW_CHAPTER_NUMBER/ {chapter++;count=0} /\\begin{dorepl}/ {print sprintf("%s{%s}{%s}", $0,chapter,count); count++; matching=1} {if (!matching) print} /\\end{dorepl}/ {matching=0}' book5.txt | sed 's/\\begin{dorepl}/\\snipRepl/' > book9.txt
sed -i 's/!BEGIN_NEW_CHAPTER_NUMBER//' book9.txt
sed -i 's/!BEGIN_NEW_CHAPTER//' book9.txt

shiatsu mixins.shiatsu includes.shiatsu book9.txt book6.txt

sed -i 's/\\input{\([^}]\+\)}/include(`\1'"'"')/g' book6.txt
m4 book6.txt > book7.txt || echo ""

# awk '/\$\$/ {if (inside==0) {print "\\[";} else {print "\\]"} inside = 1 - inside;} ! /\$\$/ {print}' book7.txt | sed 's/\$\([^$]\+\)\$/\\(\1\\)/g' > book8.html

shiatsu mixins.shiatsu includes.shiatsu book7.txt book8.html

sed -i 's/\\begin{code}/<pre><code>/' book8.html
sed -i 's_\\end{code}_</code></pre>_' book8.html
sed -i 's/\\begin{repl}/<div class="repl">/' book8.html
sed -i 's_\\end{repl}_</div>_' book8.html
sed -i 's/\\begin{lstlisting}/<pre><code>/' book8.html
sed -i 's_\\end{lstlisting}_</code></pre>_' book8.html
sed -i 's_!\\tyeq!_~_g' book8.html
sed -i 's_\\#_#_g' book8.html
sed -i 's_\\\$_$_g' book8.html
sed -i 's_---_\&mdash;_g' book8.html
sed -i 's_--_\&ndash;_g' book8.html
sed -i 's_•_\&bull;_g' book8.html
sed -i 's_‘_\&lsquo;_g' book8.html
sed -i 's_’_\&rsquo;_g' book8.html
sed -i 's/\\_/_/g' book8.html
sed -i 's/\\%/%/g' book8.html
sed -i 's_!\\annotate{\([0-9]\+\)}!_<span class="annotate">\\htmlann{\1}</span>_g' book8.html
sed -i 's_\\annotate{\([0-9]\+\)}_<span class="annotate">\\htmlann{\1}</span>_g' book8.html
sed -i 's_\\htmlann{1}_\&#x2776;_g' book8.html
sed -i 's_\\htmlann{2}_\&#x2777;_g' book8.html
sed -i 's_\\htmlann{3}_\&#x2778;_g' book8.html
sed -i 's_\\htmlann{4}_\&#x2779;_g' book8.html
sed -i 's_\\htmlann{5}_\&#x277a;_g' book8.html
sed -i 's/\\{/{/g' book8.html
sed -i 's/\\}/}/g' book8.html
sed -i 's/\\\\/<br>/g' book8.html
sed -i 's/\\textbackslash/\\/g' book8.html

cat mixin.html book8.html > book9.html
ebook-convert book9.html ebook.epub --use-auto-toc --level1-toc="//h:h1" --level2-toc="//h:h2" --level3-toc="//h:h3" --cover=~/ebook-cover.png --authors="Sandy Maguire" --title="Thinking with Types"

