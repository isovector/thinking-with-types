#! /usr/bin/env zsh

set -ex

# for FILE in .latex-live-snippets/*.tex
# do
#   NAME=$(basename -s .tex $FILE)
#   shiatsu mixins.shiatsu $FILE ".latex-live-snippets/$NAME.txt"
# done


for FILE in en/**/*.markdown; do
  tail -n +106 $FILE > $FILE.yo
  rm $FILE
  mv $FILE.yo $FILE
done

# sed -i 's/\\begin{code}/<pre><code>/' book8.html
# sed -i 's_\\end{code}_</code></pre>_' book8.html
# sed -i 's/\\begin{repl}/<div class="repl">/' book8.html
# sed -i 's_\\end{repl}_</div>_' book8.html
# sed -i 's/\\begin{lstlisting}/<pre><code>/' book8.html
# sed -i 's_\\end{lstlisting}_</code></pre>_' book8.html
# sed -i 's_!\\tyeq!_~_g' book8.html
# sed -i 's_\\#_#_g' book8.html
# sed -i 's_\\\$_$_g' book8.html
# sed -i 's_---_\&mdash;_g' book8.html
# sed -i 's_--_\&ndash;_g' book8.html
# sed -i 's_•_\&bull;_g' book8.html
# sed -i 's_‘_\&lsquo;_g' book8.html
# sed -i 's_’_\&rsquo;_g' book8.html
# sed -i 's/\\_/_/g' book8.html
# sed -i 's/\\%/%/g' book8.html
# sed -i 's_!\\annotate{\([0-9]\+\)}!_<span class="annotate">\\htmlann{\1}</span>_g' book8.html
# sed -i 's_\\annotate{\([0-9]\+\)}_<span class="annotate">\\htmlann{\1}</span>_g' book8.html
# sed -i 's_\\htmlann{1}_\&#x2776;_g' book8.html
# sed -i 's_\\htmlann{2}_\&#x2777;_g' book8.html
# sed -i 's_\\htmlann{3}_\&#x2778;_g' book8.html
# sed -i 's_\\htmlann{4}_\&#x2779;_g' book8.html
# sed -i 's_\\htmlann{5}_\&#x277a;_g' book8.html
# sed -i 's/\\{/{/g' book8.html
# sed -i 's/\\}/}/g' book8.html
# sed -i 's/\\textbackslash/\\/g' book8.html

# cat mixin.html book8.html > book9.html
# ebook-convert book9.html ebook.epub --use-auto-toc --level1-toc="//h:h1" --level2-toc="//h:h2" --level3-toc="//h:h3" --cover=~/ebook-cover.png --authors="Sandy Maguire" --title="Thinking with Types"

