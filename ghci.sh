#!/usr/bin/bash

response=$(mktemp /tmp/ghci-latex.XXXXXXXXXXX)

# echo ":l $1" | cat - $2 | sed "s/^@//" | sed 's_^/[^/]\+/[^/]\+/__' | stack repl &> .latex-live-snippets/repl/$3.tex
echo ":l $1" | cat - $2 | sed "s/^@//" | sed 's_^/[^/]\+/[^/]\+/__' | stack repl &> $response
latex-live-snippets-ghci $2 $response $3

