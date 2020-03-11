#!/usr/bin/bash

stack --stack-yaml=lib/latex-live-snippets/stack.yaml exec latex-live-snippets -- code/$1.hs $2
sed -i "s/$2/$3/" ".latex-live-snippets/$1.$2.tex"
