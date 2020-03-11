# latex-live-snippets

Install with `stack install`, and then in latex via:

```latex
% set this to where your code is
\newcommand{\srcdir}{.}

\newcommand{\snip}[2]{\immediate\write18{latex-live-snippets \srcdir/#1.hs #2}\input{.latex-live-snippets/#1.#2.tex}}
```

Now, given a file `Test.hs`:

```haskell
zoo :: Int
zoo = 5

test :: Bool -> Bool
test True = id $ True
test _    = True  -- ! 1
```

we can call

```latex
\snip{Test}{test}
```

which will result in:

```latex
\begin{code}
test :: Bool -> Bool
test True = id $ True
test _    = True !\annotate{1}!
\end{code}
```

It will also find type families, data definitions. Custom snippet areas can be
defined via comments  of the form `-- # name`.


# latex-live-snippets-ghci

Put the following on your path as `latex-live-snippets-run-ghci`

```bash
#!/usr/bin/bash

response=$(mktemp /tmp/ghci-latex.XXXXXXXXXXX)

echo ":l $1" | cat - $2 | stack exec ghci > $response
latex-live-snippets-ghci $2 $response $3
```

and install in latex via

```latex
\usepackage{fancyvrb}

\makeatletter
\newcommand*\ifcounter[1]{%
  \ifcsname c@#1\endcsname
    \expandafter\@firstoftwo
  \else
    \expandafter\@secondoftwo
  \fi
}
\makeatother

\newcommand{\doreplparam}{}
\newcommand{\doreplfile}{}
\newenvironment{dorepl}[1]{\VerbatimEnvironment
\renewcommand{\doreplparam}{#1}
\renewcommand{\doreplfile}{\doreplparam-\arabic{\doreplparam}}
\ifcounter{\doreplparam}{}{\newcounter{\doreplparam}}
\begin{VerbatimOut}{/tmp/\doreplfile.aux}}{\end{VerbatimOut}
\immediate\write18{latex-live-snippets-run-ghci \srcdir/\doreplparam.hs /tmp/\doreplfile.aux \doreplfile}
\input{.latex-live-snippets/repl/\doreplfile.tex}
\stepcounter{\doreplparam}
}
```

Now you can run repl sessions:

```latex
\begin{dorepl}{Test}
:set -XDataKinds
:t zoo
take 3 $ iterate not False
\end{dorepl}
```

results in

```latex
\begin{repl}
\ghcisilent{:set -XDataKinds}
\ghci{:t zoo}{zoo :: Int}
\ghci{take 3 $ iterate not False}{[False,True,False]]}
\end{repl}
```

