# Thinking with Types


## Dedication

> I was terrible in English. I couldn't stand the subject. It seemed to me
> ridiculous to worry about whether you spelled something wrong or not, because
> English spelling is just a human convention---it has nothing to do with
> anything real, anything from nature.
>
> Richard P. Feynman


## Overview

This repository is all of the original source material for my book [Thinking
with Types: Type-Level Programming in Haskell](http://thinkingwithtypes.com). If
you're curious about what goes into writing a book, it might be a good place to
peruse.

Building this thing is particularly hard; I had to write three separate build
tools, and patch a few upstream libraries. You're free to try to figure it out,
but I'd suggest [just buying a copy
instead!](https://leanpub.com/thinking-with-types/)

Don't make me regret open-sourcing this.


## Commentary

My overarching organizational principle for this book was to make it *as hard as
possible to fuck up.* That meant that code samples should automatically be
tested, GHCi sessions should be automated, solutions and exercises should be
co-located, and that there is always a clearly defined source of truth for all
material.

The result was a joy to write, but remarkably terrible to deal with after the
fact. Paying a marginal compile-time cost of 1s per code example is fine on a
chapter-by-chapter basis, but my god does it add up when building the entire
project.

Doing it in LaTeX was good for the short-term, but turned into an eventual
liability. LaTeX is sweet for quickly producing good-looking pdf documents, but
it's sort of the worst of all worlds. It's sort of a content-language, and sort
of a real programming language, and doesn't force you into either paradigm. As a
result, there was lots of weird fiddling in order to get something to look
right---without knowing how it really works or without any discipline.

For writing a thesis or a report, this is fine, but the problem is an eternal
one: it's not denotational. LaTeX emphasizes *how to do it* rather than *what to
do.* The difference bites you in the ass when you want to [produce an
ebook](https://github.com/isovector/thinking-with-types/blob/master/build-epub.sh),
for example. You can't use LaTeX to produce the ebook, but you also *can't not*
use LaTeX, because you've automated necessary things in its shitty programming
environment.

Also, the tooling breaks all the time, seemingly without any sort of discernible
reason.

*If I were to do this project again, knowing what I know now,* I would write the
entire book as a series of Haskell modules. I'd use quasiquoters to write inline
prose and build meaningful abstractions in a principled, well-understood
language. In essence, I'd write a book DSL, and then write interpretations of
that into my eventual desired formats.


## License

This work is licensed under the Creative Commons
Attribution-NonCommercial-NoDerivatives 4.0 International License. To view a
copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or
send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

