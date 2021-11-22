## Preface to the Second Edition {-}

The first edition of Thinking with Types came out in 2018, and was wrong about
the state of the art within a week. The Haskell community evidently moves
extremely quickly---at least in the type system. It's the nature of a book like
this that it's necessarily going to be behind the times, and that's OK. The goal
is not, and has never been, to contribute new ideas or techniques---merely to
chronicle the existing ones.

I really wanted to be content with the book as it was originally written. And I
was, for a few years. But in 2021, it felt disingenuous to continue calling
*Thinking with Types* the definitive guide to type-level programming. There's
just too much missing!

Writing a second edition is a blessing and a curse. It's provided me the ability
to update the material, but also made me uncomfortably aware of the lack of
polish in the first edition. The curse is *dissatisfaction* in the quality of my
own work, and I've found myself rewriting and polishing things that were
perfectly passable.

But the book is significantly better because of it. The most common question
readers ask me is "how can I use any of these techniques in the real world?"
I've addressed this by the addition of "war stories." These war stories are
interspersed throughout the chapters, offering real-world anecdotes where the
technique helped save the day. They help bridge the gap between the pedagogical
examples---chosen for clarity---and doing this stuff on the job, where several
tools are often required simultaneously.

In the world of Haskell, many things have also changed. We now have
`-XLinearTypes`, `-XQuantifiedConstraints` and `-XImpredicativeTypes`: new tools
that allow for more expressive programs. There are changes in type promotion,
including new primitives for dealing with built-in kinds---finally allowing for
a real `printf`! Even outside of shiny language features, new patterns like
"higher-kinded data" have proven successful in the real world. And of course,
there are a slew of new libraries that make life easier. The result is a
veritable torrent of new concepts to stay atop of.

Perhaps the best change to come out since the first edition is the `GHC2021`
variant of Haskell. `GHC2021` enables by default most of the language extensions
required to do type-level programming. This book assumes all your projects are
written in `GHC2021`. The results are staggering: on average, the number of
extensions required per example has gone from 18 down to three. It's a very
welcome change.

Despite my meddling, not all of the chapters are changed from the first edition.
If you are interested only in the changes, updated sections are identified by a
star in the table of contents. If you have a good memory and are short on time,
feel free to prioritize these sections.

> TODO(sandy): replace star with whatever it is you come up with.

Personally, I'm a very different human than the one who originally wrote this
book. The man who wrote this book was lost in the world, looking for something
he couldn't articulate. But since then I've found my confidence, my home, and,
most importantly, my partner. It's quite a trip to go back and revisit the first
meaningful artifact I ever made---and my most popular project by far.

Long story short, things change, and so this book must too. Welcome back to the
world of types.

Written with love by Sandy Maguire. 2021.

