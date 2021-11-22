## Terms, Types and Kinds

### The Kind System

In everyday Haskell programming, the fundamental building blocks are those of
terms and *types*. Terms are the values you can
manipulate---the things that exist at runtime. Types, however, are little more
than sanity-checks: proofs to the compiler (and ourselves) that the programs
we're trying to write make some amount of sense.

Completely analogously, the fundamental building blocks for type-level
programming are *types* and *kinds*. The types now become the things
to manipulate, and the kinds become the proofs we use to keep ourselves honest.

The kind system, if you're unfamiliar with it, can be reasonably described as
"the type system for types." By that line of reasoning, then, kinds are
loosely "the types of types."

Consider the numbers `4` and `5`, both of type `Int`. As far as the
compiler is concerned, we could replace every instance of `4` with `5` in
our program, and the whole thing would continue to compile. The program itself
might do something different, but by virtue of both being of type `Int`,
`4` and `5` are interchangeable---at least as far as the type checker is
concerned.


#### The Kind of "Types"

Before continuing our discussion, we must sidestep a potential point of
confusion. Rather perplexingly, there are several, related meanings that fall
under the word "type." We will need to differentiate between two of them.

The word "type" can be used to describe anything that exists at the type level,
which is to say, anything that is neither a term nor a kind.

However, we can also refer to `kind:Type`s, which is the *kind* of types that
have inhabitants. Historically `kind:Type` has been written as $\star$, but this
older notation is deprecated. `kind:Type` is the kind of things like `Int` and
`Maybe Bool`---it classifies the sorts of things that exist at runtime. Some
things which *aren't* of kind `kind:Type` are `Maybe` (without a type
parameter), and `Show`.

Sometimes we call the sorts of things which have kind `kind:Type` value types.

In this book, we will typeset kinds in `kind:Script` in order to differentiate
them from regular types. Note that this is merely a convention of the printing
process---the kind `kind:Type` should be still written as `Type` in a Haskell
source file.


#### Arrow Kinds

This book assumes you already have a working knowledge of the standard Haskell
kinds, `kind:Type` and `kind:(->)`. As such, we will not dally here any more
than is strictly necessary.

> TODO(sandy): no, this is why i'm here. TEACH THIS

Higher-kinded types (HKTs) are those which have type variables. Fully saturated
HKTs in everyday Haskell always have kind `kind:Type`, which means that their type
constructors *do not.*

Let's consider `Maybe`. Because it takes a single `kind:Type` parameter, we say
that `Maybe` has kind `kind:Type -> Type`---it takes a `kind:Type` and gives you
back one.  `Either` takes two parameters, and so its kind is `kind:Type -> Type
-> Type`.

But more interesting higher-kinded types are possible too. What about the monad
transformer `MaybeT`? It also takes two parameters, but unlike `Either`, one of
those parameters must be a `Monad`. Because monads are always of kind `kind:Type
-> Type`, we are left with the inescapable conclusion that `MaybeT` must have
kind `kind:(Type -> Type) -> Type -> Type`. Yeah---it's a bit of a mouthful.

If you're not already familiar with this stuff, it will soon come to you just as
naturally as the type checking rules do.


#### Constraint Kinds

However, kinds apply to everything at the type level, not just the things we
traditionally think of as "types." For example, the type of `show` is
`Show a => a -> String`. This `Show` thing exists as part of the type
signature, even though it's clearly not a `Type`. Does `Show a` also
have a kind?

Yes! Its kind is `kind:Constraint`. More generally, `kind:Constraint` is the
kind of any fully-saturated typeclass.

Exercise

:   If `Show Int` has kind `kind:Constraint`, what's the kind of `Show`?

Solution
:   `kind:Type -> Constraint`


Exercise

:   What is the kind of `Functor`?

Solution

:   `kind:(Type -> Type) -> Constraint`


Exercise

:   What is the kind of `Monad`?

Solution

:   `kind:(Type -> Type) -> Constraint`


Exercise

:   What is the kind of `MonadTrans` in the following?

    ```haskell
    class MonadTrans t where
      lift :: Monad m => m a -> t m a
    ```

Solution

:   `kind:((Type -> Type) -> Type -> Type) -> Constraint`


We will discuss the `kind:Constraint` kind in much further detail later
@Sec:constraints.

Without further language extensions, this is the extent of the expressiveness of
Haskell's kind system. As you can see, it's actually quite limited---we have
no notion of polymorphism, of being able to define our own kinds, or of being
able to write functions.

Fortunately, those things are the subject matter of the remainder of this
book---techniques, tools and understanding for Haskell's more esoteric language
extensions.


### Data Kinds

By enabling the `-XDataKinds` extension, we gain the ability to talk about kinds
other than `kind:Type`, `kind:Constraint`, and their arrow compositions. In
particular, `-XDataKinds` lifts data constructors into *type constructors* and
types into *kinds.*

What does that mean? As an example, let's look at a familiar type definition:

[code/Kinds.hs:Bool](Snip)

When `-XDataKinds` is enabled, the above *type* definition of `Bool` *also*
gives us the following *kind* definition---though note that this is not legal
Haskell syntax:

[code/Kinds.hs:kind](Snip)

In other words, via `-XDataKinds` we have now declared the types `'True` and
`'False`---both of kind `kind:Bool`. We call `'True` and `'False` promoted data
constructors. To be clear, the `data Bool` definition above introduces the
following things into scope (as usual):

* A type constructor `Bool` of kind `kind:Type`
* A data constructor `True` of type `Bool`
* A data constructor `False` of type `Bool`

However, when `-XDataKinds` is enabled, our definition of `Bool` also introduces
the following into scope:

* A new kind: `kind:Bool`
* A promoted data constructor `'True` of kind `kind:Bool`
* A promoted data constructor `'False` of kind `kind:Bool`

The apostrophes on `'True` and `'False` are known as ticks, and are used to
distinguish promoted data constructors from everyday type constructors.  Because
promoted data constructors exist in the same namespace as type constructors,
these ticks aid in differentiating the two. Strictly speaking, the ticks aren't
always necessary, but consider the common case of a type with a single data
constructor:

[code/Kinds.hs:Unit](Snip)

In this example, it's very important to differentiate between the *type
constructor* `Unit` (of kind `kind:Type`), and the *promoted data constructor*
`'Unit` (of kind `kind:Unit`.) This is a subtle point, and can often lead to
inscrutable compiler errors; while it's fine to ask for values of type `Maybe
Unit`, it's a *kind error* to ask for `Maybe 'Unit`---because `'Unit` isn't of
kind `kind:Type`!

Let's return to the question of the importance of data kinds. Type-level
programming in Haskell without them is equivalent to programming in a
dynamically typed language. By default, having every kind you manipulate be
`Type` is a lot like having all of your terms be of the same type.

While types don't let you do anything you couldn't otherwise, they sure make it
easier to reason about your program! Data kinds are exactly the same---as we
write more and more interesting type-level programs, we'll use kind signatures
to restrict the sorts of types we can be dealing with.

Promoted data constructors are of the wrong kind to ever exist at runtime, which
raises the question "what good are they?" It's a little too soon to answer this
in full glory, but without any other fancy type-level machinery, we can use them
as phantom parameters.


### About the War Stories {.rev2}

A fantastic way to develop an understanding about how to apply type-level
techniques is to study real life examples. Rather than waiting until an
appropriate opportunity arises, and hoping that we have a flicker of
recognition, we can instead look at other people's use-cases.

Throughout this book, I've included many "war stories" from my near-decade of
experience writing Haskell. In my personal projects, paid work, and open-source
contributions, my first attempts at code are usually smelly. While the core
ideas are (usually) correct, too often would it be easy to accidentally get one
of the small details wrong. Instead of waiting for the inevitable bug, it's
usually worth heading them off at the pass, and finding minor changes that will
help me enlist the type-system.

Every war story in this book is true. Undoubtedly my memories of them are
embellished, but I've made an honest attempt to capture the situations,
problems, and---most importantly---thought processes that I used to reason
through to a better solution.


### War Story: Tracking Data's Age {.rev2}

I spent a year contributing a large feature to the Haskell Language Server (HLS)
project. HLS is a tool that provides smart, language-dependent support to text
editors---for example, it can intelligently add imports, warn you about smelly
code, and automatically synthesize functions.

By virtue of being interactive, HLS needs to respond quickly to user actions,
and thus it makes significant use of concurrency and caching. For example, when
the user changes their source code, HLS notices this and reruns the type checker
over the active module. But suppose the user then wants to complete some code,
which requires type information. If the type checker hasn't finished, HLS will
give back *stale* type information, corresponding to whatever data it last had.

HLS communicates with text editors via the Language Server Protocol, which
mandates that code be described by its source position---that is, the line and
column numbers in source code that correspond to the thing in question. Thus,
any time HLS has an interaction with the user, it's in terms of lines and
columns.

This works well, except that there's a problem. *The source positions of stale
data might not align with the source positions of the current file!* Thankfully,
HLS handles this for us: whenever you retrieve stale data, it will helpfully
also return a `PositionMapping`, which comes with two functions:

[code/War/Age.hs:fromCurrentRange](Snip)

Client code can use the `PositionMapping` to map a source location from the past
into the current version, or vice versa.

As you can imagine, wrangling `PositionMapping`s by hand is quite the Herculean
effort! It takes significant mental effort to keep track of when any particular
`Range` is valid. To make matters worse, my feature needed to coalesce data from
multiple sources---any piece of which might be *differently* stale! Struggling
through the implementation, I finally finished, relatively confident that I had
successfully wrestled all of the `Range`s.

Unfortunately, I started getting bug reports that my feature wouldn't activate
if the user had been typing around the place it should trigger immediately
before. Uh oh! The panic flashed before my eyes---I'd screwed up a `Range`
somewhere, and it was going to be a nightmare to track down!

Well, it would have been, if I'd decided to do it by hand. Instead I thought
this was a job best left to the computer. The type checker is good at
fastidious, tedious jobs like this---all I had to do was to teach it how. My
thought was that I could associate an "age" with every particular piece of data,
modifying `fromCurrentRange` to correctly transform the age.

The first step was to define an `Age` type:

[code/War/Age.hs:BadAge](Snip)

which I could use to tag my data. By using `-XDataKinds`, I could use its
promoted data constructors as arguments to `Tracked`:

[code/War/Age.hs:Tracked](Snip)

Of course, the `age` parameter doesn't *actually do* anything, but that's OK.
All I needed it for was to ensure that `Tracked 'Stale Range` was a different
type than `Tracked 'Current Range`. I couldn't accidentally use one in place of
the other, because the type system would holler.

The next step was to use the same trick on `PositionMapping`, to track what
`Age` was on either side of the mapping:

[code/War/Age.hs:PositionMap](Snip)

Many of the structures I needed to track were much bigger than `Range`s. For
example, my type-checked data came in the form of an entire type-checked AST of
the current module---something that could conceivably contain many thousands of
`Range`s. Rather than manually lifting all of the time-traveling machinery
across each data type, it seemed wiser to build a single abstraction that could
push a `PositionMap` through a type:

[code/War/Age.hs:MapAge](Snip)

And of course, there's an instance for `MapAge Range`:

[code/War/Age.hs:MapAgeRange](Snip)

This trick of using `coerce` is a great side-effect of implementing `Tracked` as
a `newtype` rather than as a `data`---rather than have a big wrapping/unwrapping
ceremony, we can just `coerce` the desired function. This technique is discussed
at more depth in @sec:roles.

I added instances of `MapAge` for every type I was interested in, and finally
changed the function that could return stale data to instead return a
`(PositionMap 'Stale 'Current, Tracked 'Stale a)`. Now I could be sure that every
piece of data's age was being tracked correctly. All that was left was to make
sure my coalescing function was correct. What used to be:

```haskell
buildStructure
    :: ParsedSource
    -> TypecheckedSource
    -> Judgement
    -> TacticResult
```

instead was lifted to be polymorphic over its age:

```haskell
buildStructure
    :: Tracked age ParsedSource
    -> Tracked age TypecheckedSource
    -> Tracked age Judgement
    -> Tracked age TacticResult
```

With this change, I could now be certain that I could only call `buildStructure`
on data that was all the same age---and thus that all of the `Range`s inside of
these pieces of data aligned.

Feeling very proud of myself, I did some perfunctory testing, but found a bug
almost immediately! How could this be? The type checker was now enforcing that I
was correctly tracking the age of all data!

After a few moments of deep thought, I realized my mistake. While all `Current`
data is the same age, not all `Stale` data is equally stale! Different data
caches are updated at different rates, so there was no guarantee that a stale
`ParsedSource` would be the same age as a stale `TypecheckedSource`!

The solution was minor. I employed the `ST` trick (described in @sec:st-trick),
using an existential variable to differentiate `Stale` values:

[code/War/Age.hs:Age](Snip)

Immediately, my change must have worked, because my HLS feature no longer
compiled. Indeed, I was using two pieces of data that I couldn't prove were
equally stale. Instead I used `mapAgeTo` to fast-forward all my data to the
present before calling `buildStructure` on it.

Testing showed that the bug was fixed. I pushed the changes and sat back,
satisfied in the knowledge of a job well done.


### Promotion of Built-In Types

Necessary

:   [code/Kinds.hs:typelits](Snip)


With `-XDataKinds` enabled, almost[^dont-promote] all types automatically
promote to kinds, including the built-in ones. Since built-in types (strings,
numbers, lists and tuples) are special at the term-level---at least in terms of
syntax---we should expect them to behave oddly at the type level as well.

[^dont-promote]: GADTs and other "tricky" data constructors fail to promote.

When playing with promoted built-in types, it's necessary to first import the
`GHC.TypeLits` module. `GHC.TypeLits` defines the kinds themselves, as well as
all of the useful type families for manipulating them. We'll cover this idea in
more detail soon.


#### Symbols

The promoted version of a `String` is called a `kind:Symbol`. `kind:Symbol`s are
not lists of characters. Symbol type literals can be written by just using a
string literal in a place where a type is expected. For example:

```{ghci=code/Kinds.hs}
:set -XDataKinds
:kind "hello"
```

It's somewhat frustrating that `kind:Symbol`s are not merely lists of promoted
characters; it means that `kind:Symbol`s are no longer inductive types. It's
impossible to deconstruct a `kind:Symbol`, although we are capable of
concatenating them via a magic `AppendSymbol` primitive provided in
`GHC.TypeLits`.

> TODO(sandy): it is NOT impossible to deconstruct a symbol!

```{ghci=code/Kinds.hs}
:set -XDataKinds
:kind AppendSymbol
:kind! AppendSymbol "thinking" " with types"
```

Additionally, we are capable of comparing `kind:Symbol`s via the `CmpSymbol`
primitive.

```{ghci=code/Kinds.hs}
@import Prelude
:kind CmpSymbol
:kind! CmpSymbol "sandy" "sandy"
:kind! CmpSymbol "sandy" "batman"
```

Notice that `CmpSymbol` is of kind `kind:Symbol -> Symbol -> Ordering`. This
`kind:Ordering` is just the `-XDataKinds` promoted version of the standard `Ordering`
type from `Prelude`.


#### Natural Numbers

The promotion of numbers is a little more odd. Only the natural numbers ($0, 1,
2, \ldots$) can be promoted---there are no negative, fractional nor floating
type-level numeric literals. These natural numbers, naturally enough, are of
kind `Nat`.

```{ghci=code/Kinds.hs}
:kind 5085072209
```

`GHC.TypeLits` defines primitives for performing arithmetic on `kind:Nat`s, with
exactly the same symbolic identifiers you'd expect them to have. Using them will
require enabling `-XTypeOperators`.

```{ghci=code/Kinds.hs}
:set -XTypeOperators
:kind! (1 + 17) * 3
:kind! (128 `Div` 8) ^ 2
```


#### Lists

Imagine lists were defined as library code, without any special syntax. They'd
have the definition

[code/Kinds.hs:list](Snip)

And in fact, this is exactly what the promoted data constructors of lists look
like. When `-XDataKinds` is enabled, we get the following promoted data
constructors in scope:

* `'[]` of kind `kind:[a]`
* `'(:)` of kind `kind:a -> [a] -> [a]`; used infix as `x ': xs`

Note that although we haven't yet talked about kind-level polymorphism (things
of kind `a`), it is meaningful and corresponds exactly to your intuition about
how polymorphism should behave.

When compared against the data constructors of lists, `[] :: [a]` and `(:) :: a
-> [a] -> [a]`, with a little concentration, the promoted data constructors
should make sense.  Because lists' data constructors have symbolic names, they
also require `-XTypeOperators` enabled to be used. Don't worry though, GHC will
helpfully remind you if you forget.

There is another subtle point to be noted when dealing with list-kinds. While
`[Bool]` is of kind `Type` and describes a term-level list of booleans,
the type `'[Bool]` is of kind `[Type]` and describes a type-level list
with one element (namely, the type `Bool`.) Compare:

```{ghci=code/Kinds.hs}
:kind [Bool]
:kind '[Bool]
```

Further care should be taken when constructing a promoted list; due to the way
GHC's lexer parses character literals (`'a'`), make sure you add a space after
starting a promoted list. While `'[ 'True ]` is fine, `'['True]` is
unfortunately a parse error.

```{ghci=code/Kinds.hs}
:kind '[ 'True ]
:kind '['True]
```

This quirk of the lexer often bites beginners---if you get an unexpected syntax
error when dealing with type-level literals, it's likely caused by this.


#### Tuples

Tuples also promote in a straightforward way, via the `'(,)`
constructor.[^tuple-ctors]

[^tuple-ctors]: And all the related promoted tuples, `'(,,)`, `'(,,,,)` and etc.

```{ghci=code/Kinds.hs}
:kind '(2, "tuple")
```

Tuples are promoted with a leading tick. The aforementioned parsing gotcha
applies here as well, so be careful.


### Type-Level Functions {.rev2}

Where `-XDataKinds` really begins to shine, however, is through the introduction
of closed type families. You can think of closed type families as *functions at
the type level.* In fact, we've looked at quite a few in this chapter already.
Each of those "primitives" I mentioned earlier---`CmpSymbol`, `Div`, and
etc.---are all closed type families.

The ability to write closed type families isn't merely one bestowed upon GHC
developers, however. We are capable of writing our own too! But first, compare
the regular, term-level function `or`, which computes the boolean OR of two
`Bool`s:

[code/Kinds.hs:or](Snip)

Unlike data constructors, we're unfortunately unable to automatically promote
term-level functions into type-level ones. However, after enabling
`-XTypeFamilies`, we can instead "promote" `or` by explicitly duplicating this
logic and writing a completely separate, closed type family.

[code/Kinds.hs:TFOr](Snip)

Line for line, `or` and `Or` are analogous. The closed type family `Or` requires
a capital letter for the beginning of its name, because it exists at the
type level, and besides having a more verbose kind signature, the two
definitions proceed almost exactly in lockstep.

Exercise

:   Write a closed type family to compute `Not :: Bool -> Bool`.

Solution

:   [code/Kinds.hs:Not](Snip)


While the metaphor between type families and functions is enticing, it isn't
entirely *correct.* The analogues break down in several ways, but the most
important one is that *type families must be saturated.* Another way of saying
this is that all of a type family's parameters must be specified simultaneously;
there is no currying available.

Recall the `map` function:

[code/Kinds.hs:map](Snip)

We're capable of promoting `map` to the type level:

[code/Kinds.hs:Map](Snip)

But because we're unable to partially apply closed type families, `Map` doesn't
turn out to be particularly useful.

```{ghci=code/Kinds.hs}
:kind! Map (Or 'True) '[ 'True, 'False, 'False ]
```

This error is trying to tell us is that we used the `Or` closed type-family
without first saturating it---we only passed it one parameter instead of the two
it requires, and so unfortunately GHC refuses to compile this program.

There is nothing preventing us from writing `Map`, but its usefulness in this
form is severely limited. We are simply unable to curry closed type families,
and so we can't use `Map` to perform any interesting type-level computations for
us. We will later explore some techniques for working around this unfortunate
limitation when we discuss *first class families* in chapter 10.

> TODO(sandy): CUSKS and SAKS

Before leaving this topic, let's look again at our definition of `Or`. Pay close
attention to its kind signature. We write it as `Or (x :: Bool) (y :: Bool)
:: Bool`, rather than `Or x y :: Bool -> Bool -> Bool`. The kinds of type
families are tricky beasts; the kind you write after the `::` is the kind of the
type *returned* by the type family, *not* the kind of the type family itself.

> TODO(sandy): NO LONGER

[code/Kinds.hs:Foo](Snip)

[code/Kinds.hs:Bar](Snip)

Take a moment to think about the kinds of `Foo` and `Bar`. While `Foo` has kind
`Bool -> Bool -> Bool`, `Bar` has kind `Type -> Type -> Bool -> Bool -> Bool`.
GHCi agrees with our assessment:

```{ghci=code/Kinds.hs}
:kind Foo
:kind Bar
```

We will discuss type families in more generality in a later chapter; for now
it's sufficient to think of closed type families as type-level functions.

