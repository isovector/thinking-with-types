
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



Consider the numbers `4`{.haskell} and `5`{.haskell}, both of type `Int`. As far as the
compiler is concerned, we could replace every instance of `4`{.haskell} with `5`{.haskell} in
our program, and the whole thing would continue to compile. The program itself
might do something different, but by virtue of both being of type `Int`,
`4`{.haskell} and `5`{.haskell} are interchangeable---at least as far as the type checker is
concerned.


#### The Kind of "Types"

Before continuing our discussion, we must sidestep a potential point of
confusion. Rather perplexingly, there are several, related meanings that fall
under the word "type." We will need to differentiate between two of them.

The word "type" can be used to describe anything that exists at the type
level, which is to say, anything that is neither a term nor a kind.

However, we can also refer to `Type`s, which is the *kind* of types
that have inhabitants. Historically `Type` has been written as $\star$, but
this older notation is slated for deprecation in the latest versions of GHC.
`Type` is the kind of things like `Int` and `Maybe Bool`---it
classifies the sorts of things that exist at runtime. Some things which
*aren't* of kind `Type` are `Maybe` (without a type parameter), and
`Show`.

Sometimes we call the sorts of things which have kind `Type` value
types.

In this book, we will typeset kinds in `SmallCaps` in order to
differentiate them from regular types. Note that this is merely a convention of
the printing process---the kind `Type` should be still written as `Type`{.haskell}
in a Haskell source file.


#### Arrow Kinds

This book assumes you already have a working knowledge of the standard Haskell
kinds, `Type` and `(->)`. As such, we will not dally here any more
than is strictly necessary.

Higher-kinded types (HKTs) are those which have type
variables. Fully saturated HKTs in everyday Haskell always have kind
`Type`, which means that their type constructors *do not.*

Let's consider `Maybe`. Because it takes a single `Type` parameter, we
say that `Maybe` has kind `Type -> Type`---it takes a `Type` and
gives you back one. `Either` takes two parameters, and so its kind is
`Type -> Type -> Type`.

But more interesting higher-kinded types are possible too. What about the monad
transformer `MaybeT`? It also takes two parameters, but unlike `Either`,
one of those parameters must be a `Monad`. Because monads are always of kind
`Type -> Type`, we are left with the inescapable conclusion that
`MaybeT` must have kind `(Type -> Type) -> Type -> Type`.

If you're not already familiar with this stuff, it will soon come to you just as
naturally as the type checking rules do.


#### Constraint Kinds

However, kinds apply to everything at the type-level, not just the things we
traditionally think of as "types." For example, the type of `show`{.haskell} is
`Show a => a -> String`. This `Show` thing exists as part of the type
signature, even though it's clearly not a `Type`. Does `Show a` also
have a kind?

Yes! Its kind is `Constraint`. More generally, `Constraint` is the
kind of any fully-saturated typeclass.

```exercise
If `Show Int` has kind `Constraint`, what's the kind of
`Show`?
```

```solution
  `Type -> Constraint`
```


```exercise
What is the kind of `Functor`?
```

```solution
  `(Type -> Type) -> Constraint`
```


```exercise
What is the kind of `Monad`?
```

```solution
  `(Type -> Type) -> Constraint`
```


```exercise
What is the kind of `MonadTrans`?
```

```solution
  `((Type -> Type) -> Type -> Type) -> Constraint`
```


We will discuss the `Constraint` kind in much further detail later
@Sec:constraints.

Without further language extensions, this is the extent of the expressiveness of
Haskell's kind system. As you can see, it's actually quite limited---we have
no notion of polymorphism, of being able to define our own kinds, or of being
able to write functions.

Fortunately, those things are the subject matter of the remainder of this
book---techniques, tools and understanding for Haskell's more esoteric language
extensions.


### Data Kinds

By enabling the `-XDataKinds` extension, we gain the ability to talk about
kinds other than `Type`, `Constraint`, and their arrow derivatives. In
particular, `-XDataKinds` lifts data constructors into *type
constructors* and types into *kinds.*

What does that mean? As an example, let's look at a familiar type definition:

[code/Kinds.hs:Bool](Snip)

When `-XDataKinds` is enabled, the above *type* definition of `Bool`
*also* gives us the following *kind* definition---though note that
this is not legal Haskell syntax:

[code/Kinds.hs:kind](Snip)

In other words, via `-XDataKinds` we have now declared the types `'True`
and `'False`---both of kind `Bool`. We call `'True` and `'False`
promoted data constructors. To be clear the
`data Bool`{.haskell} definition above introduces the following things into scope (as
usual):

<ul>
  * A type constructor `Bool` of kind `Type`
  * A data constructor `True`{.haskell} of type `Bool`
  * A data constructor `False`{.haskell} of type `Bool`
</ul>

However, when `-XDataKinds` is enabled, our definition of `Bool` also
introduces the following into scope:

<ul>
  * A new kind: `Bool`
  * A promoted data constructor `'True` of kind `Bool`
  * A promoted data constructor `'False` of kind `Bool`
</ul>

The apostrophes on `'True` and `'False` are known as ticks,
and are used to distinguish promoted data constructors from everyday type
constructors.  Because promoted data constructors exist in the same namespace as
type constructors, these ticks aid in differentiating the two. Strictly
speaking, the ticks aren't always necessary, but consider the common case of a
type with a single data constructor:

[code/Kinds.hs:Unit](Snip)

In this example, it's very important to differentiate between the *type
constructor* `Unit` (of kind `Type`), and the *promoted data
constructor* `'Unit` (of kind `Unit`.) This is a subtle point, and can
often lead to inscrutable compiler errors; while it's fine to ask for values of
type `Maybe Unit`, it's a *kind error* to ask for `Maybe
'Unit`---because `'Unit` is the wrong kind!

Let's return to the question of the importance of data kinds. Type-level
programming in Haskell without them is equivalent to programming in a
dynamically typed language. By default, having every kind you manipulate be
`Type` is a lot like having all of your terms be of the same type.

While types don't let you do anything you couldn't otherwise, they sure make it
easier to reason about your program! Data kinds are exactly the same---as we
write more and more interesting type-level programs, we'll use kind signatures
to restrict the sorts of types we can be dealing with.

Promoted data constructors are of the wrong kind to ever exist at runtime, which
raises the question "what good are they?" It's a little too soon to answer
this in full glory, but without any other fancy type-level machinery, we can use
them as phantom parameters.

Imagine an application for managing sensitive data, which has built-in
administrator functionality. Because it would be particularly bad if we
accidentally leaked admin functionality to non-admins, we decide to turn a
business logic error into a type error and ask the type system for help.

We can provide a `UserType` type, whose only purpose is to give us access to
its promoted data constructors.

[code/Kinds.hs:UserType](Snip)

Then, we can change our `User` type so that each user potentially has an
administration token:

[code/Kinds.hs:User](Snip)

And finally, we make the sensitive operations require a copy of this
administration token.

[code/Kinds.hs:doSensitiveThings](Snip)

This minor change will cause a type error whenever `doSensitiveThings`{.haskell} is
called without an administration token. Such an approach makes it much harder to
accidentally call `doSensitiveThings`{.haskell}. More refined techniques (such as the
ST trick, discussed @Sec:ST trick) can be used to prevent
programmers from simply conjuring up an admin token whenever they might
like---requiring `doSensitiveThings`{.haskell} to be called on behalf of an
actual administrator `User`.


### Promotion of Built-In Types

```necessary-Imports
[code/Kinds.hs:typelits](Snip)
```

With `-XDataKinds` enabled, almost all types automatically promote to kinds,
including the built-in ones. Since built-in types (strings, numbers, lists and
tuples) are special at the term-level---at least in terms of syntax---we should
expect them to behave oddly at the type-level as well.

When playing with promoted built-in types, it's necessary to first import the
`GHC.TypeLits` module. `GHC.TypeLits` defines the kinds themselves, as
well as all of the useful type families for manipulating them. We'll cover this
idea in more detail soon.


#### Symbols

The promoted version of a `String` is called a `Symbol`. `Symbol`s
are not lists of characters. Symbol type literals can be written by just using a
string literal in a place where a type is expected. For example:

```{ghci=code/Kinds.hs}
:set -XDataKinds
:kind "hello"
```

It's somewhat frustrating that `Symbol`s are not merely lists of promoted
characters; it means that `Symbol`s are no longer inductive types. It's
impossible to deconstruct a `Symbol`, although we are capable of
concatenating them via a magic `AppendSymbol` primitive provided in
`GHC.TypeLits`.

```{ghci=code/Kinds.hs}
:set -XDataKinds
:kind AppendSymbol
:kind! AppendSymbol "thinking" " with types"
```

Additionally, we are capable of comparing `Symbol`s via the `CmpSymbol`
primitive.

```{ghci=code/Kinds.hs}
@import Prelude
:kind CmpSymbol
:kind! CmpSymbol "sandy" "sandy"
:kind! CmpSymbol "sandy" "batman"
```

Notice that `CmpSymbol` is of kind `Symbol -> Symbol -> Ordering`. This
`Ordering` is just the `-XDataKinds` promoted version of the standard
`Ordering` type from `Prelude`.


#### Natural Numbers

The promotion of numbers is a little more odd. Only the natural numbers ($0, 1,
2, \ldots$) can be promoted---there are no negative, fractional nor floating
type-level numeric literals. These natural numbers, naturally enough, are of
kind `Nat`.

```{ghci=code/Kinds.hs}
:kind 5085072209
```

`GHC.TypeLits` defines primitives for performing arithmetic on `Nat`s,
with exactly the same symbolic identifiers you'd expect them to have. Using them
will require enabling `-XTypeOperators`.

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

<ul>
  * `'[]` of kind `[a]`
  * `'(:)` of kind `a -> [a] -> [a]`; used infix as `x ': xs`
</ul>

Note that although we haven't yet talked about kind-level polymorphism (things
of kind `a`), it is meaningful and corresponds exactly to your intuition
about how polymorphism should behave.

When compared against the data constructors of lists, `[] :: [a]`{.haskell} and `(:)
:: a -> [a] -> [a]`{.haskell}, with a little concentration, the promoted data constructors
should make sense.  Because lists' data constructors have symbolic names, they
also require `-XTypeOperators` enabled to be used. Don't worry though, GHC
will helpfully remind you if you forget.

There is another subtle point to be noted when dealing with list-kinds. While
`[Bool]` is of kind `Type` and describes a term-level list of booleans,
the type `'[Bool]` is of kind `[Type]` and describes a type-level list
with one element (namely, the type `Bool`.) Compare:

```{ghci=code/Kinds.hs}
:kind [Bool]
:kind '[Bool]
```

Further care should be taken when constructing a promoted list; due to the way
GHC's lexer parses character literals (`'a'`{.haskell}), make sure you add a space
after starting a promoted list. While `'[ 'True ]` is fine, `'['True]` is
unfortunately a parse error.

```{ghci=code/Kinds.hs}
:kind '[ 'True ]
:kind '['True]
```

This quirk of the lexer often bites beginners---if you get an unexpected syntax
error when dealing with type-level literals, it's likely caused by this.


#### Tuples

Tuples also promote in a straightforward way, via the `'(,)`
constructor.

```{ghci=code/Kinds.hs}
:kind '(2, "tuple")
```

Tuples are promoted with a leading tick. The aforementioned parsing gotcha
applies here as well, so be careful.


### Type-Level Functions



Where `-XDataKinds` really begins to shine, however, is through the
introduction of closed type families. You can think
of closed type families as *functions at the type-level.* In fact, we've
looked at quite a few in this chapter already. Each of those "primitives" I
mentioned earlier---`CmpSymbol`, `Div`, and etc.---are all closed type
families.

The ability to write closed type families isn't merely one bestowed upon GHC
developers, however. We are capable of writing our own too! But first, compare
the regular, term-level function `or`{.haskell}, which computes the boolean OR of two
`Bool`s:

[code/Kinds.hs:or](Snip)

Unlike data constructors, we're unfortunately unable to automatically promote
term-level functions into type-level ones. However, after enabling
`-XTypeFamilies`, we can instead "promote" `or`{.haskell} by explicitly duplicating
this logic and writing a completely separate, closed type family.

[code/Kinds.hs:TFOr](Snip)

Line for line, `or`{.haskell} and `Or` are analogous. The closed type family `Or`
requires a capital letter for the beginning of its name, because it exists at
the type-level, and besides having a more verbose kind signature, the two
definitions proceed almost exactly in lockstep.

```exercise
Write a closed type family to compute `Not`.
```

```solution
  [code/Kinds.hs:Not](Snip)
```


While the metaphor between type families and functions is enticing, it isn't
entirely *correct.* The analogues break down in several ways, but the most
important one is that *type families must be saturated.* Another way of
saying this is that all of a type family's parameters must be specified
simultaneously; there is no currying available.

Recall the `map`{.haskell} function:

[code/Kinds.hs:map](Snip)

We're capable of promoting `map`{.haskell} to the type-level:

[code/Kinds.hs:Map](Snip)

But because we're unable to partially apply closed type families, `Map`
doesn't turn out to be particularly useful.

```{ghci=code/Kinds.hs}
@import Data.Proxy
:t undefined :: Proxy (Map (Or 'True) '[ 'True, 'False, 'False ])
```

This error is trying to tell us is that we used the `Or` closed type-family
without first saturating it---we only passed it one parameter instead of the two
it requires, and so unfortunately GHC refuses to compile this program.

There is nothing preventing us from writing `Map`, but its usefulness in this
form is severely limited. We are simply unable to curry closed type families,
and so we can't use `Map` to perform any interesting type-level computations
for us. We will later explore some techniques for working around this
unfortunate limitation when we discuss *first class families* in chapter 10.



Before leaving this topic, let's look again at our definition of `Or`. Pay
close attention to its kind signature. We write it as `Or (x ::
`Bool`) (y :: `Bool`) :: `Bool``, rather than `Or x y ::
`Bool -> Bool -> Bool``. The kinds of type families are tricky beasts;
the kind you write after the `::` is the kind of the type *returned*
by the type family, *not* the kind of the type family itself.

[code/Kinds.hs:Foo](Snip)
[code/Kinds.hs:Bar](Snip)

Take a moment to think about the kinds of `Foo` and `Bar`. While `Foo`
has kind `Bool -> Bool -> Bool`, `Bar` has kind `Type -> Type -> Bool
-> Bool -> Bool`. GHCi agrees with our assessment:

```{ghci=code/Kinds.hs}
:kind Foo
:kind Bar
```

We will discuss type families in more generality in a later chapter; for now
it's sufficient to think of closed type families as type-level functions.


