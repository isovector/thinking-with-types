
## Constraints and GADTs


### Introduction

`kind:Constraint`s are odd. They don't behave like `Type`s nor like promoted
data kinds. They are a fundamentally different thing altogether, and thus worth
studying.

The `kind:Constraint` kind is reserved for things that can appear on the left
side of the fat context arrow (`=>`). This includes fully-saturated typeclasses
(like `Show a`), tuples of other `kind:Constraint`s, and type equalities (`Int ~
a`.) We will discuss type equalities in a moment.

Typeclass constraints are certainly the most familiar. We use them all the time,
even when we are not writing type-level Haskell. Consider the equality function
`(==) :: Eq a => a -> a -> Bool`. Tuples of `kind:Constraint`s are similarly
well-known: `sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)`.

Type equalities are more interesting, and are enabled via `-XGADTs`. Compare the
following two programs:

[code/Constraints.hs:five](Snip)

[code/Constraints.hs:five_](Snip)

Both `five` and `five_` are identical as far as Haskell is concerned.  While
`five` has type `Int`, `five_` has type `a`, along with a constraint saying that
`a` equals `Int`. Of course, nobody would actually write `five_`, but it's a
neat feature of the type system regardless.

Type equalities form an equivalence relation, meaning that they have the
following properties:

* reflexivity---a type is always equal to itself: `a ~ a`
* symmetry---`a ~ b` holds if and only if `b ~ a`
* transitivity---if we know both `a ~ b` and `b ~ c`, we (and GHC) can infer
  that `a ~ c`.


### GADTs

Generalized algebraic datatypes (GADTs; pronounced "gad-its") are an extension
to Haskell's type system that allow explicit type signatures to be written for
data constructors. They, like type equality constraints, are also enabled via
`-XGADTs`.

The canonical example of a GADT is a type safe syntax tree. For example, we can
declare a small language with integers, booleans, addition, logical negation,
and if statements.

[code/GADTs.hs:Expr](Snip)

The `where` at [1](Ann) is what turns on GADT syntax for the rest of the
declaration. Each of `LitInt`, `LitBool`, `Add`, etc. corresponds to a data
constructor of `Expr`. These constructors all take some number of arguments
before resulting in an `Expr`.

For example, `LitInt` at [2](Ann) takes an `Int` before returning a `Expr Int`.
On the other hand, the data constructor `If` at [3](Ann) takes three arguments
(one `Expr Bool` and two `Expr a`s) and returns an `Expr a`.

It is this ability to specify the return type that is of particular interest.

You might be pleased that `Expr` is *correct by construction.* We are incapable
of building a poorly-typed `Expr`. While this might not sound immediately
remarkable, it is---we've reflected the *typing rules of `Expr`* in the type
system of Haskell. For example, we're unable to build an AST which attempts to
add an `Expr Int` to a `Expr Bool`.

To convince ourselves that the type signatures written in GADT syntax are indeed
respected by the compiler, we can look in GHCi:

```{ghci=code/GADTs.hs}
:t LitInt
:t If
```

Because GADTs allow us to specify a data constructor's type, we can use them to
*constrain* a type variable in certain circumstances. Such a thing is not
possible otherwise.[^type-equalities]

[^type-equalities]: Or equivalently---as we will see---without type equalities.

The value of GADTs is that Haskell can use the knowledge of these constrained
types. In fact, we can use this to write a typesafe evaluator over `Expr`:

[code/GADTs.hs:evalExpr](Snip)

In just this amount of code, we have a fully functioning little language and
interpreter. Consider:

```{ghci=code/GADTs.hs}
evalExpr . If (LitBool False) (LitInt 1) . Add (LitInt 5) $ LitInt 13
evalExpr . Not $ LitBool True
```

Pay careful attention here! At [1](Ann), `evalExpr` returns an `Int`, but at
[2](Ann) it returns a `Bool`! This is possible because Haskell can *reason*
about GADTs. In the `LitInt` case, the only way such a pattern could have
matched is if `a ~ Int`, in which case it's certainly okay to return a `Int`.
The reasoning for the other patterns is similar; Haskell can use information
from inside a pattern match to drive type inference.

GADT syntax is indeed provided by `-XGADTs`, but it is not the syntax that is
fundamentally interesting. The extension is poorly named---a more appropriate
name might be "`-XTypeEqualities`". In fact, GADTs are merely syntactic sugar
over type equalities. We can also declare `Expr` as a traditional Haskell
datatype as follows:

[code/GADTs.hs:Expr_](Snip)

When viewed like this, it's a little easier to see what's happening behind the
scenes. Each data constructor of `Expr_` carries along with it a type equality
constraint. Like any constraint inside a data constructor, Haskell will require
the constraint to be proven when the data constructor is called.

As such, when we pattern match on a data constructor which contains a
constraint, this satisfied constraint *comes back into scope.* That is, a
function of type `Expr a -> a` can return an `Int` when pattern matching on
`LitInt`, but return a `Bool` when matching on `LitBool`. The type equality
constraining `a` only comes back into scope after pattern matching on the data
constructor that contains it.

We will explore the technique of packing constraints inside data constructors in
much greater generality later.

Though GADT syntax doesn't offer anything novel, we will often use it when
defining complicated types. This is purely a matter of style as I find it more
readable.


### War Story: Efficient Queries {.rev2}

During a much-needed vacation from writing advertising software, I decided to
program a video game instead. The idea was to shamelessly reimplement a favorite
game from my childhood, which was a nice middle-ground between having something
fun to write, without needing to think deeply about the non-programming
concerns. Before making the transition to Haskell, I'd made games both as an
amateur and in a professional capacity, so the lay of the land was already quite
familiar.

In fact, this wasn't even my first foray into game programming in Haskell. A few
years earlier I'd built many of the tools I'd need for a game engine---rendering
machinery, and an implementation of an entity-component system (ECS). My plan
was to reuse these pieces, and just write the game on top.

In essence, an ECS is a specialized database for video games. The idea is that
you can implement game objects (the entities) as (possibly empty) "bags" of
components. Rather than programming entity behavior directly, instead you write
transformations over their components. For example, you might have a `position
:: R2` and a `velocity :: R2` component. Entities may or may not have the
`position` component, but those that do will appear on the screen. Now, if we
wanted to implement movement, we could write a transformation over the
`position` and `velocity` components:

```haskell
emap $ do
  p <- get position
  v <- get velocity
  pure defEnt'
    { position = Set $ p + v
    }
```

Every time this transformation runs, any entity that has both the `position` and
`velocity` components set will see its `velocity` added to its `position`. Any
entities that are missing either component will be left alone, unchanged.

The end result of building a game as an ECS is that you get a nice, composable
system. All that's necessary is to define meaningful transformations over
components, and the resulting world is full of consistent, repeatable
interactions.

Back to the story. After a few days of hacking, I had quite a nice little
prototype of my game. I had a few little people who would move around and
interact with one another. Everything looked good in my limited tests. But the
game was supposed to be a war simulator, with hundreds of soldiers and thousands
of bullets flying around---each one an entity. When I cranked up the number of
objects I was creating, everything ground to a halt. My implementation was too
slow!

After some profiling, I found that the root cause was that my old ECS library
was dead slow, due to many bad assumptions. A common implementation of an ECS is
to implement entities as as *map from components to entities*, rather than the
more usual map from records to components. In a system with many interactions,
most entities will have a sparse collection of components, and so keeping them
denormalized helps cut down on memory usage. Furthermore, this representation
allows for extremely efficient evaluation of queries: if we want to run the
*position/velocity* rule above, we don't need to look at every entity---only
those in the intersection of the `position` and `velocity` maps.

So what was going wrong? The problem was that `Query` was a monad, which meant
it was impossible to do static analysis of the components it required. Knowing
that this was going to be a performance bottleneck, my library cheated and asked
you for an `IdTarget` every time you ran a `Query`. The idea was that the
`IdTarget` describes which entities the system needs to look at.

[code/War/Ecstasy.hs:allIds](Snip)

The result was an awful mess. If you wanted to be performant, you needed to
duplicate the majority of your query logic in the `IdTarget`. And if they ever
got out of sync, the game would start exhibiting bugs! Fearing that, I had
simply used the `allIds` target for every transformation, meaning that every
interaction needed to traverse every entity in the game---a sure-fire recipe for
$O(\text{awful})$ performance.

My first (unwise) attempt at a solution was to try to "plumb" my way through the
`Query`, and generate an optimistic `IdTarget` based on how far I got before the
thing inevitably forked. It was complicated and unreliable, and thus destined to
failure.

Upon deep thought, I realized the issue was that `Query` was a monad (it wasn't
as obvious as I've presented here!) What I really wanted was a data structure
that I could inspect when running queries to find an optimally small set of
`Id`s necessary to traverse. The challenge was the different ways of building
queries result in different types being queried!

Aha! But what about using GADTs? Then I could build a inspectable data
structure, and use the type index to describe the result of the `Query`! After
carefully mapping out all of the different ways I wanted to be able to query,
and their respective types, I wrote the following definition:

[code/War/Ecstasy.hs:Query](Snip)

Having reified `Query` as a data structure instead of a program, it was now
possible to write different "interpretations" of it. One those those
interpretations was to synthesize the `IdTarget`:

[code/War/Ecstasy.hs:findRelevant](Snip)

This was already a significant improvement! But I realized I could also
interpret the `Query` to determine if I needed to run it at all! Perhaps the
thing always resulted in a constant value, in which case I could save a lot of
work by computing it once and pushing the result everywhere necessary:

[code/War/Ecstasy.hs:constantValue](Snip)

Here you can see the GADT doing work---each branch of `constantValue` returns a
different type, depending on which constructor we've pattern matched over.

To put everything together, I added `Functor` and `Applicative` instances for
`Query`:

[code/War/Ecstasy.hs:QueryFunctor](Snip)

[code/War/Ecstasy.hs:QueryApplicative](Snip)

After turning on `-XApplicativeDo`, all of my old transformations continued to
work as written---except that now everything ran asymptotically faster, thanks
to the optimization power possible by expecting the `Query` GADT.


### Heterogeneous Lists

One of the primary motivations of GADTs is building inductive type-level
structures out of term-level data. As a working example for this section, we can
use GADTs to define a heterogeneous list---a list which can store values of
different types inside it.

To get a feel for what we'll build:

```{ghci=code/GADTs.hs}
:t HNil
:t True :# HNil
let hlist = Just "hello" :# True :# HNil
:t hlist
hLength hlist
```

The `HNil` constructor here is analogous to the regular list constructor `[]`.
`(:#)` likewise corresponds to `(:)`. They're defined as a GADT:

[code/GADTs.hs:HList](Snip)

At [1](Ann), you'll notice that we've given `HList`'s `ts` an explicit kind
signature.  The type parameter `ts` is defined to have kind `[Type]`, because
we'll store the contained types inside of it. Although this kind signature isn't
strictly necessary---GHC will correctly infer it for us---your future self will
appreciate you having written it. A good rule of thumb is to annotate *every*
kind if *any* of them isn't `Type`.

`HList` is analogous to the familiar `[]` type, and so it needs to define an
empty list at [2](Ann) called `HNil`, and a cons operator at [3](Ann) called
`(:#)`.[^type-operators] These constructors have carefully chosen types.

[^type-operators]: Symbolically-named data constructors in Haskell must begin
  with a leading colon. Anything else is considered a syntax-error by the
  parser.

`HNil` represents an empty `HList`. We can see this by the fact that it takes
nothing and gives back `ts ~ '[]`---an empty list of types.

The other data constructor, `(:#)`, takes two parameters. Its first is of type
`t`, and the second is a `HList ts`. In response, it returns a `HList (t ':
ts)`---the result is this new type has been consed onto the other `HList`.

This `HList` can be pattern matched over, just like we would with regular lists.
For example, we can implement a length function:

[code/GADTs.hs:hLength](Snip)

But, having this explicit list of types to work with, allows us to implement
much more interesting things. To illustrate, we can write a *total* `head`
function---something impossible to do with traditional lists.

[code/GADTs.hs:hHead](Snip)

The oddities don't stop there. We can deconstruct any length-3 `HList` whose
second element is a `Bool`, show it, and have the compiler guarantee that this
is an acceptable (if strange) thing to do.

[code/GADTs.hs:showBool](Snip)

Unfortunately, GHC's stock deriving machinery doesn't play nicely with
GADTs---it will refuse to write `Eq`, `Show` or other instances. But we can
write our own by providing a base case (for `HNil`), and an inductive case.

The base case is that two empty `HList`s are always equal.

[code/GADTs.hs:eqHNil](Snip)

And inductively, two consed `HList`s are equal only if both their heads and
tails are equal.

[code/GADTs.hs:eqHCons](Snip)

Exercise

:   Implement `Ord` for `HList`.


Solution

:   [code/GADTs.hs:ordHNil](Snip)

    [code/GADTs.hs:ordHCons](Snip)


Exercise

:   Implement `Show` for `HList`.

Solution

:   [code/GADTs.hs:showHNil](Snip)

    [code/GADTs.hs:showHCons](Snip)


The reason we had to write two instances for `Eq` was to assert that every
element in the list also had an `Eq` instance. While this works, it is rather
unsatisfying. Alternatively, we can write a closed type family which will fold
`ts` into a big `kind:Constraint` stating each element has an `Eq`.

[code/GADTs.hs:AllEq](Snip)

As `AllEq` is our first example of a non-trivial closed type family, we should
spend some time analyzing it. `AllEq` performs type-level pattern matching on a
list of types, determining whether or not it is empty.

If it is empty---line [1](Ann)---we simply return the unit `kind:Constraint`.
Note that because of the kind signature on `AllEq`, Haskell interprets this as
`kind:Constraint` rather than the unit `Type`.

However, if `ts` is a promoted list cons, we instead construct a
`kind:Constraint`-tuple at [2](Ann). You'll notice that `AllEq` is defined
inductively, so it will eventually find an empty list and terminate. By using
the `:kind!` command in GHCi, we can see what this type family expands to.

```{ghci=code/GADTs.hs}
:kind! AllEq '[Int, Bool]
```

`AllEq` successfully folds `[Type]`s into a `kind:Constraint`. But there is
nothing specific to `Eq` about `AllEq`! Instead, it can be generalized into a
fold over any `kind:Constraint` `c`. We will need `-XConstraintKinds` in order
to talk about polymorphic constraints.

[code/GADTs.hs:All](Snip)

With `All`, we can now write our `Eq` instance more directly.

[code/GADTs.hs:eqHList](Snip)

Exercise

:   Rewrite the `Ord` and `Show` instances in terms of `All`.

Solution

:   [code/GADTs.hs:ordHList](Snip)

    [code/GADTs.hs:showHList](Snip)

