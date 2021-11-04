## Dependent Types

Dependent types are types which depend on the result of *runtime values.* This
is an odd thing. In Haskell, traditionally terms and types on different sides of
the phase barrier; terms exist only at runtime, and types only at compile-time.

Proponents of dynamic typing are likely less phased by the idea of dependent
types---their typing mechanisms, if they have any at all, must exist at runtime.
But as we will see, we don't need to give up type safety in order to work with
dependent types.

The field of dependent types is fast-growing, and any attempt to describe it
definitively is doomed to fail. Professor and GHC-contributor Richard Eisenberg
is actively working on bringing first-class dependent types to Haskell, though
his projections estimate it to not be available for a few years.

In the meantime, we have no recourse but to abuse language features in order to
gain an approximation of dependent types, content with the knowledge that the
process will only get easier as time goes on. While the *techniques* in this
section will likely be deprecated sooner than later, the *ideas* will carry on.

It's also a good culmination of ideas already presented in this book. The
motivated reader can use this section as a test of their understanding of many
disparate pieces of the type system, including rank-*n* types, GADTs, type
families, and data kinds, among others.


### Overview

Dependent types in Haskell can be approximated via singletons, which are to be
understood as an isomorphism between terms and types.

Consider the unit type `()`, whose only inhabitant is the unit value `()`.  This
type has an interesting property; if you know a value's type is unit, you know
what the value must be. The type `()` is a singleton, because it only has a
single term. Furthermore, because of this one-to-one representation, we can
think about the unit type as being able to cross the term--type divide at will.

Singleton types are just this idea taken to the extreme; for every inhabitant of
a type, we create a singleton type capable of bridging the term--type divide.
The singleton allows us to move between terms to types and vice versa. As a
result, we are capable of moving types to terms, using them in regular
term-level Haskell computations, and then lifting them back into types.

Notice that data kinds already give us type-level representations of terms;
recall that `'True` is the promoted data constructor of `True`.


### Ad-Hoc Implementation

To wet our whistles, let's first look at a simple implementation of singletons.
We begin with the singletons themselves.

[code/AdHocSingletons.hs:SBool](Snip)

The data constructors `STrue` and `SFalse` act as our bridge between the term
and type-levels. Both are terms, but are the sole inhabitants of their types. We
thus have isomorphisms `STrue` &cong; `'True` and `SFalse` &cong; `'False`.  The
next step is to find an isomorphism between `Bool` &cong; `SBool b`. This is
easy in one direction.

[code/AdHocSingletons.hs:fromSBool](Snip)

The converse however is trickier. Because `SBool 'True` is a different type than
`SBool 'False`, we are unable to directly write the other side of the
isomorphism. Instead, we introduce an existential wrapper `SomeSBool` and
eliminator.

[code/AdHocSingletons.hs:SomeSBool](Snip)

[code/AdHocSingletons.hs:withSomeSBool](Snip)

`toSBool` can now be written in terms of `SomeSBool`.

[code/AdHocSingletons.hs:toSBool](Snip)

It makes intuitive sense that `toSBool` would need to return an existential
type, as there is no guarantee its arguments are known at compile-time. Perhaps
the `Bool` being singletonized came from the user, or across the network. We can
convince ourselves that `fromSBool` and `toSBool` behave sanely.

```{ghci=code/AdHocSingletons.hs}
withSomeSBool (toSBool True)  fromSBool
withSomeSBool (toSBool False) fromSBool
```

As an example of the usefulness of singletons, we will build a monad stack which
can conditionally log messages. These messages will only be for debugging, and
thus should be disabled for production builds. While one approach to this
problem is to simply branch at runtime depending on whether logging is enabled,
this carries with it a performance impact. Instead, we can conditionally choose
our monad stack depending on a runtime value.

We begin with a typeclass indexed over `Bool`. It has an associated type family
to select the correct monad stack, as well as a few methods for working with the
stack.

[code/AdHocSingletons.hs:MonadLogging](Snip)

The `| r -> b` notation at [2](Ann) is known as a type family dependency and
acts as an injectivity annotation. In other words, it tells Haskell that if it
knows `LoggingMonad b` it can infer `b`. The `'False` case is uninteresting, as
it ignores any attempts to log messages.

[code/AdHocSingletons.hs:MonadLoggingFalse](Snip)

However, in the case of `'True`, we'd like to introduce a `WriterT [String]`
transformer over the monad stack. Running a `LoggingMonad 'True` should print
out all of its logged messages after running the program.

[code/AdHocSingletons.hs:MonadLoggingTrue](Snip)

With this machinery in place, we are capable of writing a program that logs
messages. When invoked in `LoggingMonad 'False` it should have no side effects,
but when run in `LoggingMonad 'True` it will print `"hello world"`.

[code/AdHocSingletons.hs:program](Snip)

[code/AdHocSingletons.hs:badMain](Snip)

This function reads a `Bool` from stdin, and lifts it into a singleton at
[1](Ann). The resulting existential type `b` is bound at [2](Ann) and
type-applied at [3](Ann) in order to tell Haskell which monad stack it should
use to run `program`.

Unfortunately, `main` fails to compile.

```
<interactive>:87:5: error:
    • No instance for (MonadLogging b)
        arising from a use of ‘runLogging’
```


The problem is that typeclasses are implemented in GHC as implicitly passed
variables. By [3](Ann), GHC doesn't know the type of `b`, and thus can't find
the appropriate `MonadLogging` dictionary---even though `MonadLogging` is total
and theoretically GHC should be able to determine this.

We can help GHC along by writing a function that can deliver dictionaries for
any constraint that is total over `Bool`.

[code/AdHocSingletons.hs:dict](Snip)

`dict` works by requiring some `Bool -> Constraint` to be satisfied for both
`'True` and `'False` ([1](Ann)). It then takes a singleton ([2](Ann)) and uses
that to deliver the appropriate `Dict (c b)` by branching at [3](Ann). The fact
that both branches simply return `Dict` might be confusing---but remember that
GHC infers different types for them due to the type-equality implicit in the
GADT match. `dict` can be invoked with a `Bool -> Constraint` type-application
to select the desired constraint.

Finally, we can write a working `main` which acquires the correct typeclass
dictionary using the singleton it lifted.

[code/AdHocSingletons.hs:main](Snip)

```{ghci=code/AdHocSingletons.hs}
main True
main False
```

Here we have provided an isomorphism `Bool` &cong; `SBool a` and from `SBool a`
&cong; `a :: Bool`. Because isomorphisms are transitive, this means we have the
desired correspondence between terms and types as witnessed by `Bool` &cong; `a
:: Bool`.


### Generalized Machinery

While the `SBool` approach works well enough for lifting `Bool`s, it's not
immediately evident how to generalize the pattern to be ergonomic when working
with many different types.

We begin with a poly-kinded open data family, which is responsible for indexing
the equivalents of `SBool`.

[code/Singletons.hs:Sing](Snip)

`SomeSing` and its eliminator carry over as before.

[code/Singletons.hs:SomeSing](Snip)

[code/Singletons.hs:withSomeSing](Snip)

However, it is more ergonomic to package together `toSing` and `fromSing` into a
typeclass rather than be independent functions.

[code/Singletons.hs:SingKind](Snip)

Notice that at [2](Ann), the type variable `k` is used *both as a type and as a
kind.* This requires the `-XTypeInType` language extension, which removes the
distinction between types and kinds. Additionally, it makes the type `Type` an
inhabitant of `Type` (which is now the same thing as `Type` itself!)
Mathematician readers will likely fear the unsoundities resulting from this
change---but Richard Eisenberg, the author of this feature, assures us these
paradoxes are not observable in Haskell.

The associated type family `Demote k` ([1](Ann)) is an implementation detail.
It is almost always equal to `k`, except in cases when GHC already provides a
type literal (for `Nat` and `Symbol`.) A type family dependency is added to
`Demote`, allowing GHC to infer `k` from `Demote k`.

Instances of `Sing` and `SingKind` are trivially provided for `Bool`.

[code/Singletons.hs:SingBool](Snip)

[code/Singletons.hs:SingKindBool](Snip)

This machinery is enough to recover our previous round-trip examples.

```{ghci=code/Singletons.hs}
withSomeSing (toSing True) fromSing
withSomeSing (toSing False) fromSing
```

Because singletons are the unique inhabitant of their types, at the term-level
they are isomorphic with `()`. Therefore, we expect to be able to get this
unique inhabitant, in the same way we can always conjure a `()` out of thin air.

[code/Singletons.hs:SingI](Snip)

Instances of `SingI` are predictably uninteresting.

[code/Singletons.hs:SingITrue](Snip)

[code/Singletons.hs:SingIFalse](Snip)

The `sing` function can be used with `-XTypeApplications` in order to retrieve
the desired singleton.

```{ghci=code/Singletons.hs}
:t sing @'True
```

Singletons can also be provided for more interesting types. For example, if we
have singletons for `a`, we can build singletons for `Maybe a`.

[code/Singletons.hs:SingMaybe](Snip)

[code/Singletons.hs:SingIJust](Snip)

[code/Singletons.hs:SingINothing](Snip)

[code/Singletons.hs:SingKindMaybe](Snip)

Besides some relatively tricky wrapping of existentials, there is nothing new or
interesting in this code. If a given data constructor is built out of other
pieces of data, its singletons are built out of the analogous singletons, and
its data kinds out of the analogous promoted data constructors.

To get a feel for this transformation, we can also build singletons for lists.

[code/Singletons.hs:SingList](Snip)

[code/Singletons.hs:SingKindList](Snip)

Exercise

:   Provide instances of `SingI` for lists.

Solution

:   [code/Singletons.hs:SingINil](Snip)

    [code/Singletons.hs:SingICons](Snip)


### The Singletons Package

The construction of singletons is entirely mechanical. The `singletons`
@cite:singletons package provides TemplateHaskell capable of writing singletons
for us (as well as automatically promoting term-level functions.)

By enabling `-XTemplateHaskell` and the other slew of extensions listed above,
`singletons` is capable of generating singletons for us. Data definitions can be
wrapped in the definitions quasiquoter `[d| ... |]`, and run via the
`singletons` function.

[code/SingletonsTH.hs:TimeOfDay](Snip)

The resulting splice will be completely equivalent to our hand-rolled code
above, including instances for `Sing`, `SingKind` and `SingI`. In addition, if
the definitions have `Eq` instances, `singletons` will also generate `SDecide`
instances for them.

[code/Singletons.hs:SDecide](Snip)

`SDecide` is nominal equality for singletons; if two `Sing`s of the same kind
are equal, `SDecide` allows us to use that fact. Recall the definition of `a :~:
b`, which is only constructable if `a ~ b`:

[code/Misc.hs:Refl](Snip)

`Decision` is defined as:

[code/Singletons.hs:Decision](Snip)

The type `a -> Void` at [1](Ann) is the Curry--Howard encoding of logical
false---because `Void` is uninhabited, it can't be constructed. Thus, a function
that produces `Void` must not be callable, since it can't ever produce a `Void`;
and the only way a function isn't callable is if its domain is also uninhabited.

A "free" instance of `SDecide` can be given for all types with well-behaving
`Eq` instances by "cheating" with our equality proofs. The desired `a ~ b` proof
can be generated via `unsafeCoerce`, which is safe due to our term-level
equality check.

[code/Singletons.hs:FreeSDecide](Snip)

This instance doesn't actually exist in `singletons`, but it's a nice
demonstration of the old motto "if it's too hard at the type-level, do it at the
term-level."

Of course, real instances can be provided as well.

[code/Singletons.hs:SDecideBool](Snip)

Exercise

:   Give instances of `SDecide` for `Maybe`.

Solution

:   [code/Singletons.hs:SDecideMaybe](Snip)


### Dependent Pairs

Sigma types also known as dependent pairs, generalize arbitrarily-deeply nested
`Either` types parameterized by a type. When viewed through the lens of the
Curry--Howard isomorphism, they correspond to the existential quantifier
$\exists$.

What does this mean? Sigma types are the pair of an existential singleton and *a
type indexed by that singleton.* Consider the definition:

[code/Sigma.hs:Sigma](Snip)

`Sigma` takes a singleton for an existential type `a` (of kind `k`), and
datatype `f a`. Notice that `f a` is parameterized on the existential type `a`;
as a result, `Sigma f` might be any number of possible `f a`s. However, `a` is
not truly existential, as we have its singleton to work with.

As usual, we can provide an eliminator for `Sigma`.

[code/Sigma.hs:withSigma](Snip)

But, using `SingI`, we can also provide an introduction for `Sigma`. `toSigma`
lifts an arbitrary `f a` into a `Sigma f`.

[code/Sigma.hs:toSigma](Snip)

The singleton inside `Sigma` can additionally be used to help cast a `Sigma f`
back into a `f a`; assuming the type inside is the same one being requested.
This is done via `SDecide` to convince the type system such a cast is legal.

[code/Sigma.hs:fromSigma](Snip)

By pattern matching on `Refl` at [1](Ann), GHC learns that `a ~ t`, where `t` is
the "existential" type inside of the `Sigma`. With this equality in hand, it's
clearly safe to return the `f t` when asking for a `f a`.

The `dict` function from our logging example can also be generalized into a
typeclass capable of providing total constraints given a singleton.

[code/Sigma.hs:Dict1](Snip)

`c` has kind `Type -> Constraint` and `f` has kind `k -> Type`.  Since we have
`a` with kind `k` from the singleton, this chain of constraints can be traversed
to get a `Constraint`. Given a singleton, `dict1` will provide the requested
dictionary, and it will fail at compile-time if the requested constraint isn't
total over `f`.

A `Dict1` instance can be used to lift `Eq` over `Sigma`.

[code/Sigma.hs:EqSigma](Snip)

The kind signature at [1](Ann) is required to associate `k` and `f`.

`Dict1` can also be used to lift other instances, for example, `Show`.

[code/Sigma.hs:ShowSigma](Snip)

Exercise

:   Provide an instance of `Ord` for `Sigma` by comparing the `f`s if the
    singletons are equal, comparing the singletons at the term-level otherwise.

Solution

:   [code/Sigma.hs:OrdSigma](Snip)


#### Structured Logging

As an example of what good are singletons, consider the common use-case from
dynamically-typed languages. Often, protocols will attempt ad-hoc type-safety by
taking two parameters: the first being a string describing the type of the
second.

This is a clear use for dependent types; the type of the second argument depends
on the value of the first. In particular, it is a `Sigma` type, because once we
know the type of the second parameter, the first is no longer of interest to us.

To demonstrate the technique, consider the case of structured logging.  When we
have information we'd like to log, rather than emitting it as a string, we can
instead log a datastructure. The resulting log can then be interpreted as a
string, or it can be mined for more structured data.

For this example, we will assume we have two sorts of data we'd like to be able
to log: strings and JSON. The approach can be generalized to add more types
later, and the compiler's warnings will drive us.

While an existentialized GADT approach might suffice---where we stuff proofs of
each typeclass into the datatype---it requires us to know all of our desired
dictionaries in advance. Such an approach isn't particularly extensible, since
it requires us to anticipate everything we might like to do with our log.

Instead, we can pack this structured data into a `Sigma`, and use the `Dict1`
class to retrieve dictionaries as we need them.

We can begin by writing an enum describing which sort of data we want to log.

[code/Sigma.hs:LogType](Snip)

An open data family `LogMsg` of kind `LogType -> Type` can then be
defined. `LogMsg` is indexed by `LogType`, meaning we can build a
`Sigma` around it.

[code/Sigma.hs:LogMsg](Snip)

For every inhabitant of `LogType`, we add a `data instance` for
`LogMsg`, corresponding to the data we'd like to store in that case.

[code/Sigma.hs:LogMsgPayloadJson](Snip)

[code/Sigma.hs:LogMsgPayloadText](Snip)

And a `Dict1` instance can be provided.

[code/Sigma.hs:Dict1LogMsgPayload](Snip)

Now, given a list of log messages (possibly generated via `WriterT [Sigma
LogMsg]` or something similar), we can operate over them. For example, perhaps
we want to just render them to text:

[code/Sigma.hs:logs](Snip)

[code/Sigma.hs:showLogs](Snip)

```{ghci=code/Sigma.hs}
import Data.Foldable
traverse_ putStrLn (showLogs logs)
```

But this approach of structured logging also affords us more interesting
opportunities for dealing with data. For example, we can filter it, looking only
for the JSON entries.

[code/Sigma.hs:catSigmas](Snip)

[code/Sigma.hs:jsonLogs](Snip)

```{ghci=code/Sigma.hs}
show jsonLogs
```

