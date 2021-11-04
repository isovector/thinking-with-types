## First Class Families

### Defunctionalization

Until recently, it was believed that type families had no chance of being first
class; because they're unable to be partially applied, reuse and abstraction
seemed impossible goals. Every type-level `fmap` would need to be specialized
with its mapping function built-in, and there seemed no way of abstracting away
this repetitive boilerplate.

The work of Li-yao Xia @cite:fcf has relaxed this limitation, not by providing
unsaturated type families, but by giving us tools for working around them. It
works via Defunctionalization---the process of
replacing an instantiation of a polymorphic function with a specialized label
instead.

For example, rather than the function `fst`:

[code/Defunc.hs:fst](Snip)

We can instead defunctionalize it by providing an equivalent label:

[code/Defunc.hs:Fst](Snip)

All that's left to implement is the actual evaluation function. This can be
codified as a typeclass with a functional dependency to guide the return type.

[code/Defunc.hs:Eval](Snip)

The syntax `| l -> t` at [1](Ann) is known as a functional dependency,
and states that the type `t` is fully determined by the type `l`. Here,
`l` is the type of our defunctionalized label, and `t` is the return type
of the evaluation.

[code/Defunc.hs:EvalFst](Snip)

Despite being roundabout, this approach works just as well as `fst` itself
does.

```{ghci=code/Defunc.hs}
eval (Fst ("hello", True))
```

Exercise

:   Defunctionalize `listToMaybe :: [a] -> Maybe a`.

Solution

:   [code/Defunc.hs:ListToMaybe](Snip)

    [code/Defunc.hs:EvalListToMaybe](Snip)


Even higher-order functions can be defunctionalized:

[code/Defunc.hs:Map](Snip)

The `dfb` type at [1](Ann) could be labeled simply as `b`, but we will
enforce it is a defunctionalized symbol---evaluation of `MapList` will depend on
an `Eval dfb` instance. This name helps suggest that.

[code/Defunc.hs:EvalMap](Snip)

Pay attention to [1](Ann)---rather than consing `f a` to the mapped list, we
instead cons `eval (f a)`. While there is morally no difference, such an
approach allows defunctionalization of `map` to propagate evaluation of other
defunctionalized symbols. We can see it in action:

```{ghci=code/Defunc.hs}
eval (MapList Fst [("hello", 1), ("world", 2)])
```


### Type-Level Defunctionalization

This entire line of reasoning lifts, as Xia shows, to the type-level where it
fits a little more naturally. Because type families are capable of
discriminating on types, we can write a defunctionalized symbol whose type
corresponds to the desired type-level function.

These are known as first class families, or FCFs for short.

We begin with a kind synonym, `Exp a` which describes a type-level function
which, when evaluated, will produce a type of kind `a`.

[code/FCTF.hs:Exp](Snip)

This "evaluation" is performed via an open type family `Eval`. `Eval`
matches on `Exp a`s, mapping them to an `a`.

[code/FCTF.hs:Eval](Snip)

To write defunctionalized "labels", empty data-types can be used. As an
illustration, if we wanted to lift `snd` to the type-level, we write a
data-type whose kind mirrors the type of `snd`.

[code/FCTF.hs:Snd](Snip)

```{ghci=code/FCTF.hs}
:t snd
:kind Snd
```

The type of `snd` and kind of `Snd` share a symmetry, if you ignore the
trailing `-> Type` on `Snd`. An instance of `Eval` can be used to
implement the evaluation of `Snd`.

[code/FCTF.hs:EvalSnd](Snip)

```{ghci=code/FCTF.hs}
:kind! Eval (Snd '(1, "hello"))
```

Functions that perform pattern matching can be lifted to the defunctionalized
style by providing multiple type instances for `Eval`.

[code/FCTF.hs:FromMaybe](Snip)

`Eval` of `FromMaybe` proceeds as you'd expect.

```{ghci=code/FCTF.hs}
:kind! Eval (FromMaybe "nothing" ('Just "just"))
:kind! Eval (FromMaybe "nothing" 'Nothing)
```

Exercise

:   Defunctionalize `listToMaybe` at the type-level.

Solution

:   [code/FCTF.hs:ListToMaybe](Snip)


However, the real appeal of this approach is that it allows for
*higher-order* functions. For example, `map` is lifted by taking a
defunctionalization label of kind `a -> Exp b` and applying it to a
`[a]` to get a `Exp [b]`.

[code/FCTF.hs:MapList](Snip)

`Eval` of `'[]` is trivial:

[code/FCTF.hs:EvalNil](Snip)

And `Eval` of cons proceeds in the only way that will type check, by
evaluating the function symbol applied to the head, and evaluating the
`MapList` of the tail.

[code/FCTF.hs:EvalCons](Snip)

Behold! A type-level `MapList` is now *reusable*!

```{ghci=code/FCTF.hs}
:kind! Eval (MapList (FromMaybe 0) ['Nothing, ('Just 1)])
:kind! Eval (MapList Snd ['(5, 7), '(13, 13)])
```

Exercise

:   Defunctionalize `foldr :: a -> b -> b) -> b -> [a] -> b`.

Solution

:   [code/FCTF.hs:Foldr](Snip)


### Working with First Class Families

Interestingly, first-class families form a monad at the type-level.

[code/FCTF.hs:Pure](Snip)

[code/FCTF.hs:bind](Snip)

As such, we can compose them in terms of their Kleisli composition.
Traditionally the fish operator (`<=<`) is used to represent this combinator
in Haskell.  We are unable to use the more familiar period operator at the
type-level, as it conflicts with the syntax of the `forall` quantifier.

[code/FCTF.hs:kleisli](Snip)

```{ghci=code/FCTF.hs}
:set -XPolyKinds
type Snd2 = Snd <=< Snd
:kind! Eval (Snd2 '(1, '(2, 3)))
```

While `(<=<)` at the type-level acts like regular function composition
`(.)`, `(=<<)` behaves like function application `(\$)`.

```{ghci=code/FCTF.hs}
:kind! Eval (Snd <=< FromMaybe '(0, 0) =<< Pure (Just '(1, 2)))
```

The `first-class-families`@cite:fcf package provides most of `Prelude`
as FCFs, as well as some particularly useful functions for type-level
programming. For example, we can determine if any two types are the
same---remember, there is no `Eq` at the type-level.

[code/FCTF.hs:TyEq](Snip)

[code/FCTF.hs:EvalTyEq](Snip)

[code/FCTF.hs:TyEqImpl](Snip)

We also have the ability to collapse lists of `Constraint`s.

[code/FCTF.hs:Collapse](Snip)

Which leads us very naturally to:

[code/FCTF.hs:All](Snip)

[code/FCTF.hs:Pure1](Snip)

And we find ourselves with a much nicer implementation of `All` than we wrote
in chapter 5.

```{ghci=code/FCTF.hs}
:kind! Eval (All Eq '[Int, Bool])
```


### Ad-Hoc Polymorphism

Consider the following definition of `Map`.

[code/FCTF.hs:Map](Snip)

Because type families are capable of discriminating on types, we can write
several instances of `Eval` for `Map`. For example, for lists:

[code/FCTF.hs:Map4List](Snip)

And also for `Maybe`:

[code/FCTF.hs:Map4Maybe](Snip)

Why not an instance for `Either` while we're at it.

[code/FCTF.hs:Map4Either](Snip)

As you might expect, this gives us ad-hoc polymorphism for a promoted `fmap`.

```{ghci=code/FCTF.hs}
:kind! Eval (Map Snd ('Just '(1, 2)))
:kind! Eval (Map Snd '[ '(1, 2) ])
:kind! Eval (Map Snd (Left 'False))
```

Exercise

:   Write a promoted functor instance for tuples.

Solution

:   [code/FCTF.hs:MapTuple](Snip)


This technique of ad-hoc polymorphism generalizes in the obvious way, allowing
us to implement the semigroup operation `Mappend`:

[code/FCTF.hs:Mappend](Snip)

However, at first glance, it's unclear how the approach can be used to implement
`Mempty`. Type families are not allowed to discriminate on their return type.
We can cheat this restriction by muddying up the interface a little and making
the "type application" explicit. We give the kind signature of
`Mempty` as follows:

[code/FCTF.hs:Mempty](Snip)

The understanding here is that given a type of any monoidal kind `k`,
`Mempty` will give back the monoidal identity for that kind. This can be done
by matching on a rigid kind signature, as in the following instances.

[code/FCTF.hs:mempties](Snip)

