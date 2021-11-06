
## Variance

Consider the following type declarations. Which of them have viable `Functor`
instances?

[code/PosNeg.hs:T1](Snip)

[code/PosNeg.hs:T2](Snip)

[code/PosNeg.hs:T3](Snip)

[code/PosNeg.hs:T4](Snip)

[code/PosNeg.hs:T5](Snip)


Exercise

:   Which of these types are `Functor`s? Give instances for the ones that are.

Solution

:   Only `T1` and `T5` are `Functor`s.

    [code/PosNeg.hs:FunctorT1](Snip)

    [code/PosNeg.hs:FunctorT5](Snip)


Despite all of their similarities, only `T1` and `T5` are `Functor`s.
The reason behind this is one of variance: if we can transform an `a`
into a `b`, does that mean we can necessarily transform a `T a` into a
`T b`?

As it happens, we can sometimes do this, but it has a great deal to do with what
`T` looks like. Depending on the shape of `T` (of kind `Type ->
Type`) there are three possibilities for `T`'s variance:[^re-variance]

[^re-variance]: Precisely speaking, variance is a property of a *type variable*
  in relation to one of its type-constructors. Because we have the convention
  that `map`-like functions transform the last type parameter, we can
  unambiguously say "`T` is covariant" as a short-hand for "`T a` is covariant
  with respect to `a`."

1. *Covariant:* Any function `a -> b` can be lifted into a function
    `T a -> T b`.
2. *Contravariant:* Any function `a -> b` can be lifted into a function
    `T b -> T a`.
3. *Invariant:* In general, functions `a -> b` cannot be lifted into a function
    over `T a`.

Covariance is the sort we're most familiar with---it corresponds directly to
`Functor`s. And in fact, the type of `fmap` is exactly witness to this "lifting"
motion `(a -> b) -> T a -> T b`. A type `T` is a `Functor` if and only if it is
covariant.

Before we get to when is a type covariant, let's first look at contravariance
and invariance.

The `contravariant`@cite:contravariant and `invariant`@cite:invariant packages
give us access to the `Contravariant` and `Invariant` classes. These classes are
to their sorts of variance as `Functor` is to covariance.

A contravariant type allows you to map a function *backwards* across its type
constructor.

[code/PosNeg.hs:Contravariant](Snip)

On the other hand, an invariant type `T` allows you to map from `a` to `b` if
and only if `a` and `b` are isomorphic. In a very real sense, this isn't an
interesting property---an isomorphism between `a` and `b` means they're already
the same thing to begin with!

[code/PosNeg.hs:Invariant](Snip)

The variance of a type `T a` with respect to its type variable `a` is fully
specified by whether `a` appears solely in positive position, solely in negative
position or in a mix of both.

Type variables which appear exclusively in positive position are covariant.
Those exclusively in negative position are contravariant. And type variables
which find themselves in both become invariant.

But what *is* a positive or negative position? Recall that all types have a
canonical representation expressed as some combination of `(,)`, `Either` and
`(->)`. We can therefore define positive and negative positions in terms of
these fundamental building blocks, and develop our intuition moving forwards.

|     Type     | Position of `a` | Position of `b` |
|:------------:|:---------------:|:---------------:|
| `Either a b` |       $+$       |       $+$       |
|   `(a, b)`   |       $+$       |       $+$       |
|   `a -> b`   |       $-$       |       $+$       |

The conclusion is clear---our only means of introducing type variables in
negative position is to put them on the left-side of an arrow. This should
correspond to your intuition that the type of a function goes "backwards" when
pre-composed with another function.

In the following example, pre-composing with `show :: Bool -> String`
transforms a type `String -> [String]` into `Bool -> [String]`.

```{ghci=code/PosNeg.hs}
:t words
:t show :: Bool -> String
:t words . (show :: Bool -> String)
```

Mathematically, things are often called "positive" and "negative" if their
polarities follow the usual laws of multiplication. That is to say, a positive
composed with a positive remains positive, a negative composed with a positive
is a negative, and so on.

Variances are no different. To illustrate, consider the type `(a, Bool) -> Int`.
The `a` in the subtype `(a, Bool)` is in positive position, but `(a, Bool)` is
in negative position relative to `(a, Bool) -> Int`. As we remember from early
arithmetic in school, a positive times a negative is negative, and so `(a, Bool)
-> Int` is contravariant with respect to `a`.

This relationship can be expressed with a simple table---but again, note that
the mnemonic suggested by the name of positive and negative positions should be
enough to commit this table to memory.

| `a` | `b` | `a` $\cdot$ `b` |
|:---:|:---:|:---------------:|
| $+$ | $+$ |       $+$       |
| $+$ | $-$ |       $-$       |
| $-$ | $+$ |       $-$       |
| $-$ | $-$ |       $+$       |

We can use this knowledge to convince ourselves why `Functor` instances exist
only for the `T1` and `T5` types defined above.

```align
  `T1`  &\cong  `Int -> `\posp{`a`} & \possym &= \possym
  `T2`  &\cong  \negp{`a`}` -> Int` & \negsym &= \negsym
  `T3`  &\cong  \negp{`a`}` -> `\posp{`a`} & \pmsym &= \pmsym
  `T4`  &\cong  \negp{`(Int -> `\posp{`a`}`)`}` -> Int` & \negsym\circ\possym &= \negsym
  `T5`  &\cong  \negp{`(`\negp{`a`}` -> Int)`}` -> Int` & \negsym\circ\negsym &= \possym
```

This analysis also shows us that `T2` and `T4` have `Contravariant` instances,
and `T3` has an `Invariant` one.

A type's variance also has a more concrete interpretation: variables in positive
position are *produced* or *owned*, while those in negative position are
*consumed*. Products, sums and the right-side of an arrow are all pieces of data
that already exist or are produced, but the type on the left-side of an arrow is
indeed consumed.

There are some special names for types with multiple type variables. A type that
is covariant in two arguments (like `Either` and `(,)`) is called a bifunctor. A
type that is contravariant in its first argument, but covariant in its second
(like `(->)`) is known as a profunctor. The `Bifunctor` class exists in `base`,
and `Profunctor` comes from the `profunctors` package.

Positional analysis like this is a powerful tool---it's quick to eyeball, and
lets you know at a glance which class instances you need to provide. Better yet,
it's impressive as hell to anyone who doesn't know the trick.

> TODO(sandy): saves a lot of time trying to implement things that are
> impossible

