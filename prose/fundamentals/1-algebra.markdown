## The Algebra Behind Types

### Isomorphisms and Cardinalities

One of functional programming's killer features is pattern matching, as made
possible by algebraic data types. But this term
isn't just a catchy title for things that we can pattern match on. As their name
suggests, there is in fact an *algebra* behind algebraic data types.

Being comfortable understanding and manipulating this algebra is a mighty
superpower---it allows us to analyze types, find more convenient forms for them,
and determine which operations (eg. typeclasses) are possible to implement.

To start, we can associate each finite type with its cardinality---the
number of inhabitants it has, ignoring bottoms. Consider the following simple
type definitions:

[code/Algebra.hs:Void](Snip)

[code/Algebra.hs:Unit](Snip)

[code/Algebra.hs:Bool](Snip)

`Void` has zero inhabitants, and so it is assigned cardinality 0. The unit
type `()` has one inhabitant---thus its cardinality is 1. Not to belabor
the point, but `Bool` has cardinality 2, corresponding to its constructors
`True` and `False`.

We can write these statements about cardinality more formally:

```align
`cardinality:Void` &= 0
`cardinality:()` &= 1
`cardinality:Bool` &= 2
```

Any two finite types that have the same cardinality will always be isomorphic to
one another. An isomorphism between types `s` and `t` is defined as
a pair of functions `to` and `from`:

[code/Algebra.hs:to](Snip)

such that composing either after the other gets you back where you started. In
other words, such that:

```align
`to .\spaceJob{}from = id`

`from .\spaceJob{}to = id`
```

We sometimes write an isomorphism between types `s` and `t` as `s` &cong; `t`.

If two types have the same cardinality, any one-to-one mapping between their
elements is exactly these `to` and `from` functions. But where does such a
mapping come from? Anywhere---it doesn't really matter! Just pick an arbitrary
ordering on each type---not necessarily corresponding to an `Ord`
instance---and then map the first element under one ordering to the first
element under the other. Rinse and repeat.

For example, we can define a new type that also has cardinality 2.

[code/Algebra.hs:Spin](Snip)

By the argument above, we should expect `Spin` to be isomorphic to `Bool`.
Indeed it is:

[code/Algebra.hs:boolToSpin1](Snip)

[code/Algebra.hs:spinToBool1](Snip)

However, note that there is another isomorphism between `Spin` and
`Bool`:

[code/Algebra.hs:boolToSpin2](Snip)

[code/Algebra.hs:spinToBool2](Snip)

Which of the two isomorphisms should we prefer? Does it matter?

In general, for any two types with cardinality $n$, there
are $n!$ unique isomorphisms between them. As far as the math goes, any of these
is just as good as any other---and for most purposes, knowing that an
isomorphism *exists* is enough.

An isomorphism between types `s` and `t` is a proof that *for all
intents and purposes,* `s` and `t` *are the same thing.* They might
have different instances available, but this is more a statement about Haskell's
typeclass machinery than it is about the equivalence of `s` and `t`.

Isomorphisms are a particularly powerful concept in the algebra of types.
Throughout this book we shall reason via isomorphism, so it's best to get
comfortable with the idea now.


### Sum, Product and Exponential Types

In the language of cardinalities, sum types correspond to
addition. The canonical example of these is `Either a b`, which is
*either* an `a` or a `b`. As a result, the cardinality (remember, the
number of inhabitants) of `Either a b` is the cardinality of `a` plus the
cardinality of `b`.

$$
`cardinality:Either a b` = `cardinality:a` + `cardinality:b`
$$

As you might expect, this is why such things are called *sum* types. The
intuition behind adding generalizes to any datatype with multiple
constructors---the cardinality of a type is always the sum of the cardinalities
of its constructors.

[code/Algebra.hs:Deal](Snip)

We can analyze `Deal`'s cardinality;

```align
  `cardinality:Deal a b` &= `cardinality:a` + `cardinality:b` + `cardinality:Bool`

  &= `cardinality:a` + `cardinality:b` + 2
```

We can also look at the cardinality of `Maybe a`. Because nullary data
constructors are uninteresting to construct---there is only one
`Nothing`---the cardinality of `Maybe a` can be expressed as follows;

$$
`cardinality:Maybe a` = 1 + `cardinality:a`
$$

Dual to sum types are the so-called product types. Again,
we will look at the canonical example first---the pair type `(a, b)`.
Analogously, the cardinality of a product type is the *product* of their
cardinalities.

$$
`cardinality:(a, b)` = `cardinality:a` \times `cardinality:b`
$$

To give an illustration, consider mixed fractions of the form $5\frac{1}{2}$. We
can represent these in Haskell via a product type;

[code/Algebra.hs:MixedFraction](Snip)

And perform its cardinality analysis as follows:

$$
`cardinality:MixedFraction a` = `cardinality:Word8` \times `cardinality:a` \times `cardinality:a` = 256
\times `cardinality:a` \times `cardinality:a`
$$

An interesting consequence of all of this cardinality stuff is that we find
ourselves able to express *mathematical truths in terms of types*. For
example, we can prove that $a \times 1 = a$ by showing an isomorphism between
`(a, ())` and `a`.

[code/Algebra.hs:prodUnitTo](Snip)

[code/Algebra.hs:prodUnitFrom](Snip)

Here, we can think of the unit type as being a monoidal identity for product
types---in the sense that "sticking it in doesn't change anything." Because $a
\times 1 = a$, we can pair with as many unit types as we want.

Likewise, `Void` acts as a monoidal unit for sum types. To convince ourselves
of this, the trivial statement $a+0 = a$ can be witnessed as an isomorphism
between `Either a Void` and `a`.

[code/Algebra.hs:sumUnitTo](Snip)

[code/Algebra.hs:sumUnitFrom](Snip)

The function `absurd` at [1](Ann) has the type `Void -> a`. It's a sort of
bluff saying "if you give me a `Void` I can give you anything you want."
This is a promise that can never be fulfilled, but because there are no
`Void`s to be had in the first place, we can't disprove such a claim.

Function types also have an encoding as statements about cardinality---they
correspond to exponentialization. To give an example, there are exactly four
($2^2$) inhabitants of the type `Bool -> Bool`. These functions are `id`,
`not`, `const True` and `const False`. Try as hard as you can, but you
won't find any other pure functions between `Bool`s!

More generally, the type `a -> b` has cardinality $`cardinality:b`^{`cardinality:a`}$.
While this might be surprising at first---it always seems backwards to me---the
argument is straightforward. For every value of `a` in the domain, we need to
give back a `b`. But we can choose any value of `b` for every value of
`a`---resulting in the following equality.

$$
`cardinality:a -> b` = \underbrace{`cardinality:b` \times `cardinality:b` \times \cdots \times
`cardinality:b`}_{`cardinality:a` \text{times}} = `cardinality:b`^{`cardinality:a`}
$$


Exercise

:   Determine the cardinality of `Either Bool (Bool, Maybe Bool) -> Bool`.


Solution

:   ```align
      &  `cardinality:Either Bool (Bool, Maybe Bool) -> Bool`
      &= `cardinality:Bool`^{`cardinality:Either Bool (Bool, Maybe Bool)`}
      &= `cardinality:Bool`^{`cardinality:Bool`+`cardinality:Bool`\times`cardinality:Maybe Bool`}
      &= `cardinality:Bool`^{`cardinality:Bool`+`cardinality:Bool`\times(`cardinality:Bool`+1)}
      &= 2^{2+2\times(2+1)}
      &= 2^{2+2\times 3}
      &= 2^{2+6}
      &= 2^{8}
      &= 256
    ```

The inquisitive reader might wonder whether subtraction, division and other
mathematical operations have meaning when applied to types. Indeed they do, but
such things are hard, if not impossible, to express in Haskell. Subtraction
corresponds to types with particular values removed, while division of a type
makes some of its values equal (in the sense of being defined equally---rather
than having an `Eq` instance which equates them.)

In fact, even the notion of differentiation in calculus has meaning in the
domain of types. Though we will not discuss it further, the interested reader is
encouraged to refer to Conor McBride's paper "The Derivative of a Regular Type
is its Type of One-Hole Contexts."@cite:one-hole.


### Example: Tic-Tac-Toe {.rev2}

I said earlier that being able to manipulate the algebra behind types is a
mighty superpower. Let's prove it.

Imagine we wanted to write a game of tic-tac-toe. The standard tic-tac-toe board
has nine spaces, which we could naively implement like this:

[code/TicTacToe.hs:Board1](Snip)

While such a thing works, it's rather unwieldy to program against. If we wanted
to construct an empty board for example, there's quite a lot to fill in.

[code/TicTacToe.hs:empty1](Snip)

Given a type `Three`, which we will reuse for rows and columns, we can index
into a board thusly:

[code/TicTacToe.hs:Three](Snip)

[code/TicTacToe.hs:getAt1](Snip)

And of course, we will need the ability to write to a board cell:

[code/TicTacToe.hs:setAt1](Snip)

This is all quite a lot of work, and writing functions like
`checkWinner` turn out to be even more involved.

Rather than going through all of this trouble, we can use our knowledge of the
algebra of types to help. The first step is to perform a cardinality analysis on
`Board1`;

```align
`cardinality:Board1 a` &= \underbrace{`cardinality:a` \times `cardinality:a` \times \cdots
\times `cardinality:a`}_{9 \text{ times}}
  &= `cardinality:a`^{9}
  &= `cardinality:a`^{3\times 3}
```

When written like this, we see that `Board1 a` is isomorphic to a function
`(Three, Three) -> a`, or in its curried form: `Three -> Three -> a`. Due to
this isomorphism, we can instead represent `Board1` in this form:

[code/TicTacToe.hs:Board3](Snip)

And thus simplify our implementations of `empty1`:

[code/TicTacToe.hs:empty3](Snip)

It's certainly much less effort. Getting the contents of a `Board3` is just a
matter of applying the function:

[code/TicTacToe.hs:getAt3](Snip)

However, setting a `Board3` is a little trickier. The trick is to create a new
function which checks if the requested position is the one we're trying to set.
If so, we give back the new value, otherwise delegating to looking up in the
older board's function.

[code/TicTacToe.hs:setAt3](Snip)

In spirit, `Board3` represents state by chaining a bunch of if-else expressions
together.

You might notice that the asymptotics are different here. The `empty` function
went from $O(n)$ to $O(1)$ in our new representation, but `getAt3` goes from
$O(1)$ to $O(n)$. Trade-offs like these are common when looking at different
representations of types. If you are in a performance-critical
domain[^not-very-often], it makes sense to optimize your representation for best
performance in the common case. But this example is merely a tic-tac-toe board,
so we will not burden ourselves with further thoughts about performance.

[^not-very-often]: Which is significantly less common than most programmers
  believe.

The new representation of our type doesn't let us do anything we couldn't have
done otherwise. In fact, it probably feels silly and needlessly slow to you. But
you must admit that dramatically improves the ergonomics of `getAt1` and
`setAt1` --- as well as the other functions you'd need to implement for a
working tic-tac-toe game. By making this change, we are rewarded with the entire
toolbox of combinators for working with functions; we gain better
compositionality and have to pay less of a cognitive burden.

Let us not forget that programming is primarily a human endeavor, and ergonomics
are indeed a worthwhile pursuit. Your colleagues and collaborators will thank
you later!


### The Curry--Howard Isomorphism

Our previous discussion of the algebraic relationships between types and their
cardinalities can be summarized in the following table.

|  **Algebra** |    **Logic**   |   **Types**   |
|:------------:|:--------------:|:-------------:|
|    $a + b$   |   $a \vee b$   |  `Either a b` |
| $a \times b$ |  $a \wedge b$  |    `(a, b)`   |
|     $b^a$    | $a \implies b$ |    `a -> b`   |
|    $a = b$   |   $a \iff b$   | *isomorphism* |
|       0      |     $\bot$     |     `Void`    |
|       1      |     $\top$     |      `()`     |

This table itself forms a more-general isomorphism between mathematics and
types. It's known as the Curry--Howard isomorphism---loosely stating
that every statement in logic is equivalent to some computer program, and vice
versa.

The Curry--Howard isomorphism is a profound insight about our universe. It
allows us to analyze mathematical theorems through the lens of functional
programming. What's better is that often even "boring" mathematical theorems
are interesting when expressed as types.

To illustrate, consider the theorem $a^1 = a$. When viewed through
Curry--Howard, it describes an isomorphism between `() -> a` and `a`.
Said another way, this theorem shows that there is no essential distinction
between having a value and having a (pure) program that computes that value.
This insight is the core principle behind why writing Haskell is such a joy
compared with other programming languages.

Exercise

:   Use Curry--Howard to prove that $(a^b)^c = a^{b\times c}$. That is, provide a
    function of type `(b -> c -> a) -> (b, c) -> a`, and one of `((b, c) ->
    a) -> b -> c -> a`. Make sure they satisfy the equalities `to . from = id`
    and `from . to = id`. Do these functions remind you of anything from
    `Prelude`?

Solution

:   [code/Algebra.hs:curry](Snip)

    [code/Algebra.hs:uncurry](Snip)

    Both of these functions already exist in `Prelude`.


Exercise

:   Give a proof of the exponent law that $a^b \times a^c = a^{b+c}$.

Solution

:   [code/Algebra.hs:productRule1To](Snip)

    [code/Algebra.hs:productRule1From](Snip)

    Notice that `productRule1To` is the familiar `either` function from
    `Prelude`.


Exercise

:   Prove $(a\times b)^c = a^c \times b^c$.

Solution

:   [code/Algebra.hs:productRule2To](Snip)

    [code/Algebra.hs:productRule2From](Snip)


### Canonical Representations

A direct corollary that any two types with the same cardinality are isomorphic,
is that there are multiple ways to represent any given type. Although you
shouldn't necessarily let it change the way you model types, it's good to keep
in mind that you have a choice.

Due to the isomorphism, all of these representations of a type are "just as
good" as any other. However, as we'll see @Sec:ghc.generics, it's
often useful to have a conventional form when working with types generically.
This canonical representation is known as a sum of products, and
refers to any type `t` of the form,

$$
{t = \sum_{m}^{}{\prod_{n}^{}{t_{m,n}}}}
$$

The big $\Sigma$ means addition, and
the $\Pi$ means multiplication---so we can read this as "addition on the
outside and multiplication on the inside." We also make the stipulation that
all additions must be represented via `Either`, and that multiplications via
`(,)`. Don't worry, writing out the rules like this makes it seem much more
complicated than it really is.

All of this is to say that each of following types is in its canonical
representation:

* `()`
* `Either a b`
* `Either (a, b) (c, d)`
* `Either a (Either b (c, d))`
* `a -> b`
* `(a, b)`
* `(a, Int)`---we make an exception to the rule for numeric types, as it would
    be too much work to express them as sums.

But neither of the following types are in their canonical representation;

* `(a, Bool)`
* `(a, Either b c)`

As an example, the canonical representation of `Maybe a` is `Either a ()`.
To reiterate, this doesn't mean you should prefer using `Either a ()` over
`Maybe a`. For now it's enough to know that the two types are equivalent. We
shall return to canonical forms in chapter 13.

