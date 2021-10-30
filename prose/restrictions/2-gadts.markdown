
## Constraints and GADTs


### Introduction

`Constraint`s are odd. They don't behave like `Type`s nor like
promoted data kinds. They are a fundamentally different thing altogether, and
thus worth studying.

The `Constraint` kind is reserved for things that can appear on the left
side of the fat context arrow (`=>`{.haskell}). This includes fully-saturated
typeclasses (like `Show a`), tuples of other `Constraint`s, and type
equalities (`Int ~ a`.) We will discuss type equalities in a moment.

Typeclass constraints are certainly the most familiar. We use them all the time,
even when we are not writing type-level Haskell. Consider the equality function
`(==) :: Eq a => a -> a -> Bool`{.haskell}. Tuples of `Constraint`s are similarly
well-known: `sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t
a)`{.haskell}.

Type equalities are more interesting, and are enabled via `-XGADTs`. Compare
the following two programs:

[code/Constraints.hs:five](Snip)
[code/Constraints.hs:five_](Snip)

Both `five`{.haskell} and `five\_`{.haskell} are identical as far as Haskell is concerned.
While `five`{.haskell} has type `Int`, `five\_`{.haskell} has type `a`, along with a
constraint saying that `a` equals `Int`. Of course, nobody would actually
write `five\_`, but it's a neat feature of the type system regardless.

Type equalities form an equivalence relation, meaning that they have the
following properties:

<ul>
  * reflexivity---a type is always equal to itself: `a ~ a`
  * symmetry---`a ~ b` holds if and only if `b
    ~ a`
  * transitivity---if we know both `a ~ b` and `b
    ~ c`, we (and GHC) can infer that `a ~ c`.
</ul>


### GADTs


Generalized algebraic datatypes (GADTs; pronounced "gad-its") are an extension
to Haskell's type system that allow explicit type signatures to be written for
data constructors. They, like type equality constraints, are also enabled via
`-XGADTs`.

The canonical example of a GADT is a type safe syntax tree. For example, we can
declare a small language with integers, booleans, addition, logical negation,
and if statements.

[code/GADTs.hs:Expr](Snip)

The `where`{.haskell} at [1](Ann) is what turns on GADT syntax for the rest of the
declaration. Each of `LitInt`{.haskell}, `LitBool`{.haskell}, `Add`{.haskell}, etc. corresponds to a
data constructor of `Expr`. These constructors all take some number of
arguments before resulting in an `Expr`.

For example, `LitInt`{.haskell} at [2](Ann) takes an `Int` before returning a
`Expr Int`. On the other hand, the data constructor `If`{.haskell} at [3](Ann) takes
three arguments (one `Expr Bool` and two `Expr a`s) and returns an
`Expr a`.

It is this ability to specify the return type that is of particular interest.

You might be pleased that `Expr` is *correct by
construction.* We are incapable of building a
poorly-typed `Expr`. While this might not sound immediately remarkable, it
is---we've reflected the *typing rules of `Expr`* in the type system of
Haskell. For example, we're unable to build an AST which attempts to add an
`Expr Int` to a `Expr Bool`.

To convince ourselves that the type signatures written in GADT syntax are indeed
respected by the compiler, we can look in GHCi:

```{ghci=code/GADTs.hs}
:t LitInt
:t If
```

Because GADTs allow us to specify a data constructor's type, we can use them to
*constrain* a type variable in certain circumstances. Such a thing is not
possible otherwise.

The value of GADTs is that Haskell can use the knowledge of these constrained
types. In fact, we can use this to write a typesafe evaluator over `Expr`:

[code/GADTs.hs:evalExpr](Snip)

In just this amount of code, we have a fully functioning little language and
interpreter. Consider:

```{ghci=code/GADTs.hs}
evalExpr . If (LitBool False) (LitInt 1) . Add (LitInt 5) $ LitInt 13
evalExpr . Not $ LitBool True
```

Pay careful attention here! At [1](Ann), `evalExpr`{.haskell} returns an `Int`, but
at [2](Ann) it returns a `Bool`! This is possible because Haskell can
*reason* about GADTs. In the `LitInt`{.haskell} case, the only way such a pattern
could have matched is if `a ~ Int`, in which case it's certainly okay to
return a `Int`. The reasoning for the other patterns is similar; Haskell can
use information from inside a pattern match to drive type inference.

GADT syntax is indeed provided by `-XGADTs`, but it is not the syntax that is
fundamentally interesting. The extension is poorly named---a more appropriate
name might be "`-XTypeEqualities`". In fact, GADTs are merely syntactic
sugar over type equalities. We can also declare `Expr` as a traditional
Haskell datatype as follows:

[code/GADTs.hs:Expr_](Snip)

When viewed like this, it's a little easier to see what's happening behind the
scenes. Each data constructor of `Expr\_` carries along with it a
type equality constraint. Like any constraint inside a data constructor, Haskell
will require the constraint to be proven when the data constructor is called.

As such, when we pattern match on a data constructor which contains a
constraint, this satisfied constraint *comes back into scope.* That is, a
function of type `Expr a -> a` can return an `Int` when pattern matching
on `LitInt`{.haskell}, but return a `Bool` when matching on `LitBool`{.haskell}. The type
equality constraining `a` only comes back into scope after pattern matching
on the data constructor that contains it.

We will explore the technique of packing constraints inside data constructors
in much greater generality later.

Though GADT syntax doesn't offer anything novel, we will often use it when
defining complicated types. This is purely a matter of style as I find it more
readable.


### Heterogeneous Lists



One of the primary motivations of GADTs is building inductive type-level
structures out of term-level data. As a working example for this section, we can use GADTs
to define a heterogeneous list---a list which can store values of different
types inside it.

To get a feel for what we'll build:

```{ghci=code/GADTs.hs}
:t HNil
:t True :# HNil
let hlist = Just "hello" :# True :# HNil
:t hlist
hLength hlist
```

The `HNil`{.haskell} constructor here is analogous to the regular list constructor
`[]`{.haskell}. `(:\#)`{.haskell} likewise corresponds to `(:)`{.haskell}. They're defined as a GADT:

[code/GADTs.hs:HList](Snip)

At [1](Ann), you'll notice that we've given `HList`'s `ts` an explicit kind
signature.  The type parameter `ts` is defined to have kind `[Type]`,
because we'll store the contained types inside of it. Although this kind
signature isn't strictly necessary---GHC will correctly infer it for us---your
future self will appreciate you having written it. A good rule of thumb is to
annotate *every* kind if *any* of them isn't `Type`.

`HList` is analogous to the familiar `[]` type, and so it needs to define
an empty list at [2](Ann) called `HNil`{.haskell}, and a cons operator at [3](Ann)
called `(:\#)`{.haskell}. These constructors have carefully chosen types.

`HNil`{.haskell} represents an empty `HList`. We can see this by the fact that it
takes nothing and gives back `ts ~ '[]`---an empty list of types.

The other data constructor, `(:\#)`{.haskell}, takes two parameters. Its first is of
type `t`, and the second is a `HList ts`. In response, it returns a
`HList (t ': ts)`---the result is this new type has been consed onto the
other `HList`.

This `HList` can be pattern matched over, just like we would with regular
lists. For example, we can implement a length function:

[code/GADTs.hs:hLength](Snip)

But, having this explicit list of types to work with, allows us to implement
much more interesting things. To illustrate, we can write a *total*
`head`{.haskell} function---something impossible to do with traditional lists.

[code/GADTs.hs:hHead](Snip)

The oddities don't stop there. We can deconstruct any length-3 `HList` whose
second element is a `Bool`, show it, and have the compiler guarantee that
this is an acceptable (if strange) thing to do.

[code/GADTs.hs:showBool](Snip)

Unfortunately, GHC's stock deriving machinery doesn't play nicely with
GADTs---it will refuse to write `Eq`, `Show` or other instances. But we
can write our own by providing a base case (for `HNil`{.haskell}), and an inductive
case.

The base case is that two empty `HList`s are always equal.

[code/GADTs.hs:eqHNil](Snip)

And inductively, two consed `HList`s are equal only if both their heads and
tails are equal.

[code/GADTs.hs:eqHCons](Snip)

```exercise
Implement `Ord` for `HList`.
```

```solution
[code/GADTs.hs:ordHNil](Snip)
[code/GADTs.hs:ordHCons](Snip)
```


```exercise
Implement `Show` for `HList`.
```

```solution
[code/GADTs.hs:showHNil](Snip)
[code/GADTs.hs:showHCons](Snip)
```


The reason we had to write two instances for `Eq` was to assert that every
element in the list also had an `Eq` instance. While this works, it is rather
unsatisfying. Alternatively, we can write a closed type family which will fold
`ts` into a big `Constraint` stating each element has an `Eq`.

[code/GADTs.hs:AllEq](Snip)

As `AllEq` is our first example of a non-trivial closed type family, we
should spend some time analyzing it. `AllEq` performs type-level pattern
matching on a list of types, determining whether or not it is empty.

If it is empty---line [1](Ann)---we simply return the unit `Constraint`.
Note that because of the kind signature on `AllEq`, Haskell interprets this
as `Constraint` rather than the unit `Type`.

However, if `ts`{.haskell} is a promoted list cons, we instead construct a
`Constraint`-tuple at [2](Ann). You'll notice that `AllEq` is defined
inductively, so it will eventually find an empty list and terminate. By using
the `:kind!` command in GHCi, we can see what this type family expands
to.

```{ghci=code/GADTs.hs}
:kind! AllEq '[Int, Bool]
```

`AllEq` successfully folds `[Type]`s into a `Constraint`. But there
is nothing specific to `Eq` about `AllEq`! Instead, it can be generalized
into a fold over any `Constraint` `c`. We will need
`-XConstraintKinds` in order to talk about polymorphic constraints.

[code/GADTs.hs:All](Snip)

With `All`, we can now write our `Eq` instance more directly.

[code/GADTs.hs:eqHList](Snip)

```exercise
Rewrite the `Ord` and `Show` instances in terms of `All`.
```

```solution
[code/GADTs.hs:ordHList](Snip)
[code/GADTs.hs:showHList](Snip)
```



