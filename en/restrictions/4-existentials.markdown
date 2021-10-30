









































































































## Existential Types

### Existential Types and Eliminators

Closely related to rank-*n* types are the *existential types*. These
are types with a sort of identity problem---the type system has forgotten what
they are! Although it sounds strange at first, existentials are in fact very
useful.

For the time being, we will look at a simpler example: the `Any` type.

[code/RankN.hs:Any](Snip)

`Any` is capable of storing a value of any type, and in doing so, forgets
what type it has. The type constructor doesn't have any type variables, and so
it *can't* remember anything. There's nowhere for it to store that
information.

In order to introduce a type variable for `Any` to be polymorphic over, we
can use the same `forall a.`{.haskell} as when working with rank-*n* types. This
`a` type exists only within the context of the `Any`{.haskell} data constructor; it
is existential.

The syntax for defining existential types in data constructors is heavy-handed.
GADTs provide a more idiomatic syntax for this construction.

[code/RankN.hs:GADTAny](Snip)

We can use `Any` to define a list with values of any types. At first blush
this sounds like the `HList` we constructed in chapter 5. However,
the usefulness of an `Any` list is limited due to the fact that its values
can never be recovered.



```{ghci=code/RankN.hs}
:t [ Any 5, Any True, Any "hello" ]
```

Existential types can be *eliminated* (consumed) via continuation-passing.
An *eliminator* is a rank-2 function which takes an existential type and a
continuation that can produce a value regardless of what it gets. Elimination
occurs when our existential type is fed into this rank-2 function.

To clarify, the eliminator for `Any` is `elimAny`{.haskell}:

[code/RankN.hs:elimAny](Snip)

Pay attention to where the `a` and `r` types are quantified. The caller of
`elimAny`{.haskell} gets to decide the result `r`, but `a` is determined by the
type inside of the `Any`.

```exercise
Are functions of type `forall a. a -> r` interesting? Why or why
not?
```

```solution
  These functions can only ever return constant values, as the polymorphism on
  their input doesn't allow any form of inspection.
```


This approach of existentializing types and later eliminating them is more
useful than it seems. As a next step, consider what happens when we
pack a typeclass dictionary along with our existentialized data.

[code/RankN.hs:HasShow](Snip)

The definition of `HasShow` is remarkably similar to the GADT definition of
`Any`, with the addition of the `Show t =>`{.haskell} constraint. This constraint
requires a `Show` instance whenever constructing a `HasShow`, and Haskell
will remember this. Because a `Show` instance was required to build a
`HasShow`, whatever type is inside of `HasShow` must have a `Show`
instance. Remarkably, Haskell is smart enough to realize this, and allow us to
call `show`{.haskell} on whatever type exists inside.

We can use this fact to write a `Show` instance for `HasShow`.

[code/RankN.hs:hasShowShow](Snip)

```exercise
What happens to this instance if you remove the `Show t =>`{.haskell}
constraint from `HasShow`?
```

```solution
The `Show HasShow` instance can't be written without its `Show t =>`{.haskell}
  constraint---as the type inside `HasShow` is existential and Haskell
  doesn't know which instance of `Show` to use for the `show`{.haskell} call.
```


More generally, we are able to write an eliminator for `HasShow` which knows
we have a `Show` dictionary in scope.

[code/RankN.hs:elimHasShow](Snip)

```exercise
Write the `Show` instance for `HasShow` in terms of
`elimHasShow`{.haskell}.
```

```solution
[code/RankN.hs:showElimHasShow](Snip)
```



#### Dynamic Types

This pattern of packing a dictionary alongside an existential type becomes more
interesting with other typeclasses. The `Typeable` class provides type
information at runtime, and allows for dynamic casting via `cast :: (Typeable
a, Typeable b) => a -> Maybe b`{.haskell}. We can existentialize `Typeable` types in
order to turn Haskell into a dynamically typed language.

Using this approach, we can write Python-style functions that play fast and
loose with their types. As an illustration, the `+`{.json} operator in Python
plays double duty by concatenating strings and adding numbers. And we can
implement the same function in Haskell with `Dynamic`.

Given the datatype and its eliminator:

[code/RankN.hs:Dynamic](Snip)
[code/RankN.hs:elimDynamic](Snip)

We can implement `fromDynamic` which attempts to cast a `Dynamic` to an
`a`.

[code/RankN.hs:fromDynamic](Snip)

A helper function will assist in the implementation.

[code/RankN.hs:liftD2](Snip)

This function attempts to lift a regular, strongly-typed function into a
function over dynamic types. It returns a `Maybe Dynamic`, which is returned
if the cast failed.

Finally, we can present a Haskell version of Python's `+`{.json} operator:

[code/RankN.hs:pyPlus](Snip)

In order to easily play with it in GHCi we will need to enable
`-XTypeApplications` (to get the right type out), and set the default numeric
type to `Int` (to construct `Dynamic`s without type signatures.)

```{ghci=code/RankN.hs}
default (Int)
fromDynamic @Int (pyPlus (Dynamic 1) (Dynamic 2))
fromDynamic @String (pyPlus (Dynamic "hello") (Dynamic " world"))
fromDynamic @String (pyPlus (Dynamic 4) (Dynamic " minute"))
```

If you were particularly plucky, with this approach you could embed a
fully-functioning a dynamically typed language inside of Haskell. The
boilerplate around writing type dependent pattern matches would amortize down to
$O(1)$ as more of the standard library were implemented.

But, just so we're on the same page: just because you *can*, doesn't mean
you *should.* However, there is an interesting philosophical takeaway
here---dynamically typed languages are merely strongly typed languages with a
single type.


#### Generalized Constraint Kinded Existentials

The definitions of `HasShow` and `Dynamic` are nearly identical. Recall:

[code/RankN.hs:HasShow](Snip)
[code/RankN.hs:Dynamic](Snip)

There is a clear pattern here, that can be factored out by being polymorphic
over the `Constraint` packed inside. By enabling `-XConstraintKinds`, we
are able to be polymorphic over `Constraint`s:

[code/RankN.hs:Has](Snip)
[code/RankN.hs:elimHas](Snip)

We can thus implement `HasShow` and `Dynamic` as type synonyms.

[code/RankN.hs:typeHasShow](Snip)

Sometimes we want to be able to talk about multiple constraints at once. Like
the function which determines if its argument is `mempty`{.haskell}.

[code/RankN.hs:isMempty](Snip)

Maybe we'd like to construct an `Has` around this constraint, `(Monoid a,
Eq a)`. Unfortunately, there is no type-level lambda syntax, so we're unable to
turn this type into something that's curryable. We can try a type synonym:

[code/RankN.hs:MonoidAndEq](Snip)

But GHC won't allow us to construct a `Has MonoidAndEq`.

```{ghci=code/RankN.hs}
:t Has [True] :: Has MonoidAndEq
```

The problem is that type synonyms must always be fully saturated. We're unable
to talk about `MonoidAndEq` in its unsaturated form---only `MonoidAndEq a`
is acceptable to the compiler.

Fortunately, there is a solution for `Constraint`-synonyms (though not for
type synonyms in general.) We can instead define a new class with a superclass
constraint, and an instance that comes for free given those same constraints.

[code/RankN.hs:MonoidEq](Snip)

This is known as a constraint synonym. While type synonyms are unable to
be partially applied, classes have no such restriction.

```{ghci=code/RankN.hs}
let foo = Has [True] :: Has MonoidEq
elimHas isMempty foo
```


### Scoping Information with Existentials



Existential types can be used to prevent information from leaking outside of a
desired scope. For example, it means we can ensure that allocated resources
can't escape a pre-specified region. We can use the type system to prove that a
HTTP session-token is quarantined within its request context, or that a file
handle doesn't persist after it's been closed.

Because existential types are unable to exist outside of their quantifier, we
can use it as a scoping mechanism. By tagging sensitive data with an existential
type, the type system will refuse any attempts to move this data outside of its
scope.

Haskell's `ST` monad is the most famous example of this approach, lending its
name to the approach: the ST trick. If you're unfamiliar with it, `ST`
allows us to write stateful code---including mutable variables---to perform
computations, so long as the statefulness never leaves the monad. In other
words, `ST` allows you to compute pure functions using impure means.

The amazing thing is that `ST` is not some magical compiler primitive---it's
just library code. And we can implement it ourselves, assuming we're comfortable
using a little `unsafePerformIO`{.haskell}! Of course, this is not a comfortable
situation---`unsafePerformIO`{.haskell} is *fundamentally unsafe*, but observe
that there is nothing inherently unsafe about mutable variables.

It's not the presence of mutable variables that makes code hard to reason about.
So long as all of its mutations are kept local, we know that a computation is
pure. Mutable variables on their own do not cause us to lose referential
transparency.

Referential transparency is lost when code relies on *external* mutable
variables. Doing so creates an invisible data dependency between our code and
the state of its external variables. It is these cases---and these cases
alone---that we need worry about.

As such, it's completely safe to have mutable variables so long as you can prove
they never escape. The ST trick exists to prevent such things from happening.
Enough jibber-jabber. Let's implement it.

At its heart, `ST` is just the `Identity` monad with a phantom `s`
parameter.

[code/ST.hs:ST](Snip)

Notice that at [1](Ann) we have a phantom type parameter `s`. This
variable exists only as a place to put our existential type tag. We'll better
see how it's used in a minute.

`Applicative` and `Monad` instances can be provided for `ST`. To ensure
that our "unsafe" IO is performed while it's actually still safe, these
instances must be explicitly strict. *This is not necessary in general to
perform the ST trick*---it's only because we will be using `unsafePerformIO`{.haskell}
for the example.

[code/ST.hs:functor](Snip)
[code/ST.hs:applicative](Snip)
[code/ST.hs:monad](Snip)

Mutable variables can be introduced inside of the `ST` monad. For our
implementation, we can simply implement these in terms of `IORef`s. We will
wrap them in a newtype.

[code/ST.hs:STRef](Snip)

Pay attention to the fact that `STRef` also has a phantom `s` parameter
([1](Ann)). This is not accidental. `s` acts as a label irrevocably knotting a
`STRef` with the `ST` context that created it. We'll discuss this after a
little more boilerplate that it's necessary to get through.

Function wrappers for `STRef` around `IORef` are provided, each of which
unsafely performs `IO`. For example, we'd like to be able to create new
`STRef`s.

[code/ST.hs:newSTRef](Snip)

See here at [1](Ann), that creating a `STRef` gives us one whose `s`
parameter is the same as `ST`'s `s`. This is the irrevocable linking
between the two types I mentioned earlier.

There are a few more useful functions to wrap:

[code/ST.hs:readSTRef](Snip)
[code/ST.hs:writeSTRef](Snip)
[code/ST.hs:modifySTRef](Snip)

And finally, we provide a function to escape from the `ST` monad. This is
merely `unsafeRunST`, but with a specialized type signature.

[code/ST.hs:runST](Snip)

At [1](Ann) we see the introduction of the ST trick. The type `(forall s.
ST s a)`{.haskell} indicates that `runST`{.haskell} is capable of running only those `ST`s
which do not depend on their `s` parameter.

We will discuss why exactly this works shortly, but let's first convince
ourselves that `runST`{.haskell} lives up to its promises. We can write a safe usage of
`ST`---one which uses its state to compute a pure value.

[code/ST.hs:safeExample](Snip)

```{ghci=code/ST.hs}
runST safeExample
```

But the type system now prevents us from `runST`{.haskell}-ing any code that would leak
a reference to a `STRef`.

```{ghci=code/ST.hs}
runST (newSTRef True)
```

Indeed, `runST`{.haskell} seems to work as expected---but how? Let's look again at the
type of `runST`.

[code/ST.hs:runSTType](Snip)

The word `forall`{.haskell} here acts as a quantifier over `s`---the type variable
exists in scope *only* within `ST s a`. Because it's existential,
without a quantifier, we have no way of talking about the type. It simply
doesn't exist outside of its `forall`{.haskell}!

And this is the secret to why the ST trick works. We exploit this fact that
existentials can't leave their quantifier in order to scope our data. The
"quarantined zone" is defined with an existential quantifier, we tag our
quarantined data with the resulting existential type, and the type system does
the rest.

To really drive this home, let's look at a specific example. Take again the case
of `runST (newSTRef True)`{.haskell}. If we specialize the type of `runST`{.haskell} here, it
results in the following:

[code/ST.hs:signature](Snip)

Written like this, it's more clear what's going wrong. The type variable `s`
is introduced---and scoped---at [1](Ann). But later `s` is referenced at
[2](Ann). At this point the type no longer exists---there isn't any type `s`
in scope!

GHC calls `s` a rigid skolem type variable. Rigid
variables are those that are constrained by a type signature written by a
programmer---in other words, they are not allowed to be type inferred. A human
has already given them their type.

A skolem is, for all intents and purposes, any existential
type.

The purpose of the phantom `s` variable in `ST` and `STRef` is exactly
to introduce a rigid skolem. If it weren't rigid (specified), it would be free
to vary, and Haskell would correctly infer that it is unused. If it weren't
a skolem, we would be unable to restrict its existence.

This ST trick can be used whenever you want to restrict the existence of some
piece of data. I've seen it used to tag variables owned by external FFI, and
used it to implement monadic regions which have more or fewer effect
capabilities.



