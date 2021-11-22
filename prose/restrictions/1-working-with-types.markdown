## Working with Types

### Type Scoping

Haskell uses (a generalization of) the Hindley--Milner type system. One of
Hindley--Milner's greatest contributions is its ability to infer the types of
programs---without needing any explicit annotations. The result is that
term-level Haskell programmers rarely need to pay much attention to types. It's
often enough to just annotate the top-level declarations. And even then, this is
done more for our benefit than for the compiler's.

This state of affairs is ubiquitous and the message it sends is loud and clear:
"types are something we need not think much about". Unfortunately, such an
attitude on the language's part is not particularly helpful for the type-level
programmer. It often goes wrong---consider the following function, which doesn't
compile *because* of its type annotation:

[code/Misc.hs:broken](Snip)

The problem with `broken` is that, despite all appearances, the type `b` in
`apply` is not the same `b` in `broken`. Haskell thinks it knows better than us
here, and introduces a new type variable for `apply`. The result of this is
effectively as though we had instead written the following:

[code/Misc.hs:brokenWhy](Snip)

Hindley--Milner seems to take the view that types should be "neither seen nor
heard," and an egregious consequence of this is that type variables have no
notion of scope.  This is why the example fails to compile---in essence we've
tried to reference an undefined variable, and Haskell has "helpfully" created a
new one for us. The Haskell Report provides us with no means of referencing type
variables outside of the contexts in which they're declared.

Fortunately this situation is greatly improved by `-XScopedTypeVariables`
(implied by `GHC2021`.) When enabled, it allows us to bind type variables and
refer to them later. However, this behavior is only turned on for types that
begin with an explicit `forall` quantifier. For example, with
`-XScopedTypeVariables`, `broken` is still broken, but the following works
instead:

[code/Misc.hs:working](Snip)

The `forall a b.` quantifier introduces a type scope, and exposes the type
variables `a` and `b` to the remainder of the function's definition. This allows
us to reuse `b` when adding the type signature to `apply`, rather than
introducing a *new* type variable as it did before.

We can now talk about types, but are still left without a good way of
*instantiating* types. If we wanted to specialize `fmap` to `Maybe`, for
example, the only solution sanctioned by the Haskell Report is to add an inline
type signature.

If we wanted to implement a function that provides a `String` corresponding to a
type's name, it's unclear how we could do such a thing. By default, we have no
way to explicitly pass type information, and so even *calling* such a function
would be difficult.

Some older libraries often use a `Proxy` parameter in order to help with these
problems. Its definition is this:

[code/Misc.hs:Proxy](Snip)

In terms of value-level information content, `Proxy` is exactly equivalent to
the unit type `()`. But it also has a phantom type parameter `a`, whose only
purpose is to allow users to keep track of a type, and pass it around like a
value.

For example, the module `Data.Typeable` provides a mechanism for getting
information about types at runtime. This is the function `typeRep`, whose type
is `Typeable a => Proxy a -> TypeRep`. Again, the `Proxy`'s only purpose is to
let `typeRep` know which type representation we're looking for.  As such,
`typeRep` has to be called as `typeRep (Proxy :: Proxy Bool)`.


### Type Applications

Clearly, Haskell's inability to directly specify types has ugly user-facing
ramifications. The extension `-XTypeApplications` (again, implied by `GHC2021`)
patches this glaring issue in the language.

`-XTypeApplications`, as its name suggests, allows us to directly apply types to
expressions. By prefixing a type with an `@`, we can explicitly fill in type
variables. This can be demonstrated in GHCi:

> TODO(sandy): do we need a section on visibility? probably!

```{ghci=code/TypeApps.hs}
:set -XTypeApplications
:t fmap
:t fmap @Maybe
```

While `fmap` lifts a function over any functor `f`, `fmap @Maybe` lifts a
function over `Maybe`. We've applied the type `Maybe` to the polymorphic
function `fmap` in the same way we can apply value arguments to functions.

There are two rules to keep in mind when thinking about type applications. The
first is that types are applied in the same order they appear in a type
signature---including its context and `forall` quantifiers. This means that
applying a type `Int` to `a -> b -> a` results in `Int -> b -> Int`.  But type
applying it to `forall b a. a -> b -> a` is in fact `a -> Int -> a`.

Recall that typeclass methods have their context at the beginning of their type
signature. `fmap`, for example, has type `Functor f => (a -> b) -> f a -> f b`.
This is why we were able to fill in the functor parameter of `fmap`---because it
comes first!

The second rule of type applications is that you can avoid applying a type with
an underscore: `@_`. This means we can also specialize type variables which are
not the first in line. Looking again at GHCi, we can type apply `fmap`'s `a` and
`b` parameters while leaving `f` polymorphic:

```{ghci=code/TypeApps.hs}
:t fmap
:t fmap @_ @Int @Bool
```

Because types are applied in the order they're defined, in the presence of
`-XTypeApplications` types become part of a public signature. Changing the order
of type variables can break downstream code, so be careful when performing
refactors of this nature.

Pay attention to type order whenever you write a function that might be type
applied. As a guiding principle, the hardest types to infer should come first.
This will often require using `-XScopedTypeVariables` and an explicitly scoped
`forall`.

`-XTypeApplications` and `-XScopedTypeVariables` are the two most fundamental
extensions in a type-programmer's toolbox. They go together hand in hand.


### Ambiguous Types

> TODO(sandy): and skolems!

Returning again to the example of `Data.Typeable`'s `typeRep` function, we can
use it to implement a function that will give us the name of a type. And we can
do so without requiring the `Proxy` parameter.

[code/TypeApps.hs:typeName](Snip)

There are two interesting things to note in `typeName`. At [2](Ann), `Proxy @a`
is written as shorthand for `Proxy :: Proxy a`---this is because the `Proxy`
data constructor has type `Proxy t`. The type variable `t` here is the first one
in its type signature, so we're capable of type applying it.  Type applications
aren't reserved for functions, they can be used anywhere types are present.

At [1](Ann) we see that the type `a` doesn't actually appear to the right of the
fat context arrow (`=>`). Because Hindley--Milner's type inference only works to
the right of the context arrow, it means the type parameter `a` in `typeName`
can never be correctly inferred. Haskell refers to such an unspecified type as
being ambiguous.

By default, Haskell will refuse to compile any programs with ambiguous types. We
can bypass this behavior by enabling the aptly-named `-XAllowAmbiguousTypes`
extension anywhere we'd like to define one. Actually *using* code that has
ambiguous types, will require `-XTypeApplications`.

The two extensions are thus either side of the same coin.
`-XAllowAmbiguousTypes` allows us to define ambiguously typed functions, and
`-XTypeApplications` enables us to call them.

We can see this for ourselves. By enabling `-XAllowAmbiguousTypes`, we can
compile `typeName` and play with it.

```{ghci=code/TypeApps.hs}
:set -XTypeApplications
typeName @Bool
typeName @String
typeName @(Maybe [Int])
```

Though this is a silly example, ambiguous types are very useful when doing
type-level programming. Often we'll want to get our hands on a term-level
representation of types---think about drawing a picture of a type, or about a
program that will dump a schema of a type. Such a function is almost always
going to be ambiguously typed, as we'll see soon.

However, ambiguous types aren't always this obvious to spot. To compare, let's
look at a surprising example. Consider the following closed type family:

[code/PrintfTypes.hs:AlwaysUnit](Snip)

Given this definition, are all of the following type signatures non-ambiguous?
Take a second to think through each example.

1. `AlwaysUnit a -> a`
2. `b -> AlwaysUnit a -> b`
3. `Show a => AlwaysUnit a -> String`

The third example here is, in fact, ambiguous. But why? The problem is that it's
not clear which `Show a` instance we're asking for! Even though there is an `a`
in `Show a => AlwaysUnit a -> String`, we're unable to access it---`AlwaysUnit
a` is equal to `()` for all `a`s!

More specifically, the issue is that `AlwaysUnit` doesn't have an inverse;
there's no closed type family `Inverse` such that `Inverse (AlwaysUnit a)`
equals `a`.  In mathematical lingo, this lack of an inverse is known as
*non-injectivity.*

Because `AlwaysUnit` is non-injective, we're unable to learn what `a` is, given
`AlwaysUnit a`.

Consider an analogous example from cryptography; just because you know the hash
of someone's password is `1234567890abcdef` doesn't mean you know what the
password is; any good hashing function, like `AlwaysUnit`, is *one way*.  Just
because we can get there doesn't mean we can also come back again!

The solution to non-injectivity is to give GHC some other way of determining the
otherwise ambiguous type. This can be done like in our examples by adding a
`Proxy a` parameter whose only purpose is to drive inference, or it can be
accomplished by enabling `-XAllowAmbiguousTypes` at the definition site, and
using `-XTypeApplications` at the call-site to fill in the ambiguous parameter
manually.

