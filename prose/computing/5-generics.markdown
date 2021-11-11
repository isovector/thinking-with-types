## Generic Programming {.rev2}

When writing Haskell, we have two tools in our belt for introducing
polymorphism: parametric and ad-hoc polymorphism.

As a gentle recap, parametric polymorphism gives one definition for every
possible type (think `head :: [a] -> a`.) It's what you get when you write a
standard Haskell function with type variables. This flavor of polymorphism is
predictable---the same function must always do the same thing, regardless of the
types it's called with.

On the other hand, ad-hoc polymorphism allows us to write a different
implementation for every type---as made possible by typeclasses. Different
instantiations of ad-hoc polymorphism are meant to be *spiritually* similar,
though the particulars might be wildly different. Think of the `mempty`
function---it picks out a particular element from any monoidal type, but the
similarities between these values come only from context clues.

In this chapter, we will discuss a third variety of polymorphism---a sort of no
man's land between the parametric and the ad-hoc: *structural polymorphism.*
Structural polymorphism is ad-hoc in the sense of being different for each type,
but it is also *highly regular and predictable.* It's what's colloquially known
as "boilerplate." It's the boring, uninteresting code that is repetitive but
just different enough to be hard to automate away. While structural polymorphism
doesn't have any formal definition, it's the sort of thing you recognize when
you see it.

`Eq`, `Show` and `Functor` instances are good examples of structural
polymorphism---there's nothing interesting about writing these instances. The
tedium of writing boilerplate polymorphism is somewhat assuaged by the
compiler's willingness to write these particular instances for us.

Consider the `Eq` typeclass; while every type needs its own implementation of
`(==)`, instances witnessing structural[^not-quotient] equality are always of
the form:

[code/DeriveAnyClass.hs:eqFoo](Snip)

[^not-quotient]: As opposed to quotienting equality, in which we say that any
  particular value might have a non-unique representation.

There's no creativity involved in writing a structural `Eq` instance, nor should
there be.  The same data constructors are equal if and only if all of their
components are equal.

Structural polymorphism is mindless work to write, but needs to be done. In the
case of some of the standard Haskell typeclasses, GHC is capable of writing
these instances for you via the `deriving` machinery. Unfortunately, for custom
typeclasses we're on our own, without any bespoke support from the compiler!

As terrible as this situation appears, all hope is not lost. Using *generic
programming,* we can abstract over structural polymorphism and write first-class
programs much in the same way that GHC's internal `deriving` machinery does.


### Generic Representations

Recall that all types have a canonical representation as a
sum-of-products---that they can all be built from `Either`s of `(,)`s. For
example, `Maybe a`, which is defined as:

[code/Misc.hs:Maybe](Snip)

`Maybe a` has a canonical sum-of-products form as `Either () a`. This can be
proven via a (non-unique) isomorphism:

[code/DeriveAnyClass.hs:toCanonical](Snip)

[code/DeriveAnyClass.hs:fromCanonical](Snip)

Via `toCanonical` and `fromCanonical`, we can convert between `Maybe a` and
`Either () a` without losing any information.

Why is this an interesting fact? If we have a small number of primitive
building blocks, ad-hoc polymorphic code can be written over each. By
manipulating canonical forms, we can solve generic programming problems. The
technique is simple: transform a value into its canonical representation, solve
the problem in the canonical domain, and finally "untransform" it.

You might have spotted a problem here. Witnessing the isomorphism between values
and their canonical forms is itself a generic programming problem; at best,
we've just shuffled the problem. Thankfully, `GHC` has built-in support for
deriving these canonical isomorphisms, and we can piggy-back off of that. The
`GHC.Generics` module defines the machinery we will need:

[code/DeriveAnyClass.hs:Generic](Snip)

The associated type `Rep a` at [1](Ann) corresponds to the canonical form of the
type `a`. Notice, however, the kinds: while `a` has kind `kind:Type`, `Rep a` is
of kind `kind:Type -> Type`. We will investigate why this is the case in a
moment.

The functions `from` and `to` at [2](Ann) and [3](Ann) form the isomorphism
between `a` and `Rep a`. It always takes me a moment to remember which direction
is which---it helps to remember that the directionality here is *with respect
to the non-generic type!*

GHC's generic representations are compositions of a handful of
highly-polymorphic building blocks. Not only do we get access to the structure
of the data inside, but the `Rep` types also give us lots of metadata.

For an idea about what this thing might look like, let's analyze `Rep Bool`.

```{ghci=code/DeriveAnyClass.hs}
:kind! Rep Bool
```

Quite a mouthful. The `D1` and `C1` types are metadata about the type definition
and data constructors, respectively. Let's ignore these for a moment. At the
heart of `Rep Bool` are the `(:+:)` and `U1` types. These correspond to the
canonical sum and canonical unit, respectively. Cutting out some of the excess
data for a second, we can see the gentle shape of `Bool` peeking out.

[code/DeriveAnyClass.hs:RepBool](Snip)

Compare this against the definition of `Bool` itself.

[code/DeriveAnyClass.hs:Bool](Snip)

The `(:+:)` type is the generic analogue of the `|` that separates data
constructors from one another. Because, as data constructors, `True` and `False`
contain no information, each is isomorphic to the unit type `()`.  As a result,
the canonical representation of `Bool` is conceptually just `Either () ()`, or
in its `GHC.Generics` form as `... (... U1 :+: ... U1)`.

With some idea of what's going on, let's look again at `Rep Bool`.

```{ghci=code/DeriveAnyClass.hs}
:kind! Rep Bool
```

The `D1` and `C1` types contain metadata about `Bool`'s definition in source
code as promoted data kinds. `D1` describes its *type*---its name, where it
was defined, and whether or not it's a newtype.

`C1` describes a data constructor---its name, fixity definition, and whether or
not it has record selectors for its data.

Generic programming that is interested in any of this information can statically
extract it from these data kinds; code that isn't interested can easily ignore
it.  In my experience, very rarely will you need access to these things, but
it's nice to have the option.


### The Generic Building Blocks {.rev2}

The `GHC.Generics` module gives us six composable pieces from which `Rep`
type instances can be built. There are three for describing values, two for
combining values, and one for storing metadata. Let's quickly go over each.

The three sorts of types that `GHC.Generics` can describe are:

* **Nullary types** are represented via `V1`, which has no inhabitants. As such,
the only time you'll run into `V1` is for types that have no data constructors
(and thus no inhabitants.)

* **Unary types** are represented via `U1`, whose only inhabitants is `U1`. This
type is used for *nullary* data constructors like `True` and `False`.

* **Constant types** are represented via `K1 R a`, where `a` is the constant type
in question. For example, the `Int` in `data Foo = Foo Int` is represented by a
`K1 R Int` node. The `R` thing is an extension point that never ended up being
used; you can ignore it in all cases. `K1` has a single data constructor: `K1`.

Generic representations can be combined in the usual two ways:

* **Sum types** of `f` and `g` are represented via `f :+: g`. There are two
data constructors of `(:+:)`, namely `L1` (for `f`) and `R1` (for `g`.)

* **Product types** of `f` and `g` are represented via `f :*: g`. The generic
product has only one data constructor, namely `(:*:)`.

Finally, **metadata** of types is stored in `M1 tag meta` nodes. The `tag`
parameter can be one of `D` (for metadata on data types), `C` (for
constructors), or `S` (for record selectors.) `GHC.Generics` comes with type
synonyms for each of these cases, for example `type C1 = M1 C`. This design
seems slightly roundabout, but it has practical applications: if we are
disinterested in all metadata, we only need to give a rule for `M1`. But if we'd
like only a certain sort of metadata, we can use one of the type synonyms.

Of course, these type constructors aren't powerful enough to describe *every*
type. Noticeably absent here is support for GADTs and existential types, which
we will return to later.

Don't forget that you can always use `:kind!` in GHCi to inspect the `Rep`s of
any types you're interested in. Getting a good intuition for how `Rep`s are put
together will make your life significantly easier when doing generic
programming.


### Implementing Semigroups Generically {.rev2}

Enough theory. How do we actually get work done? Let's try our hand at doing
something useful: generically implementing `Semigroup` instances. Because
product types preserve `Semigroup`s, giving instances is both straightforward
*and* an $O(n)$ amount of work. To illustrate:

[code/Generic/Monoid.hs:BigProduct](Snip)

[code/Generic/Monoid.hs:BigProductSemigroup](Snip)

Any `Semigroup` instance over a product type like this is completely mechanical,
and grows linearly with the size of the product. This is an excellent use-case
for generic programming: there is an $O(1)$ (albeit with a rather large constant
factor) amount of work to solve a problem generically, so in common cases like
this, we will be saving effort.

The approach to generic programming is threefold:

1. Define a typeclass to act as a *carrier.*
2. Provide inductive instances of the class for the generic constructors.
3. Finally, write a helper function to lift the carrier class over the desired
   type.

We begin by defining our carrier typeclass. The carrier mirrors the typeclass
we'd like to derive, but is shaped to be able to give instances for the `Rep`
constructors.

A good convention is add a `G` prefix to the carrier typeclass. Because we are
implementing `Semigroup` generically, a good name is `GSemigroup`---but this is
merely convention!

[code/Generic/Monoid.hs:GSemigroup](Snip)

Our `GSemigroup` class has a single method, `gappend`, whose signature closely
matches `(<>) :: a -> a -> a`.

Notice that the type parameter `f` to `GSemigroup` has kind `kind:k -> Type`.
This is a quirk of `GHC.Generics`, and allows the same `Rep` machinery when
dealing with higher-kinded classes. For generic programming over things of
kind `kind:Type`, this parameter will always be a phantom, and we can safely
ignore it for now. The dummy type `x` in `gappend` is thus necessary only to
make everything kind check.

With our carrier defined, the next step is to provide instances for the generic
`Rep` constructors. A good approach when writing generic instances is to work
"inside-out." Start with the innermost constructors (`U1`, `V1`, and `K1`,) as
these are the base cases of our structural induction.

In this case, `U1` is the simplest, so we will start there. Recall that `U1`
represents a data constructor with no parameters, in which case it's just `()`
with a different name. Since in this case, we have no data which needs to be
semigroup-ed together, our implementation of `gappend` can simply give back
`U1`:

[code/Generic/Monoid.hs:GSemigroupU1](Snip)

We can also give an instance of `GSemigroup` for `V1`, which corresponds to
types that can't be constructed. It might seem silly to provide a `Semigroup`
instance for such types, but it costs us nothing. Consider instances over `V1`
as being vacuous; if you *could* give me a value of `V1`, I claim that I could
give you back a function combining it. Since you *can't* actually construct a
`V1`, then my claim can never be tested, and so we might as well consider it
true.

Strictly speaking, `V1` instances usually aren't necessary, but we might as well
provide one if we can.

[code/Generic/Monoid.hs:GSemigroupV1](Snip)

The `case` stuff at [1](Ann) is a Haskell trick for dealing with empty data
types. We need to case-scrutinize an empty value in order for GHC to learn that
the type is empty, and thus that no resulting value is necessary.

The one other case we need to consider is what should happen for concrete types
inside of data constructors? Such things are denoted via `K1`, and in this case,
we want to fall back on a `Semigroup` (*not* `GSemigroup`!) instance to combine
the two. This corresponds to our manual implementation of `Semigroup` for
`BigProduct`---at some point, we actually do want to combine things in terms of
their underlying `(<>)` function!

[code/Generic/Monoid.hs:GSemigroupK1](Snip)

With our base cases complete, we're ready to lift them over product types. By
giving an instance for `GSemigroup (f :*: g)` in terms of underlying
`GSemigroup` instances for `f` and `g`, the code writes itself. This case
implements the "zipping together" of values that we saw in the `BigProduct`
instance.

[code/Generic/Monoid.hs:GSemigroupProduct](Snip)

Next, we want to give an instance of `GSemigroup` for sum types---or do we? As
it happens, sums do not preserve semigroups, thus there is no sensible generic
implementation we can give for sum types. That's not to say sum types aren't
semigroups, just that we can't get it for free. By explicitly *not* giving a
`GSemigroup (f :+: g)` instance, we can ensure our generic program sputters out
on sum types. Alternatively, we could give a dummy instance with a custom type
error, but we will ignore that option in this chapter.

All that's left is to deal with the metadata nodes. Since our generic program
doesn't need to change its behavior based on the source names of its
constructors, or on whether something is a newtype, or the fixity of its
operators, we can ignore the metadata notes and simply lift a `GSemigroup`
instance over them all:

[code/Generic/Monoid.hs:GSemigroupM1](Snip)

This completes step two. We now have a generic program capable of lifting
semigroup instances. All that's left is to lift `gappend` over real (rather than
generic) values of our type:

[code/Generic/Monoid.hs:genericMappend](Snip)

The real work here is happening at [3](Ann), which uses `from` to get to the
generic representations, calls `gappend`, and then transforms the whole thing
back via `to`. But notice the type signature here---we need to assert both that
`a` has a representation ([1](Ann)), and that can `GSemigroup` over that
particular generic representation ([2](Ann)). These two constraints are
characteristic of all generic programs.

Let's convince ourselves that this works:

```{ghci=code/Generic/Monoid.hs}
let x = BigProduct (Sum 5) (All True) (Last Nothing) ["Erin"]
let y = BigProduct (Sum 2) (All False) (Last $ Just 5) ["Lee"]
genericMappend x y
genericMappend ("hello", Sum 2) ("world", Sum 5)
```

`genericMappend` is powerful step in the right direction. We can define actual
`Semigroup` instances in terms of it:

[code/Generic/Monoid.hs:BigProductGSemigroup](Snip)


Exercise

:   Write a generic program that implements `mempty` from the `Monoid` class.
    Hint: fewer generic building blocks are admissable when writing `gmempty`
    than were available for `gappend`.

Solution

:   [code/Generic/Monoid.hs:GMonoid](Snip)

    [code/Generic/Monoid.hs:GMonoidU1](Snip)

    [code/Generic/Monoid.hs:GMonoidK1](Snip)

    [code/Generic/Monoid.hs:GMonoidProduct](Snip)

    [code/Generic/Monoid.hs:GMonoidM1](Snip)

    [code/Generic/Monoid.hs:genericMempty](Snip)

/* TODO(sandy): */ stopping place


This is about as good as we can do for classes we haven't defined ourselves.
However, for our own typeclasses we can go further and have the compiler
actually write that last piece of boilerplate for us too. We'll get full access
to the `deriving` machinery.

To illustrate the point, let's define a new typeclass `MyEq`. For all intents
and purposes `MyEq` is exactly the same as `Eq`, except that we've defined it
ourselves.

[code/DeriveAnyClass.hs:MyEq](Snip)

Using `-XDefaultSignatures`, at [1](Ann) we can provide a default implementation
of `eq` in terms of `genericEq`. `-XDefaultSignatures` is necessary to provides
the correct `GEq (Rep a)` context.

Finally, by enabling `-XDeriveAnyClass`, we can convince the compiler to give us
an instance of `MyEq` for free!

[code/DeriveAnyClass.hs:Foo](Snip)

Notice how at [1](Ann), we simply ask for a derived instance of `MyEq`, and the
compiler happily gives it to us. We can fire up the REPL to see how we did:

```{ghci=code/DeriveAnyClass.hs}
:t eq
eq F0 F0
eq (F1 "foo") (F1 "foo")
eq F0 (F1 "hello")
eq (F1 "foo") (F1 "bar")
```


### Using Generic Metadata

JavaScript's lack of a proper type system is widely known. However, in an
attempt to add some degree of type-safety, its proponents recommend a thing
called JSON Schema. If you're unfamiliar with it, JSON Schema is, in its own,
words "a vocabulary that allows you to annotate and validate JSON documents."
It's sort of like a type system, but described in JSON itself.

For example, the following Haskell type:

[code/JSONSchema.hs:Person](Snip)

would be described in JSON Schema as:

```json
  { "title": "Person"
  , "type": "object"
  , "properties":
      { "name":  { "type": "string"  }
      , "age":   { "type": "integer" }
      , "phone": { "type": "string"  }
      , "permissions":
          { "type": "array", "items": { "type": "boolean" }}
      }
  , "required": ["name" , "age", "permissions"]
  }
```

When sharing data between Haskell and JavaScript, providing JSON Schema as a
common format between the two languages seems like it might help mitigate
JavaScript's weak typing. But writing JSON Schema by hand is no fun, and so we
find ourselves with a motivating example of generating code generically.

As always, we begin with a definition of the carrier typeclass. Such a thing
needs to produce a `Value` (`aeson`'s @cite:aeson representation of a JSON
value.) However, we'll also need to propagate information in order to fill the
`required` property. As such, we decide on a single method of type `Writer
[Text] Value`. The `[Text]` will be used to track the required properties, and
the `Value` is the schema we're building.

[code/JSONSchema.hs:GSchema](Snip)

Notice that `gschema` doesn't reference the `a` type parameter anywhere.  While
we *could* use a `Proxy` to drive the instance lookups the way we did for
`HasPrintf`, a cleaner interface is to enable `-XAllowAmbiguousTypes` and later
use `-XTypeApplications` to fill in the desired variable.

For our purposes, we will assume we only want to generate JSON Schema for
Haskell records. In fact, it will be an error to ask for a schema for any
sum-types, since it's not clear how to embed them into JSON.

Before diving in, we'll need some helper functions for manipulating JSON
objects. For example, we'll want to merge two of them by taking the union of
their properties.

[code/JSONSchema.hs:mergeObjects](Snip)

We will also write a helper function that takes a `KnownSymbol nm` and `tell`s
the corresponding term-level string.

[code/JSONSchema.hs:emitRequired](Snip)

```{ghci=code/JSONSchema.hs}
runWriter (emitRequired @"required property")
```

`symbolVal` is a function that converts a `kind:Symbol` into a `String`. It comes
from `GHC.TypeLits`. For example:

```{ghci=code/Printf.hs}
:t symbolVal
symbolVal (Proxy @"i am a symbol")
```

The `KnownSymbol` stuff in `symbolVal`'s type is simply a proof that GHC knows
what `kind:Symbol` we're talking about; it will automatically generate the
`KnownSymbol` instance for us, so it's nothing we need to worry about.

Anyway, in JSON Schema, the boolean type `Bool` is represented via `"boolean"`.
Along the same vein, integral types are `"integer"`, but all other numeric types
are simply `"number"`. User types should be serialized with their given name.
This is a good opportunity to use a closed type family to convert from Haskell
type names to their JSON Schema counterparts.

[code/JSONSchema.hs:ToJSONType](Snip)

Unfortunately, there is no straightforward means of getting the name of a type
as a symbol. We can use generic metadata to retrieve a type's name.

[code/JSONSchema.hs:RepName](Snip)

[code/JSONSchema.hs:TypeName](Snip)

```{ghci=code/JSONSchema.hs}
:kind! ToJSONType Double
:kind! ToJSONType String
:kind! ToJSONType [Int]
:kind! ToJSONType Person
```

Something we'll find ourselves generating often are objects of the form
`{"type": "foo"}`. The function `makeTypeObj` is type-applicable, and will use
the `ToJSONType` of the applied type.

[code/JSONSchema.hs:makeTypeObj](Snip)

```{ghci=code/JSONSchema.hs}
makeTypeObj @Int
```

One last helper function we'll need before getting to the meat of the
`GHC.Generics` code is to be able to wrap an object with the name of a property.
This will be used to build the `"properties"` property in the JSON Schema
document.

[code/JSONSchema.hs:makePropertyObj](Snip)

Like `makeTypeObj`, `makePropertyObj` also is intended to be called with a type
application. In this case, it takes a `Symbol` corresponding to the name of the
property to emit. These `Symbol`s will come directly from the `Rep` of the
data-structure's record selectors.

In order to get access to the record name, it's insufficient to simply define an
instance of `GSchema` for `K1`. By the time we get to `K1` we've lost access to
the metadata---the metadata is stored in an outer wrapper.  Instead, we can do
type-level pattern matching on `M1 S meta (K1 _ a)`. The `S` type is used as a
parameter to `M1` to describe *record selector metadata*.

[code/JSONSchema.hs:gschemaK1](Snip)

At [1](Ann), this instance says that the property `nm` is required. It then
builds and returns a property object.

```{ghci=code/JSONSchema.hs}
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Aeson.Encode.Pretty (encodePretty)
let pp = LC8.putStrLn . encodePretty
pp (makePropertyObj @"myproperty" (makeTypeObj @Bool))
```

There are other base cases of `M1 .. K1` we still need to handle, but we will
build the rest of the machinery first. If we have a product of fields, we need
to merge them together.

[code/JSONSchema.hs:gschemaTimes](Snip)

For coproduct types, we will simply error out as the JSON Schema documentation
is conspicuously quiet about the encoding of sums.

[code/JSONSchema.hs:gschemaPlus](Snip)

Because sum-types are not allowed, information about data constructors isn't
interesting to us. We simply lift a `GSchema` instance through `M1 C` (metadata
for data constructors.)

[code/JSONSchema.hs:gschemaM1C](Snip)

To close out our induction cases, we need an instance of `GSchema` for `M1
D`---type constructors. Here we have access to the type's name, and all of its
properties.

[code/JSONSchema.hs:gschemaM1D](Snip)

Finally, we need to run our `Writer [Text]` and transform that into the list of
required properties `"required"`. We'll use the opportunity to also act as our
interface between `a` and `Rep a`.

[code/JSONSchema.hs:schema](Snip)

`schema` already works quite well. It will dump out a JSON Schema for our
`Person` type, though the encoding won't work correctly with optional values,
lists or strings. Each of these corresponds to a different base case of `M1 ..
K1`, and so we can provide some overlapping instances to clear them up.

The easiest case is that of `Maybe a`, which we'd like to describe as a field of
`a`, though without calling `emitRequired`.

[code/JSONSchema.hs:gschemaMaybe](Snip)

This instance is identical to `K1 _ a` except for the omission of
`emitRequired`.

Lists are serialized oddly in JSON Schema; their type is `"array"`, but the
descriptor object comes with an extra property `"items"` which *also* contains a
`"type"` property:

```json
{ "type": "array", "items": { "type": "boolean" }}
```

We can implement this with an overlapping instance which targets `K1 _ [a]`.

[code/JSONSchema.hs:gschemaList](Snip)

This works well, but because in Haskell, `String`s are simply lists of `Char`s,
our emitted JSON Schema treats `String`s as arrays. The correct behavior for
`String` is the same as the default `K1 _ a` case, so we add yet another
overlapping instance.

[code/JSONSchema.hs:gschemaString](Snip)

This instance overrides the behavior for `[a]`, which in itself overrides the
behavior for `a`. Programming with typeclass instances is not always the most
elegant experience.

And we're done. We've successfully used the metadata in `GHC.Generics` to
automatically marshall a description of our Haskell data types into JSON Schema.
We didn't need to resort to using code generation---which would have complicated
our compilation pipeline---and we've written nothing but everyday Haskell in
order to accomplish it.

We can admire our handiwork:

```{ghci=code/JSONSchema.hs}
@import qualified Data.ByteString.Lazy.Char8 as LC8
@import Data.Aeson.Encode.Pretty (encodePretty)
@let pp = LC8.putStrLn . encodePretty
pp (schema @Person)
```

And, as expected, sum types fail to receive a schema with a helpful error
message.

```{ghci=code/JSONSchema.hs}
schema @Bool
```


### Performance

With all of the fantastic things we're capable of doing with `GHC.Generics`,
it's worth wondering whether or not we need to pay a runtime cost to perform
these marvels. After all, converting to and from `Rep`s probably isn't free.

If there is indeed a hefty cost for using `GHC.Generics`, the convenience to the
programmer might not be worthwhile. After all, code gets executed much more
often than it gets written. Writing boilerplate by hand is annoying and tedious,
but at least it gives us some understanding of what's going on under the hood.
With `GHC.Generics`, these things are certainly less clear.

There is good and bad news here. The good news is that usually adding `INLINE`
pragmas to each of your class' methods is enough to optimize away all usage of
`GHC.Generics` at compile-time.

The bad news is that this is only *usually* enough to optimize them away. Since
there is no separate compilation step when working with `GHC.Generics`, it's
quite a lot of work to actually determine whether or not your generic code is
being optimized away.

Thankfully, we have tools for convincing ourselves our performance isn't being
compromised. Enter the `inspection-testing`@cite:inspection-testing library.
`inspection-testing` provides a plugin to GHC which allows us to make assertions
about our generated code. We can use it to ensure GHC optimizes away all of our
usages of `GHC.Generics`, and generates the exact same code that we would have
written by hand.

We can use `inspection-testing` like so:

1. Enable the `{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}` pragma.
2. Enable `-XTemplateHaskell`.
3. Import `Test.Inspection`.
4. Write some code that exercises the generic code path. Call it `foo`, for
   example.
5. Add `inspect $ hasNoGenerics 'foo` to your top level module.

> TODO(sandy): don't need the plugin anymore

For example, if we wanted to show that the `schema` function successfully
optimized away all of its generics, we could add a little test to our project
like this:

[noncode/InspectionTesting.hs](Snip)

Easy as that. Now in the course of compiling your module, if your generic code
has any runtime overhead, GHC will refuse to continue. Unfortunately for us,
`inspection-testing` isn't magic and can't guarantee our implementation is as
good as a hand-written example, but at least it can prove the generic
representations don't exist at runtime.

In order to prove two implementations (eg. one written generically and one
written by hand) *are* equal, you can use `inspection-testing`'s `(===)`
combinator. `(===)` causes a compile-time error if the actual generate Core
isn't identical. This is often impractical to do for complicate usages of
`GHC.Generics`, but it's comforting to know that it's possible in principle.

There is a particularly egregious case that GHC is unable to optimize, however.
It's described colloquially as "functions that are too polymorphic." But what
does it mean to be *too polymorphic*?

This class of problems sets in when GHC requires knowing about the
functor/applicative/monad laws in order to perform the inlining, but the type
itself is polymorphic. That is to say, a generic function that produces a
`forall m. m a` will perform poorly, but `Maybe a` is fine. A good rule of thumb
is that if you have a polymorphic higher-kinded type, your performance is going
to go into the toolies.


### Kan Extensions

On the grasping hand, there is still good news to be found. Reclaiming our
performance from the clutches of too-polymorphic generic code isn't a
challenging exercise. The secret is to rewrite our types in terms of kan
extensions.

* Rather than `forall f. Functor f => f a`, instead use `forall f. Yoneda f a`
* Instead of `forall f. Applicative f => f a`, use `forall f. Curried (Yoneda f)
  (Yoneda f) a`
* Instead of `forall f. Monad f => f a`, use `forall f. Codensity f a`

These types `Yoneda`, `Curried` and `Codensity` all come from the
`kan-extensions`@cite:kan-extensions package. We'll talk more about these
transformations in a moment.

In essence, the trick here is to write our "too polymorphic" code in a form more
amenable to GHC's inlining abilities, and then transform it back into the
desired form at the very end. `Yoneda`, `Curried` and `Codensity` are tools that
can help with this transformation.

Consider the definition of `Yoneda`:

[code/Kan.hs:Yoneda](Snip)

When we ask GHCi about the type of `runYoneda`, an interesting similarity to
`fmap` emerges:

```{ghci=code/Kan.hs}
:t runYoneda
:t flip fmap
```

`Codensity`---our transformation for polymorphic `Monad`ic code---also bears a
similar resemblance.

```{ghci=code/Kan.hs}
:t runCodensity
:t (>>=)
```

And `Curried` which we used to transform polymorphic `Applicative` code also
shows this pattern, although it's a little trickier to see.

```{ghci=code/Kan.hs}
:t runCurried @(Yoneda _) @(Yoneda _)
:t flip (<*>)
```

This is not an accident. The `Functor` instance for `Yoneda` is particularly
enlightening:

[code/Kan.hs:FunctorYoneda](Snip)

Note the lack of a `Functor f` constraint on this instance! `Yoneda f` is a
`Functor` *even when* `f` *isn't.* In essence, `Yoneda f` gives us a instance of
`Functor` for free. Any type of kind `kind:Type -> Type` is eligible. There's lots of
interesting category theory behind all of this, but it's not important to us.

But how does `Yoneda` work? Keep in mind the functor law that `fmap f .  fmap g
= fmap (f . g)`. The implementation of `Yoneda`'s `Functor` instance abuses this
fact. All it's doing is accumulating all of the functions we'd like to `fmap` so
that it can perform them all at once.

As interesting as all of this is, the question remains: how does `Yoneda` help
GHC optimize our programs? GHC's failure to inline "too polymorphic" functions
is due to it being unable to perform the functor/etc. laws while inlining
polymorphic code. But `Yoneda f` is a functor even when `f` isn't---exactly by
implementing the `Functor` laws by hand. `Yoneda`'s `Functor` instance can't
possibly depend on `f`. That means `Yoneda f` is never "too polymorphic," and as
a result, acts as a fantastic carrier for our optimization tricks.

Finally, the functions `liftYoneda :: Functor f => f a -> Yoneda f a` and
`lowerYoneda :: Yoneda f a -> f a` witness an isomorphism between `Yoneda f a`
and `f a`. Whenever your generic code needs to do something in `f`, it should
use `liftYoneda`, and the final interface to your generic code should make a
call to `lowerYoneda` to hide it as an implementation detail.

This argument holds exactly when replacing `Functor` with `Applicative` or
`Monad`, and `Yoneda` with `Curried` or `Codensity` respectively.

