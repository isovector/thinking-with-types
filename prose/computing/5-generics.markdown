
## Generics

When writing Haskell, we have two tools in our belt for introducing
polymorphism: parametric and ad-hoc polymorphism.

Parametric polymorphism gives one definition
for every possible type (think `head :: [a] -> a`{.haskell}.) It's what you get when
you write a standard Haskell function with type variables. This flavor of
polymorphism is predictable---the same function must always do the same thing,
regardless of the types it's called with.

Ad-hoc polymorphism, like its name implies, allows
us to write a different implementation for every type---as made possible by
typeclasses.

But for our purposes, there's also a third category---a sort of no man's land
between the parametric and the ad-hoc: structural polymorphism.
Structural polymorphism is ad-hoc in the sense of being different for each type,
but it is also highly regular and predictable. It's what's colloquially known as
"boilerplate." It's the boring, uninteresting code that is repetitive but just
different enough to be hard to automate away. While structural polymorphism
doesn't have any formal definition, it's the sort of thing you recognize when
you see it.

`Eq`, `Show` and `Functor` instances are good examples of structural
polymorphism---there's nothing interesting about writing these instances. The
tedium of writing boilerplate polymorphism is somewhat assuaged by the
compiler's willingness to write some of them for us.

Consider the `Eq` typeclass; while every type needs its own implementation of
`(==)`{.haskell}, these implementations are always of the form:

[code/DeriveAnyClass.hs:eqFoo](Snip)

There's no creativity involved in writing an `Eq` instance, nor should there
be. The same data constructors are equal if and only if all of their components
are equal.

Structural polymorphism is mindless work to write, but needs to be done. In the
case of some of the standard Haskell typeclasses, GHC is capable of writing
these instances for you via the `deriving`{.haskell} machinery. Unfortunately, for
custom typeclasses we're on our own, without any direct support from the
compiler.

As terrible as this situation appears, all hope is not lost. Using
`GHC.Generics`{.haskell}, we're capable of writing our own machinery for helping GHC
derive our typeclasses, all in regular Haskell code.


### Generic Representations

Recall that all types have a canonical representation as a
sum-of-products---that they can all be built from `Either`s of `(,)`s. For
example, `Maybe a`, which is defined as:

[code/Misc.hs:Maybe](Snip)

`Maybe a` has a canonical sum-of-products form as `Either () a`. This can
be proven via an isomorphism:

[code/DeriveAnyClass.hs:toCanonical](Snip)

[code/DeriveAnyClass.hs:fromCanonical](Snip)

`toCanonical`{.haskell} and `fromCanonical`{.haskell} convert between `Maybe a` and
`Either () a` without losing any information. This witnesses an isomorphism
between the two types.

Why is this an interesting fact? Well, if we have a small number of primitive
building blocks, we can write code that is generic over those primitives.
Combined with the ability to convert to and from canonical representations, we
have the workings for dealing with structural polymorphism.

How can such a thing be possible? The secret is in the `-XDeriveGeneric`
extension, which will automatically derive an instance of `Generic` for you:

[code/DeriveAnyClass.hs:Generic](Snip)

The associated type `Rep a`{.haskell} at [1](Ann) corresponds to the canonical form of
the type `a`. Notice however the kinds; while `a` has kind `Type`,
`Rep a` is of kind `Type -> Type`. We will investigate why this is the
case in a moment.

The functions `from`{.haskell} and `to`{.haskell} at [2](Ann) and [3](Ann) form the isomorphism
between `a` and `Rep a`. Somewhat confusingly, their implied
directionality might be the opposite of what you'd expect. `to`{.haskell} converts
*to* the usual (type `a`{.haskell}) form, and `from`{.haskell} converts *from* the
usual form.

Let's look at `Rep Bool` for inspiration about what this thing might look
like.

```{ghci=code/DeriveAnyClass.hs}
:kind! Rep Bool
```

Quite a mouthful, but at it's heart the interesting parts of this are the
`(:+:)` and `U1` types. These correspond to the canonical sum and
canonical unit, respectively. Cutting out some of the excess data for a
second, we can see the gentle shape of `Bool` peeking out.

[code/DeriveAnyClass.hs:RepBool](Snip)

Compare this against the definition of `Bool` itself.

[code/DeriveAnyClass.hs:Bool](Snip)

The `(:+:)` type is the canonical analogue of the `|`{.haskell} that separates data
constructors from one another. And because `True`{.haskell} and `False`{.haskell} contain no
information, each is isomorphic to the unit type `()`{.haskell}. As a result, the
canonical representation of `Bool`{.haskell} is conceptually just `Either () ()`, or
in its `GHC.Generics` form as `... (... U1 :+: ... U1)`.

With some idea of what's going on, let's look again at `Rep Bool`.

```{ghci=code/DeriveAnyClass.hs}
:kind! Rep Bool
```

The `D1` and `C1` types contain metadata about `Bool`'s definition in
source code as promoted `-XDataKinds`. `D1` describes its *type*---its
name, where it was defined, and whether or not it's a newtype.

`C1` describes a data constructor---its name, fixity definition, and whether
or not it has record selectors for its data.

Structural polymorphism that is interested in any of this information is capable
of extracting it statically from these data kinds, and code that isn't can
easily ignore it. In my experience, very rarely will you need access to these
things, but it's nice to have the option.


### Deriving Structural Polymorphism




Armed with the knowledge of `Rep`, we can write an illustrative example of
generically deriving `Eq`. Of course, it's unnecessary because `Eq` is one
of those classes the compiler can write for us. Nevertheless, it's a good
introduction to the topic, and we must walk before we can run. We will look at
more challenging classes afterwards.

The approach to generically deriving structural polymorphism is threefold:

<ol>
  * Define a typeclass to act as a carrier.
  * Provide inductive instances of the class for the generic constructors.
  * Finally, write a helper function to map between the `Rep` and the
    desired type.
</ol>

We begin by defining our carrier typeclass. The carrier mirrors the typeclass
we'd like to derive, but is shaped to be able to give instances for the `Rep`
constructors.

A good convention is add a `G`{.haskell} prefix to the carrier typeclass---if you want
to derive `Eq` generically, call your carrier typeclass `GEq`.

[code/DeriveAnyClass.hs:GEq](Snip)

Our `GEq` class has a single method, `geq`{.haskell}, whose signature closely
matches `(==) :: a -> a -> Bool`{.haskell}.

Notice that the type parameter `a` to `GEq` has kind `Type -> Type`.
This is a quirk of `GHC.Generics`, and allows the same `Rep` machinery
when dealing with higher-kinded classes. When writing carrier classes for types
of kind `Type`, we will always saturate `a` with a dummy type `x`
whose only purpose is to make the whole thing kind check.

With our carrier defined, the next step is to provide instances for the generic
`Rep` constructors. A good approach when writing generic instances is to work
"inside-out." Start with the innermost constructors (`U1`, `V1`, and
`K1`,) as these are the base cases of our structural induction.

In this case, `U1` is the simplest, so we will start there. Recall that
`U1` represents a data constructor with no parameters, in which case it's
just `()` with a different name. Since `()` is always equal to itself, so
too should `U1` be.

[code/DeriveAnyClass.hs:geqU1](Snip)

Similarly for `V1` which corresponds to types that can't be constructed.
`V1` is the generic representation of `Void`---the `Type` with no
inhabitants. It might seem silly to provide an `Eq` instance for such types,
but it costs us nothing. Consider instances over `V1` as being vacuous; if
you *could* give me a value of `V1`, I claim that I could give you back
a function comparing it for equality. Since you *can't* actually construct
a `V1`, then my claim can never be tested, and so we might as well consider
it true.

Strictly speaking, `V1` instances usually aren't necessary, but we might as
well provide one if we can.

[code/DeriveAnyClass.hs:geqV1](Snip)

The one other case we need to consider is what should happen for concrete types
inside of data constructors? Such things are denoted via `K1`, and in this
case, we want to fall back on an `Eq` (*not* `GEq`!) instance to
compare the two. The analogous non-generic behavior for this is how the `Eq`
instance for `Maybe a` is `Eq a => Eq (Maybe a)`; most datatypes simply
want to lift equality over their constituent fields.

[code/DeriveAnyClass.hs:geqK1](Snip)

But why should we use an `Eq` constraint rather than `GEq`? Well we're
using `GEq` to help derive `Eq`, which implies `Eq` is the actual type
we care about. If we were to use a `GEq` constraint, we'd remove the ability
for anyone to write a non-generic instance of `Eq`!

With our base cases complete, we're ready to lift them over sums. Two sums are
equal if and only if they are the same data constructor (left or right), and if
their internal data is equal.

[code/DeriveAnyClass.hs:geqPlus](Snip)

We will also want to provide `GEq` instances for products---two pieces of
data side-by-side. Products are represented with the `(:*:)` type and data
constructors.

[code/DeriveAnyClass.hs:geqTimes](Snip)

Finally, we want to lift all of our `GEq` instances through the `Rep`'s
metadata constructors, since the names of things aren't relevant for defining
`Eq`. Fortunately, all of the various types of metadata (`D1`, `C1` and
`S1`) provided by `GHC.Generics` are all type synonyms of `M1`. Because
we don't care about any metadata, we can simply provide a `M1` instance and
ignore it.

[code/DeriveAnyClass.hs:geqM1](Snip)

This completes step two; we're now capable of getting `Eq` instances for
free. However, to convince ourselves that what we've done so far works, we can
write a function that performs our generic equality test.

[code/DeriveAnyClass.hs:genericEq](Snip)

```{ghci=code/DeriveAnyClass.hs}
genericEq True False
genericEq "ghc.generics" "ghc.generics"
```

`genericEq`{.haskell} is powerful step in the right direction. We can define actual
`Eq` instances in terms of it. Given our `Foo` datatype from earlier:

[code/DeriveAnyClass.hs:Foo1](Snip)

We can give an `Eq` instance with very little effort.

[code/DeriveAnyClass.hs:EqFoo](Snip)

```exercise
Provide a generic instance for the `Ord` class.
```

```solution
[code/DeriveAnyClass.hs:GOrd](Snip)

[code/DeriveAnyClass.hs:gordU1](Snip)

[code/DeriveAnyClass.hs:gordV1](Snip)

[code/DeriveAnyClass.hs:gordK1](Snip)

[code/DeriveAnyClass.hs:gordTimes](Snip)

[code/DeriveAnyClass.hs:gordPlus](Snip)

[code/DeriveAnyClass.hs:gordM1](Snip)

[code/DeriveAnyClass.hs:genericOrd](Snip)
```


```exercise
  Use `GHC.Generics` to implement the function `exNihilo :: Maybe
  a`{.haskell}. This function should give a value of `Just a` if `a` has exactly one
  data constructor which takes zero arguments. Otherwise, `exNihilo`{.haskell} should
  return `Nothing`{.haskell}.
```

```solution
[code/DeriveAnyClass.hs:GExNihilo](Snip)

[code/DeriveAnyClass.hs:gexNihiloU1](Snip)

[code/DeriveAnyClass.hs:gexNihiloV1](Snip)

[code/DeriveAnyClass.hs:gexNihiloK1](Snip)

[code/DeriveAnyClass.hs:gexNihiloTimes](Snip)

[code/DeriveAnyClass.hs:gexNihiloPlus](Snip)

[code/DeriveAnyClass.hs:gexNihiloM1](Snip)
```


This is about as good as we can do for classes we haven't defined ourselves.
However, for our own typeclasses we can go further and have the compiler
actually write that last piece of boilerplate for us too. We'll get full access
to the `deriving`{.haskell} machinery.

To illustrate the point, let's define a new typeclass `MyEq`. For all intents
and purposes `MyEq` is exactly the same as `Eq`, except that we've defined
it ourselves.

[code/DeriveAnyClass.hs:MyEq](Snip)

Using `-XDefaultSignatures`, at [1](Ann) we can provide a default
implementation of `eq`{.haskell} in terms of `genericEq`{.haskell}. `-XDefaultSignatures` is
necessary to provides the correct `GEq (Rep a)` context.

Finally, by enabling `-XDeriveAnyClass`, we can convince the compiler to give
us an instance of `MyEq` for free!

[code/DeriveAnyClass.hs:Foo](Snip)

Notice how at [1](Ann), we simply ask for a derived instance of `MyEq`, and
the compiler happily gives it to us. We can fire up the REPL to see how we did:

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

```
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
needs to produce a `Value` (`aeson`'s @cite:aeson representation of a
JSON value.) However, we'll also need to propagate information in order to fill
the `required`{.json} property. As such, we decide on a single method of type
`Writer [Text] Value`. The `[Text]` will be used to track the required
properties, and the `Value` is the schema we're building.

[code/JSONSchema.hs:GSchema](Snip)

Notice that `gschema`{.haskell} doesn't reference the `a` type parameter anywhere.
While we *could* use a `Proxy` to drive the instance lookups the way we
did for `HasPrintf`, a cleaner interface is to enable
`-XAllowAmbiguousTypes` and later use `-XTypeApplications` to fill in the
desired variable.

For our purposes, we will assume we only want to generate JSON Schema for
Haskell records. In fact, it will be an error to ask for a schema for any
sum-types, since it's not clear how to embed them into JSON.

Before diving in, we'll need some helper functions for manipulating JSON
objects. For example, we'll want to merge two of them by taking the union of
their properties.

[code/JSONSchema.hs:mergeObjects](Snip)

We will also write a helper function that takes a `KnownSymbol nm` and
`tell`{.haskell}s the corresponding term-level string.

[code/JSONSchema.hs:emitRequired](Snip)

```{ghci=code/JSONSchema.hs}
runWriter (emitRequired @"required property")
```

`symbolVal`{.haskell} is a function that converts a `Symbol` into a `String`.
It comes from `GHC.TypeLits`. For example:

```{ghci=code/Printf.hs}
:t symbolVal
symbolVal (Proxy @"i am a symbol")
```

The `KnownSymbol` stuff in `symbolVal`{.haskell}'s type is simply a proof that GHC
knows what `Symbol` we're talking about; it will automatically generate the
`KnownSymbol` instance for us, so it's nothing we need to worry about.

Anyway, in JSON Schema, the boolean type `Bool` is represented via
`"boolean"`{.json}.  Along the same vein, integral types are `"integer"`{.json}, but
all other numeric types are simply `"number"`{.json}. User types should be
serialized with their given name. This is a good opportunity to use a closed
type family to convert from Haskell type names to their JSON Schema
counterparts.

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
`{"type": "foo"}`{.json}. The function `makeTypeObj`{.haskell} is type-applicable, and
will use the `ToJSONType` of the applied type.

[code/JSONSchema.hs:makeTypeObj](Snip)

```{ghci=code/JSONSchema.hs}
makeTypeObj @Int
```

One last helper function we'll need before getting to the meat of the
`GHC.Generics` code is to be able to wrap an object with the name of a
property. This will be used to build the `"properties"`{.json} property in the
JSON Schema document.

[code/JSONSchema.hs:makePropertyObj](Snip)

Like `makeTypeObj`{.haskell}, `makePropertyObj`{.haskell} also is intended to be called with a
type application. In this case, it takes a `Symbol` corresponding to the
name of the property to emit. These `Symbol`s will come directly from the
`Rep` of the data-structure's record selectors.

In order to get access to the record name, it's insufficient to simply define an
instance of `GSchema` for `K1`. By the time we get to `K1` we've lost
access to the metadata---the metadata is stored in an outer wrapper.  Instead,
we can do type-level pattern matching on `M1 S meta (K1 \_ a)`. The `S`
type is used as a parameter to `M1` to describe *record selector
metadata*.

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
interesting to us. We simply lift a `GSchema` instance through `M1 C`
(metadata for data constructors.)

[code/JSONSchema.hs:gschemaM1C](Snip)

To close out our induction cases, we need an instance of `GSchema` for `M1
D`---type constructors. Here we have access to the type's name, and all of its
properties.

[code/JSONSchema.hs:gschemaM1D](Snip)

Finally, we need to run our `Writer [Text]` and transform that into the list
of required properties `"required"`{.json}. We'll use the opportunity to also act
as our interface between `a` and `Rep a`.

[code/JSONSchema.hs:schema](Snip)

`schema`{.haskell} already works quite well. It will dump out a JSON Schema for our
`Person` type, though the encoding won't work correctly with optional values,
lists or strings. Each of these corresponds to a different base case of `M1
.. K1`, and so we can provide some overlapping instances to clear them up.

The easiest case is that of `Maybe a`, which we'd like to describe as a field
of `a`, though without calling `emitRequired`{.haskell}.

[code/JSONSchema.hs:gschemaMaybe](Snip)

This instance is identical to `K1 \_ a` except for the omission of
`emitRequired`{.haskell}.

Lists are serialized oddly in JSON Schema; their type is `"array"`{.json}, but the
descriptor object comes with an extra property `"items"`{.json} which *also*
contains a `"type"`{.json} property:

```
{ "type": "array", "items": { "type": "boolean" }}
```

We can implement this with an overlapping instance which targets `K1 \_ [a]`.

[code/JSONSchema.hs:gschemaList](Snip)

This works well, but because in Haskell, `String`s are simply lists of
`Char`s, our emitted JSON Schema treats `String`s as arrays. The correct
behavior for `String` is the same as the default `K1 \_ a` case, so we add
yet another overlapping instance.

[code/JSONSchema.hs:gschemaString](Snip)

This instance overrides the behavior for `[a]`, which in itself overrides the
behavior for `a`. Programming with typeclass instances is not always the
most elegant experience.

And we're done. We've successfully used the metadata in `GHC.Generics` to
automatically marshall a description of our Haskell datatypes into JSON Schema.
We didn't need to resort to using code generation---which would have complicated
our compilation pipeline---and we've written nothing but everday Haskell in
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

If there is indeed a hefty cost for using `GHC.Generics`, the convenience to
the programmer might not be worthwhile. After all, code gets executed much more
often than it gets written. Writing boilerplate by hand is annoying and tedious,
but at least it gives us some understanding of what's going on under the hood.
With `GHC.Generics`, these things are certainly less clear.

There is good and bad news here. The good news is that usually adding
`INLINE`{.haskell} pragmas to each of your class' methods is enough to optimize away
all usage of `GHC.Generics` at compile-time.

The bad news is that this is only *usually* enough to optimize them away.
Since there is no separate compilation step when working with `GHC.Generics`,
it's quite a lot of work to actually determine whether or not your generic code
is being optimized away.

Thankfully, we have tools for convincing ourselves our performance isn't being
compromised. Enter the `inspection-testing`@cite:inspection-testing
library. `inspection-testing` provides a plugin to GHC which allows us to
make assertions about our generated code. We can use it to ensure GHC optimizes
away all of our usages of `GHC.Generics`, and generates the exact same code
that we would have written by hand.

We can use `inspection-testing` like so:


<ol>
  * Enable the `{-\# OPTIONS\_GHC -O -fplugin Test.Inspection.Plugin
    \#-}`{.haskell} pragma.
  * Enable `-XTemplateHaskell`.
  * Import `Test.Inspection`.
  * Write some code that exercises the generic code path. Call it `foo`{.haskell},
    for example.
  * Add `inspect \$ hasNoGenerics 'foo`{.haskell} to your top level module.
</ol>

For example, if we wanted to show that the `schema`{.haskell} function successfully
optimized away all of its generics, we could add a little test to our project
like this:

[code/InspectionTesting.hs](Snip)

Easy as that. Now in the course of compiling your module, if your generic code
has any runtime overhead, GHC will refuse to continue. Unfortunately for us,
`inspection-testing` isn't magic and can't guarantee our implementation is
as good as a hand-written example, but at least it can prove the generic
representations don't exist at runtime.

In order to prove two implementations (eg. one written generically and one
written by hand) *are* equal, you can use `inspection-testing`'s
`(===)`{.haskell} combinator. `(===)`{.haskell} causes a compile-time error if the actual
generate Core isn't identical. This is often impractical to do for complicate
usages of `GHC.Generics`, but it's comforting to know that it's possible in
principle.

There is a particularly egregious case that GHC is unable to optimize, however.
It's described colloquially as "functions that are too polymorphic." But what
does it mean to be *too polymorphic*?

This class of problems sets in when GHC requires knowing about the
functor/applicative/monad laws in order to perform the inlining, but the type
itself is polymorphic. That is to say, a generic function that produces a
`forall m. m a` will perform poorly, but `Maybe a` is fine. A good rule of
thumb is that if you have a polymorphic higher-kinded type, your performance is
going to go into the toolies.


### Kan Extensions



On the grasping hand, there is still good news to be found. Reclaiming our
performance from the clutches of too-polymorphic generic code isn't a
challenging exercise. The secret is to rewrite our types in terms of kan
extensions.

<ul>
  * Rather than `forall f. Functor f => f a`, instead use `forall f.
    Yoneda f a`
  * Instead of `forall f. Applicative f => f a`, use `forall f.
    Curried (Yoneda f) (Yoneda f) a`
  * Instead of `forall f. Monad f => f a`, use `forall f. Codensity f a`
</ul>

These types `Yoneda`, `Curried` and `Codensity` all come from the
`kan-extensions`@cite:kan-extensions package. We'll talk more about these
transformations in a moment.

In essence, the trick here is to write our "too polymorphic" code in a form
more amenable to GHC's inlining abilities, and then transform it back into the
desired form at the very end. `Yoneda`, `Curried` and `Codensity` are
tools that can help with this transformation.

Consider the definition of `Yoneda`:

[code/Kan.hs:Yoneda](Snip)

When we ask GHCi about the type of `runYoneda`{.haskell}, an interesting similarity to
`fmap`{.haskell} emerges:

```{ghci=code/Kan.hs}
:t runYoneda
:t flip fmap
```

`Codensity`---our transformation for polymorphic `Monad`ic code---also
bears a similar resemblance.

```{ghci=code/Kan.hs}
:t runCodensity
:t (>>=)
```

And `Curried` which we used to transform polymorphic `Applicative` code
also shows this pattern, although it's a little trickier to see.

```{ghci=code/Kan.hs}
:t runCurried @(Yoneda _) @(Yoneda _)
:t flip (<*>)
```

This is not an accident. The `Functor` instance for `Yoneda`
is particularly enlightening:

[code/Kan.hs:FunctorYoneda](Snip)

Note the lack of a `Functor f` constraint on this instance! `Yoneda f` is
a `Functor` *even when* `f` *isn't.* In essence, `Yoneda f`
gives us a instance of `Functor` for free. Any type of kind `Type ->
Type` is eligible. There's lots of interesting category theory behind all of
this, but it's not important to us.

But how does `Yoneda` work? Keep in mind the functor law that `fmap f .
fmap g = fmap (f . g)`{.haskell}. The implementation of `Yoneda`'s `Functor`
instance abuses this fact. All it's doing is accumulating all of the functions
we'd like to `fmap`{.haskell} so that it can perform them all at once.

As interesting as all of this is, the question remains: how does `Yoneda`
help GHC optimize our programs? GHC's failure to inline "too polymorphic"
functions is due to it being unable to perform the functor/etc. laws while
inlining polymorphic code. But `Yoneda f` is a functor even when `f`
isn't---exactly by implementing the `Functor` laws by hand. `Yoneda`'s
`Functor` instance can't possibly depend on `f`. That means `Yoneda f`
is never "too polymorphic," and as a result, acts as a fantastic carrier for
our optimization tricks.

Finally, the functions `liftYoneda :: Functor f => f a -> Yoneda f a`{.haskell} and
`lowerYoneda :: Yoneda f a -> f a`{.haskell} witness an isomorphism between `Yoneda
f a` and `f a`. Whenever your generic code needs to do something in `f`,
it should use `liftYoneda`{.haskell}, and the final interface to your generic code
should make a call to `lowerYoneda`{.haskell} to hide it as an implementation detail.

This argument holds exactly when replacing `Functor` with `Applicative` or
`Monad`, and `Yoneda` with `Curried` or `Codensity` respectively.


