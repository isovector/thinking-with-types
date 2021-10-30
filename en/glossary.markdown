






































































































<div class="gloss"><strong>CPS</strong><p>see *continuation-passing style*</p></div>

<div class="gloss"><strong>Coercible</strong><p>a typeclass showing two types have the same representation at runtime</p></div>

<div class="gloss"><strong>Contravariant</strong><p>a typeclass for "backwards"-`Functor`s</p></div>

<div class="gloss"><strong>Covariant</strong><p>another name for `Functor`</p></div>

<div class="gloss"><strong>FCF</strong><p>see *first class family*</p></div>

<div class="gloss"><strong>GADTs</strong><p>generalized algebraic data types. Useful for giving explicit, restrictive types on data constructors. Enabled via `-XGADTs`</p></div>

<div class="gloss"><strong>indexed monad</strong><p>a monadic structure which carries a piece of static state along with it. Indexed monads allow you to enforce protocols in the type system</p></div>

<div class="gloss"><strong>MPTC</strong><p>a multi-parameter typeclass. Enabled via `-XMultiParamTypeClasses`</p></div>

<div class="gloss"><strong>OverloadedLabels</strong><p>syntax for converting `Symbol`s into values. Used
via the syntax `\#mySymbol`{.haskell}, and desugared in terms of the `GHC.Overloadedlabels.fromLabel`{.haskell} function. Enabled via `-XOverloadedLabels`</p></div>

<div class="gloss"><strong>RankNTypes</strong><p>an extension which enables writing higher-rank types. Enabled
via `-XRankNTypes`</p></div>

<div class="gloss"><strong>ST trick</strong><p>a technique for scoping the lifetime of a piece of data via an existential variable</p></div>

<div class="gloss"><strong>ScopedTypeVariables</strong><p>a language extension which brings type variables into scope if their type had an explicit `forall`{.haskell} quantifier. For example, the type `forall a. a -> Int` brings `a` into scope, but the type `a -> Int` doesn't. Enabled via `-XScopedTypeVariables`</p></div>

<div class="gloss"><strong>TypeInType</strong><p>a language extension which allows you to use types as kinds, and vice versa. Enabled via `-XTypeInType`</p></div>

<div class="gloss"><strong>ad-hoc polymorphism</strong><p>another name for the overloading we get from typeclasses</p></div>

<div class="gloss"><strong>algebraic data type</strong><p>any type made up of sums, products and exponents types</p></div>

<div class="gloss"><strong>ambiguous type</strong><p>a type is an ambiguous when it is unable to be inferred from a callsite. See `-XAllowAmbiguousTypes` and use `-XTypeApplications` to disambiguate them</p></div>

<div class="gloss"><strong>associated type family</strong><p>a type family associated with a typeclass.  Instances of the class must provide an instance of the type family</p></div>

<div class="gloss"><strong>bifunctor</strong><p>a type which is a functor over its last two type parameters</p></div>

<div class="gloss"><strong>canonical product</strong><p>another name for `(,)`</p></div>

<div class="gloss"><strong>canonical representation</strong><p>every type is isomorphic to its canonical
representation---a type defined as a sum of products</p></div>

<div class="gloss"><strong>canonical sum</strong><p>another name for `Either`</p></div>

<div class="gloss"><strong>canonical unit</strong><p>another name for `()`</p></div>

<div class="gloss"><strong>cardinality</strong><p>the number of unique values inhabiting a type. Two types with the same cardinality are always isomorphic</p></div>

<div class="gloss"><strong>carrier</strong><p>informal name for a typeclass whose only purpose is to carry ad-hoc polymorphic implementations for generic methods</p></div>

<div class="gloss"><strong>closed type family</strong><p>a type family with all of its instances provided in its definition. Closed type families are a close analogue of functions at the type-level</p></div>

<div class="gloss"><strong>constraint synonym</strong><p>a technique for turning a type synonym of `Constraint`s into something partially-applicable. Performed by making a new typeclass with a superclass constraint of the synonym, and giving instances of it for free given the superclass constraint. For example, `class c a => Trick a`{.haskell} and `instance c a => Trick a`{.haskell}</p></div>

<div class="gloss"><strong>constraint synonym</strong><p>a technique for constructing constraints that can be
partially applied. Performed by creating a new typeclass with the desired
synonym as a superclass constraint, and then by giving a free instance of this
class for any types that have the constraint</p></div>

<div class="gloss"><strong>constraint trick</strong><p>the transformation of a multiparameter typeclass
instance from `instance Foo Int b` to `instance (a ~ Int) => Foo a b`. Useful for improving type inference when working with MPTCs</p></div>

<div class="gloss"><strong>constraint</strong><p>a type of kind `Constraint`. Constraints are the things
that exist to the left of the fat context arrow (`=>`{.haskell}).</p></div>

<div class="gloss"><strong>continuation-passing style</strong><p>the technique of taking (and subsequently
calling) a callback, rather than directly returning a value</p></div>

<div class="gloss"><strong>contravariant</strong><p>a type `T a` is contravariant with respect to `a` if
it can lift a function `a -> b` into a function `T b -> T a`</p></div>

<div class="gloss"><strong>correct by construction</strong><p>using the type system to ensure that incorrect
programs are inexpressible</p></div>

<div class="gloss"><strong>covariant</strong><p>a type `T a` is covariant with respect to `a` if
it can lift a function `a -> b` into a function `T a -> T b`. Another name
for a `Functor`</p></div>

<div class="gloss"><strong>defunctionalization</strong><p>a technique for replacing a family of functions with an opaque symbol, and moving the original logic into an `eval`{.haskell} function.  Used by *First Class Families*</p></div>

<div class="gloss"><strong>dependent pair</strong><p>a type that pairs a singleton with a value indexed by the
singleton</p></div>

<div class="gloss"><strong>dependent type</strong><p>a type which isn't known statically, which depends on
term-level values</p></div>

<div class="gloss"><strong>endomorphism</strong><p>a function of the form `a -> a`</p></div>

<div class="gloss"><strong>fcf</strong><p>a first class (type) family</p></div>

<div class="gloss"><strong>first class family</strong><p>a technique for building reusable, higher-order type
families via defunctionalization</p></div>

<div class="gloss"><strong>functional dependency</strong><p>an additional invariant added to a multiparameter
typeclass declaration saying that some of its type varaibles are entirely
determined by others. Primarily used to improve type inference</p></div>

<div class="gloss"><strong>higher rank</strong><p>another name for a rank-*n* type</p></div>

<div class="gloss"><strong>higher-kinded type</strong><p>a type which is parameterized by something other than
`Type`</p></div>

<div class="gloss"><strong>indexed monad</strong><p>a monad that passes additional, static information between
subsequent binds</p></div>

<div class="gloss"><strong>instance head</strong><p>the part of a typeclass instance that comes after the
context arrow (`=>`{.haskell})</p></div>

<div class="gloss"><strong>introduction</strong><p>another word for constructor</p></div>

<div class="gloss"><strong>invariant</strong><p>a higher-kinded type is said to be invariant in a type parameter if that parameter is in neither positive nor negative position</p></div>

<div class="gloss"><strong>isomorphism</strong><p>an isomorphism is a mapping between two things---primarily
types in this book. If two types are isomorphic, they are identical for all
intents and purposes</p></div>

<div class="gloss"><strong>kind error</strong><p>like a type error, but at the kind-level</p></div>

<div class="gloss"><strong>kind signature</strong><p>a declaration (inferred or specified) of a type's kind</p></div>

<div class="gloss"><strong>negative position</strong><p>a type variable which is contravariant with respect to
its data type</p></div>

<div class="gloss"><strong>nominal</strong><p>a type variable is at role nominal if it is an error to coerce that type into another type</p></div>

<div class="gloss"><strong>non-injectivity</strong><p>a property of type families. Something that is
non-injective does not have an inverse</p></div>

<div class="gloss"><strong>overloaded labels</strong><p>syntax for converting `Symbol`s into values. Used
via the syntax `\#mySymbol`{.haskell}, and desugared in terms of the `GHC.Overloadedlabels.fromLabel`{.haskell} function. Enabled via `-XOverloadedLabels`</p></div>

<div class="gloss"><strong>parametric polymorphism</strong><p>the polymorphism that arises from quantified type
variables</p></div>

<div class="gloss"><strong>phantom</strong><p>a type variable is at role phantom if it can safely be coerced
into any other type. Type parameters are called phantom if they aren't used at
the term-level</p></div>

<div class="gloss"><strong>positive position</strong><p>a type variable which is covariant with respect to its
data type</p></div>

<div class="gloss"><strong>product type</strong><p>any type that contains several other types simultaneously</p></div>

<div class="gloss"><strong>profunctor</strong><p>a type `T a b` is a profunctor if it is contravariant in
`a` and covariant with respect to `b`</p></div>

<div class="gloss"><strong>promoted data constructor</strong><p>the type that results from a data constructor
when lifting its type to the kind level. Enabled via `-XDataKinds`</p></div>

<div class="gloss"><strong>rank-n type</strong><p>a type that delays the instantiantion of its type variables,
allowing polymorphism to be preserved through a function call</p></div>

<div class="gloss"><strong>rank-n</strong><p>a rank-*n* type</p></div>

<div class="gloss"><strong>rank</strong><p>a function's degree of polymorphism</p></div>

<div class="gloss"><strong>reflexivity</strong><p>the property that an object has when it is in relationship with itself. For example, equality is reflexive, because something is always equal to itself</p></div>

<div class="gloss"><strong>representationally equal</strong><p>two types are representationally equal if they
have identical physical layouts in memory</p></div>

<div class="gloss"><strong>representational</strong><p>a type variable is at role representational if it can
be coerced into other type sthat are representationally equal to it</p></div>

<div class="gloss"><strong>rigid skolem</strong><p>a type variable that is both rigid and a skolem</p></div>

<div class="gloss"><strong>rigid</strong><p>a type that was explicitly specified by a programmer. A type that
was not inferred</p></div>

<div class="gloss"><strong>role system</strong><p>the system that ensures role annotations are not violated</p></div>

<div class="gloss"><strong>role</strong><p>a property of a type variable that describes how a data constructor
that owns it is allowed to be coerced</p></div>

<div class="gloss"><strong>role signature</strong><p>the declared roles for a data type's type parameters</p></div>

<div class="gloss"><strong>sigma type</strong><p>another name for dependent pair</p></div>

<div class="gloss"><strong>singleton</strong><p>a type with a single inhabitant. Can be abused to create an
isomorphism between types and terms</p></div>

<div class="gloss"><strong>skolem</strong><p>an existentially quantified variable</p></div>

<div class="gloss"><strong>strengthen</strong><p>the act of using a stricter role than is necessary</p></div>

<div class="gloss"><strong>structural polymorphism</strong><p>a technique for automating boilerplate code</p></div>

<div class="gloss"><strong>structural recursion</strong><p>tackling a problem by dividing it and conquering
the pieces</p></div>

<div class="gloss"><strong>structured logging</strong><p>logging real data types rather than only their string
representations</p></div>

<div class="gloss"><strong>sum of products</strong><p>a possible form that a type can be expressed in</p></div>

<div class="gloss"><strong>sum type</strong><p>a type with multiple data constructors</p></div>

<div class="gloss"><strong>symmetry</strong><p>the property that two objects have in a relationship when it is
bidirectional. For example, equality is symmetric, because if $a = b$ then
$b = a$
</p></div>

<div class="gloss"><strong>term</strong><p>a value of a type. Something that can exist at runtime</p></div>

<div class="gloss"><strong>tick</strong><p>the leading apostrophe in the name of a promoted data constructor</p></div>

<div class="gloss"><strong>transitivity</strong><p>the idea that if
$a \star b$ and
$b \star c$ then
$a \star c$ for any relation
$\star$
</p></div>

<div class="gloss"><strong>type family dependency</strong><p>a technique for adding injectivity to a type
family</p></div>

<div class="gloss"><strong>value type</strong><p>a type with kind `Type`</p></div>

<div class="gloss"><strong>variance</strong><p>the behavior a type has when transforming its type parameters</p></div>

<div class="gloss"><strong>Curry--Howard isomorphism</strong><p>TODO(sandy)</p></div>

