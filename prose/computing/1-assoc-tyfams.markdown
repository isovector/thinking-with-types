
## Associated Type Families


Let's return to our earlier discussion about `printf`. Recall, the concern
was that despite `printf` having a type that humans can understand, its many
bugs come from our inability to convince the compiler about this type.

One of Haskell's most profound lessons is a deep appreciation for types. With it
comes the understanding that `String`s are suitable only for
*unstructured* text. Our format "strings" most certainly *are*
structured, and thus, as the argument goes, they should not be `String`s.

But, if `printf`'s format string isn't really a string, what is it?

When we look only at the specifiers in the format string, we see that they're a
kind of type signature themselves. They describe not only the number of
parameters, but also the types of those parameters.

For example, the format string `"\%c\%d\%d"` could be interpreted in
Haskell as a function that takes a character, two integers, and returns a
string---the concatenation of pushing all of those parameters together. In
other words, `"\%c\%d\%d"` corresponds to the type `Char -> Int -> Int
-> String`.

But, a format string is not only specifiers; it can also contain arbitrary text
that is to be strung together between the arguments. In our earlier example,
this corresponds to format strings like `"some number: \%d"`. The type
corresponding to this function is still just `Int -> String`, but its actual
implementation should be `\textbackslash s -> "some number: " <> show s`.

After some thinking, the key insight here turns out that these format strings
are nothing more than a sequence of types and text to intersperse between them.
We can model this in Haskell by keeping a type-level list of `Type`s and
`Symbol`s. The `Type`s describe parameters, and the `Symbol`s are
literal pieces of text to output.


### Building Types from a Schema

We'll need a data-structure to store the format schema of a `printf` call.
This can be done by building a binary type constructor which is polykinded in
both of its parameters. The goal is to build a type-safe, heterogeneously-kinded
linked-list.

[code/Printf.hs:typeList](Snip)

The `(:<<)` symbol was chosen due to the similarly it has with C++'s `<<`
output stream operator, but has no other special meaning to Haskell or to us.

Notice here that `(:<<)` doesn't have any data constructors, so we are unable
to construct one of them at the term-level. This makes sense, as its only
purpose is to store type-level information.

```{ghci=code/Printf.hs}
:kind (:<<)
```

Indeed, `(:<<)` works as a cons-cell for our linked-list; we can chain them
together indefinitely and store everything we want at the type-level:

```{ghci=code/Printf.hs}
:kind! "hello " :<< String :<< "!"
```

Because of our `infixr 5 :<<` declaration, repeated applications of `(:<<)`
associate to the right as we'd expect.

Armed with a means of storing our format schema, our next step is to use it to
construct the proper type signature of our formatting function. Which is to say,
given *eg.* a type `Int :<< ":" :<< Bool :<< "!"`, we'd like to produce
the type `Int -> Bool -> String`. This sounds like a type-level function,
and so we should immediately begin to think about type families.

However, instead of using *closed* type families which are useful when
promoting functions from the term-level to the type-level, we instead will use
an associated type family. Associated type families are associated with a
typeclass, and provide a convenient way to bundle term-level code with computed
types.

Because typeclasses are our means of providing ad-hoc polymorphism, associated
type families allow us to compute ad-hoc types.

We'll talk about bundling term-level code in a moment, but first, we can
define our associated type family:

[code/PrintfTypes.hs:HasPrintf](Snip)

Here at [1](Ann) we're saying we have a typeclass `HasPrintf a`, of which
every instance must provide an associated type `Printf a` ([2](Ann)).
`Printf a` will correspond to the desired type of our formatting function,
and we will construct it in a moment.

While it's not strictly necessary to use an associated type family instead of a
closed one---they're equivalent in power---the ability to take advantage of
Haskell's overlapping typeclasses will greatly simplify our logic.

In the name of implementation parsimony, we will say our format types will
always be of the form `a :<< ...  :<< "symbol"`---that is to say that they'll
always end with a `Symbol`. Such a simplification gives us a convenient
base case for the structural recursion we want to build.

Structural recursion refers to the technique of
producing something by tearing a recursive structure apart into smaller and
smaller pieces, until you find a case simple enough you know how to handle. It's
really just a fancy name for "divide and conquer."

In our `printf` example, we will require three cases:

1. `HasPrintf (text :: Symbol)`
2. `HasPrintf a => HasPrintf ((text :: Symbol) :<< a)`
3. `HasPrintf a => HasPrintf ((param :: Type) :<< a)`

With these three cases, we can tear down any right-associative sequence of
`(:<<)`s via case 2 or 3 until we run out of `(:<<)` constructors. At that
point, we will finally be left with a `Symbol` that we can handle via case 1.

Case 1 corresponds to having no more parameters. Here there is not any
type-level recursion to be done, and so we should just return our desired output
type---a `String`.

[code/PrintfTypes.hs:baseInstance](Snip)

The second case corresponds to having additional text we want to inject into our
final formatted string. In this case, we don't have a parameter available to
consume, and so here we don't change the resulting simpler type of `Printf`.
Therefore, we define the associated type instance of `Printf` as:

[code/PrintfTypes.hs:textInstance](Snip)

This recursive definition is an acceptable thing to do, because a type instance
of `Printf a` comes from an instance of `HasPrintf a`---which we have as a
constraint on this instance of `HasPrintf`.

Case 3 is the most interesting; here we want to add our `param` type as a
parameter to the generated function. We can do that by defining `Printf` as an
arrow type that takes the desired parameter, and recurses.

[code/PrintfTypes.hs:paramInstance](Snip)

We're saying our formatting type requires a `param`, and then gives back our
recursively-defined `Printf a` type. Strictly speaking, the `Type` kind
signature here isn't necessary---GHC will infer it based on `param -> Printf
a`---but it adds to the readability, so we'll keep it.

As a general principle, making type-level programming as legible as possible
will make you and your coworkers' life much easier. Everyone will thank you
later.

We can walk through our earlier example of `Int :<< ":" :<< Bool :<< "!"` to
convince ourselves that `Printf` expands correctly. First, we see that
`Int :<< ":" :<< Bool :<< "!"` is an instance of case 3.

From here, we expand the definition of `Printf (param :<< a)` into `param
-> Printf a`, or, substituting for our earlier type equalities: `Int ->
Printf (":" :<< Bool :<< "!")`.

We continue matching `Printf (":" :<< Bool :<< "!")` and notice now that it
matches case 2, giving us `Int -> Printf (Bool :<< "!")`. Expansion again
follows case 3, and expands to `Int -> Bool -> Printf "!"`.

Finally, we have run out of `(:<<)` constructors, and so `Printf "!"`
matches case 1, where `Printf text = String`. Here our recursion ends, and we
find ourselves with the generated type `Int -> Bool -> String`, exactly the
type we were hoping for.

Analysis of this form is painstaking and time-intensive. Instead, in the future,
we can just ask GHCi if we got it right, again with the `:kind!`
command:

```{ghci=code/Printf.hs}
:kind! Printf (Int :<< ":" :<< Bool :<< "!")
```

Much easier.


### Generating Associated Terms

Building the type `Printf a` is wonderful and all, but producing a type
without any corresponding terms won't do us much good. Our next step is to
update the definition of `HasPrintf` to also provide a `format` function.

[code/Printf.hs:HasPrintf](Snip)

The type of `format` is a little odd, and could use an explanation. Looking
at the [2](Ann), we find a term of type `Proxy a`. This `Proxy` exists only
to allow Haskell to find the correct instance of `HasPrintf` from the
call-site of `format`. You might think Haskell would be able to find an
instance based on the `a` in `Printf a`, but this isn't so for reasons we
will discuss soon.

The parameter [1](Ann) is an implementation detail, and will act as an
accumulator where we can keep track of all of the formatting done by earlier
steps in the recursion.

Finally, `format` results in a `Printf a` at [3](Ann). Recall that
`Printf` will expand to arrow types if the formatting schema contains
parameters, and thus all of our additional formatting is hiding inside [3](Ann).

Our instance definitions for each of the three cases can be updated so they
correctly implement `format`.

In the first case, we have no work to do, so the only thing necessary is to
return the accumulator and append the final text to it.

[code/Printf.hs:baseInstance](Snip)

Case 2 is very similar; here we want to update our accumulator with the
`symbolVal` of `text`, but also structurally recursively call `format`. This
requires conjuring up a `Proxy a`, which we can do via `-XTypeApplications`:

[code/Printf.hs:textInstance](Snip)

All that's left is case 3, which should look familiar to the attentive reader.

[code/Printf.hs:paramInstance](Snip)

Notice the `param` parameter to our `format` function here---this
corresponds to the `param` parameter in case 3's `Printf` instance. For
any specifier, we use its `Show` instance to convert the parameter into a
string, and append it to our accumulator.

With all three of our cases covered, we appear to be finished. We can define a
helper function to hide the accumulator from the user, since it's purely an
implementation detail:

[code/Printf.hs:printf](Snip)

Firing up GHCi allows us to try it:

```{ghci=code/Printf.hs}
printf (Proxy @"test")
printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2
/wrongPrintf/printf/wrongPrintf (Proxy @(String :<< " world!")) "hello"
```

It works pretty well for our first attempt, all things considered. One
noticeable flaw is that `String`s gain an extra set of quotes due to being
`show`n. We can fix this infelicity by providing a special instance of
`HasPrintf` just for `String`s:

[code/Printf.hs:stringInstance](Snip)

Writing this instance will require the `-XFlexibleInstances` extension,
since the instance head is no longer just a single type constructor and type
variables. We mark the instance with the `{-\# OVERLAPPING \#-}` pragma
because we'd like to select this instance instead of case 3 when the parameter
is a `String`.

```{ghci=code/Printf.hs}
printf (Proxy @(String :<< " world!")) "hello"
```

Marvelous.

There is something to be noted about overlapping instances for type
families---that, in general, they're not allowed. The reason we can overlap
`param :<< a` and `String :<< a` is that they actually *agree* on the
type family instance. When `param ~ String`, both instances give
`Printf (param :<< a)` to be `String -> Printf a`.

What we've accomplished here is a type-safe version of `printf`, but by
recognizing that C++'s "format string" is better thought of as a "structured
type signature." Using type-level programming, we were able to convert such a
thing into a function with the correct type, that implements nontrivial logic.

This technique is widely-applicable. For example, the popular
`servant`@cite:servant library uses a similar type-level schema to describe
web APIs, and will generate typesafe servers, clients and interop specs for
them.

