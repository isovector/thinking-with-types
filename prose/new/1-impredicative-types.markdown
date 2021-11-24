## Impredicative Types {.rev2}

`-XRankNTypes` and `-XScopedTypeVariables` are two language extensions that
attempt to make polytypes first-class citizens of the Haskell world. As an
example, if you've spent any time using rank-$n$ types in anger, you've
certainly run into a situation like the following.

Consider two implementations of a function:

[code/NoImpredicativeTypes.hs:ex](Snip)

and

[code/NoImpredicativeTypes.hs:badEx](Snip)

Surely these are equivalent, right? Peculiarly, GHC disagrees: it happily
accepts the first one, and yells something inscrutable about polytypes on the
second:

```
error:
  • Couldn't match type ‘b0’ with ‘forall x. x -> x’
    Expected: (forall x. x -> x) -> Int
      Actual: b0 -> Int
    Cannot instantiate unification variable ‘b0’
    with a type involving polytypes: forall x. x -> x
  • In the expression: const 0
    In an equation for ‘ex’: ex = const 0
```

Errors like these come up quite often when using everyday combinators to
manipulate functions with rank-$n$ types. Let's investigate why that is, and
what we can do about it.


### Polytypes

Types come in two varieties---*monotypes* and *polytypes.* Monotypes are those
types that are entirely concrete, and polytypes are any types which contain a
`forall` quantifier. Thus `Int -> Bool` is a monotype, but `forall a. a ->
String` is a polytype. All polymorphic functions have polytypes.

The usual way we think of polytypes is that they are the "generic" version of a
function, which we can "specialize" by instantiating the type variables, getting
a monotype out. Polytypes are the templates from which we stamp concrete
versions. But this is not the whole story---it's missing a particularly
important detail: *we are only allowed to instantiate type variables at
monotypes!*

Our inscrutable error message above was trying to tell us exactly this---that we
unwittingly instantiated a type variable at a polytype! How exactly that
happened requires us to dig into a few type signatures. First, let's remind
ourselves of `const`:

[code/NoImpredicativeTypes.hs:const](Snip)

and the bad definition of `ex`:

[code/NoImpredicativeTypes.hs:badEx](Snip)

To make `ex` typecheck, GHC needs to instantiate `const` by giving types to `a`
and `b`. It's no problem to instantiate `a ~ Int`. The problem comes when we
inevitably need to assign `b ~ (forall x. x -> x)`---there's that nasty polytype
unification GHC was worried about.

This property of being able to instantiate a type variable to a polytype is
known as *impredicativity*. Impredicativity rears its ugly head in one other
particularly salient context: trying to stick a rank-$n$ type into a container.
For example:

```{ghci=code/NoImpredicativeTypes.hs}
Just id :: Maybe (forall x. x -> x)
```

This example illustrates the same problem, here we're trying to instantiate
`Maybe a` at `a ~ (forall x. x -> x)`. But GHC is more forgiving, and gives us a
hint---"perhaps you intended to use `ImpredicativeTypes`."

And indeed, enabling `-XImpredicativeTypes` does away with all of these errors.
Problem solved?


### A Note on History

Long time Haskellers will likely be very nervous right now. Indeed,
`-XImpredicativeTypes` is a much maligned language extension. It was originally
added by GHC 6.10.1, released in 2008, and lingered for 13 long years in a state
of being "highly experimental, and certainly un-supported." Worse, minor type
errors would often cause GHC to suggest turning on the cursed extension. Those
of us from the long ago all thought "what a cool feature," disregarded the scary
warnings, and were subsequently burned.

And so you might be wondering why there's a chapter on impredicative types. It's
because as of GHC 9.2.1, this language extension is officially working and
supported. Have no fear!


###




[code/ImpredicativeTypes.hs:makeSerial](Snip)

[code/ImpredicativeTypes.hs:locking](Snip)

[code/ImpredicativeTypes.hs:dump](Snip)

[code/ImpredicativeTypes.hs:interleaved](Snip)

```{ghci=code/ImpredicativeTypes.hs}
interleaved
```


[code/ImpredicativeTypes.hs:serialized](Snip)

```{ghci=code/ImpredicativeTypes.hs}
serialized
```

