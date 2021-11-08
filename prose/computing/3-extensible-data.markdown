## Extensible Data

> TODO(sandy): intro

### Introduction

One heralded feature of dynamic languages---conspicuously missing in
Haskell---is their support for ad-hoc objects. Consider Python's dictionary
datatype:

```python
record = { 'foo': 12, 'bar': True } -- ! 1
record['qux'] = "hello" -- ! 2
record['foo'] = [1, 2, 3] -- ! 3
```

At [1](Ann), `record` can be said to have a (Haskell) type `{foo :: Int, bar ::
Bool}`. However, at [2](Ann) it also gains a `qux :: String` field. And at
[3](Ann), the type of `foo` changes to `[Int]`. Of course, Python doesn't
actually enforce any of these typing rules. It is, however, a good example of a
program that would be hard to represent in Haskell.

In this chapter, we will discuss how to build this sort of "extensible" record
type. As its dual, we will also investigate generalizing the `Either` type to
support an arbitrary number of potential types. These types are a good and
instructive use of type-level programming.

When looking at the canonical representations of types, we saw that all of
custom Haskell data-types can be built as *sums-of-products*. Sums correspond to
`Either a b`, and products correspond to the pair type `(a, b)`. However, just
because we can show an isomorphism between a type and this sum-of-products
doesn't mean such an encoding is *efficient*.

Because both `Either` and `(,)` are *binary operators*, using them for
arbitrarily large representations will require lots of extra constructors. In
the best case our representation will be a balanced binary tree (of the form
`Either (Either ...) (Either ...)`.) This corresponds to an overhead of
$O(\log{n})$ constructors. In the worst case, it degrades into a linked list,
and we pay for $O(n)$ constructors.

It seems, however, like we should be able to describe a sum of any number of
possibilities in $O(1)$ space, since it only ever contains one thing. Likewise,
that we can describe a product in $O(n)$ space (rather than the $O(n+\log{n})$
balanced binary tree, or $O(2n)$ linked list.)

The "trick" building extensible sum and product types is to use the type system
to encode the extra information. Anything we do at the type-level is free at
runtime. Because open sums have fewer moving pieces, we will work through them
first, and use their lessons to help build open products.


### Open Sums

By definition, an open sum is a container of a data whose type isn't known
statically. Furthermore, there are no guarantees that we know which types it
*might be,* since the list of types itself might be polymorphic.

Existential types are ones whose type has been forgotten by the type system. As
a result, we can use them to allow us to store *any* type inside of our open sum
container. We will constrain this later on.

Although they're not necessary, as we've seen, GADTs provide a nice interface
for defining and working with existential types.

[code/OpenSum.hs:OpenSum](Snip)

At [1](Ann) we declare `OpenSum` as a GADT. It has two parameters, `f` of kind
`kind:k -> Type` and `ts` of kind `kind:[k]`. We call `f` an *indexed type*,
which means it provides a `kind:Type` when given a `kind:k`. We will talk more
about these parameters in a minute.

The data constructor `UnsafeOpenSum` at [2](Ann) is thus named because, well,
its unsafe. We'll later provide tools for constructing `OpenSum`s safely, but
building one via the data constructor isn't guaranteed to maintain our
invariants. It's a common pattern in type level programming to label raw data
constructors as `Unsafe`, and write smart constructors that enforce the safety.

Looking at [3](Ann), we see that `OpenSum` is a container of `f t`, where `t`
has kind `kind:k`. Because `t` isn't mentioned in the return type of our
constructor ([4](Ann)), `t` is existential. Once `t` enters an `OpenSum`, it can
never be recovered. Knowledge of what `t` is is lost forever.

Returning to our parameters, we assign the semantics that the existential `t`
must be one of the elements of `ts`. If `ts ~ '[ Int, Bool, String ]`, we know
`t` was originally one of `Int`, `Bool` or `String`, though we do not
necessarily know which.

You might be wondering about the value of the `f` parameter. Its presence allows
us to add a common shape to all the members of `ts`. For example, `OpenSum ((->)
String) '[Int, Bool]` is capable of storing `String -> Int` and `String ->
Bool`.

Users who just want to store regular types with no additional structure should
let `f ~ Identity`.

The `OpenSum` data constructor also stores an `Int`, which we will use later as
a tag to "remember" what type the existential variable `t` had. It will be used
as an index into `ts`. For example, if we are storing the number `2` and `ts ~
'[A, B, C, D]`, we understand that originally, `t ~ C`.

A first-class type family can be used to find `t` in `ts`:

[code/OpenSum.hs:FindElem](Snip)

`FindElem` works by looking through `ts` and comparing the first element of each
tuple with `key`. The result of `FindIndex` is a `Maybe k`, which we then, at
[1](Ann), call the type-level equivalent of `fromJust` on.  If `FindIndex`
returned `'Nothing`, `FindElem` returns `Stuck`. A stuck type family is one
which can't reduce any further. We can exploit this property and ask whether
`FindElement` evaluated fully by asking whether it's a `KnownNat`.

[code/OpenSum.hs:Member](Snip)

A benefit of this approach is that a `KnownNat` constraint allows for
reification of the `kind:Nat` at the term-level---we can get an `Int`
corresponding to the `kind:Nat` we had. Additionally, the type-level nature of
`FindElem` means we pay no runtime cost for the computation.

[code/OpenSum.hs:findElem](Snip)

Armed with `findElem`, we are able to build a smart, type safe constructor for
`OpenSum`s.

[code/OpenSum.hs:inj](Snip)

`inj` allows injecting a `f t` into any `OpenSum f ts` so long as `t` is an
element somewhere in `ts`. However, nothing about this definition suggests `ts`
must be monomorphic; it can remain as a type variable so long as we propagate
the `Member t ts` constraint upwards.

But a sum type is no good without the ability to try to take things out of it.

[code/OpenSum.hs:prj](Snip)

Projections out of `OpenSum` are done by a runtime check at [1](Ann) that the
`Int` type tag inside of `OpenSum` is the same as the type we're trying to
extract it as. If they are the same, at [2](Ann) we know it's safe to perform an
`unsafeCoerce`, convincing the type checker to give back a (non-existential)
`t`. If the type tags are not the same, `prj` gives back `Nothing`.

We find ourselves wanting to reduce an `OpenSum` regardless of whats inside it.
`prj` is no help here---it either succeeds or it doesn't. If it fails, we the
programmers have learned something about the type inside (what it is *not*). The
types don't reflect that knowledge.

[code/OpenSum.hs:decompose](Snip)

The `decompose` function lets us perform induction over an `OpenSum`.  Either we
will get out the type corresponding to the head of `ts`, or we will get back a
`OpenSum` with fewer possibilities. A type tag of 0 corresponds to the head of
the type list, so it is unnecessary to call `findElem`. We maintain this
invariant by decrementing the type tag in the `Right` case.

In practice, it is also useful to be able to *widen* the possibilities of an
open sum. A new function, `weaken`, tacks a `x` type in front of the list of
possibilities.

Exercise

:   Write `weaken :: OpenSum f ts -> OpenSum f (x ': ts)`

Solution

:   [code/OpenSum.hs:weaken](Snip)


If we want to perform the same logic regardless of what's inside an `OpenSum`,
`prj` and `decompose` both feel inelegant. We introduce `match` eliminator which
consumes an `OpenSum` in $O(1)$ time.

[code/OpenSum.hs:match](Snip)

By using a rank-n type at [1](Ann), `match` is given a function that can provide
a `b` *regardless of what's inside the sum.* In this case, inspecting the type
tag isn't necessary.

There is a general principle to take away here. If it's too hard to do at the
type-level, it's OK to cheat and prove things at the term-level. In these cases,
`unsafeCoerce` can be your best friend---so long as you're careful to hide the
unsafe constructors.


### Open Products

Open products are significantly more involved than their sum duals. Their
implementation is made sticky due to the sheer number of moving pieces. Not only
do open products have several internal types it's necessary to keep track of,
they also require a human-friendly interface.

Our implementation will associate names with each type inside. These names can
later be used by the user to refer back to the data contained. As a result, the
majority of our implementation will be type-level book-keeping. Inside the
product itself will be nothing new of interest.

We begin by defining a container `Any` that will existentialize away its `k`
index. `Any` is just a name around the same pattern we did in `OpenSum`.

[code/OpenProduct.hs:Any](Snip)

This implementation of `OpenProduct` will optimize for $O(1)$ reads, and $O(n)$
writes, although other trade-offs are possible. We thus define `OpenProduct` as
a `Data.Vector` of `Any`s.

[code/OpenProduct.hs:OpenProduct](Snip)

`OpenProduct` is structured similarly to `OpenSum`, but its `ts` parameter is of
kind `[(Symbol, k)]`. The parameter `ts` now pulls double duty---not only does
it keep track of which types are stored in our `Vector Any`, but it also
associates names to those pieces of data. We will use these `Symbol`s to allow
the user to provide his own names for the contents of the product.

Creating an empty `OpenProduct` is now possible. It has an empty `Vector` and an
empty list of types.

[code/OpenProduct.hs:nil](Snip)

Because all data inside an `OpenProduct` will be labeled by a `Symbol`, we need
a way for users to talk about `Symbol`s at the term-level.

[code/OpenProduct.hs:Key](Snip)

By way of `-XTypeApplications`, `Key` allows users to provide labels for data.
For example, `Key @"myData"` is a value whose type is `Key "myData"`. Later
@Sec:OverloadedLabels we will look at how to lessen the syntax to build `Key`s.
These are the necessary tools to insert data into an open product. Given a `Key
key` and a `f k`, we can insert a `'(key, k)` into our `OpenProduct`.

[code/OpenProduct.hs](Snip){badInsert=insert}

Our function `insert` adds our new `'(key, k)` to the head of the type list, and
inserts the `f k` to the head of the internal `Vector`. In this way, it
preserves the invariant that our list of types has the same ordering as the data
in the `Vector`.

We can test this in GHCi with the `:t` command.

```{ghci=code/OpenProduct.hs}
/badInsert/insert/let result = badInsert (Key @"key") (Just "hello") nil
:t result
/badInsert/insert/:t badInsert (Key @"another") (Just True) result
```

While this looks good, there is a flaw in our implementation.

```{ghci=code/OpenProduct.hs}
@let result = badInsert (Key @"key") (Just "hello") nil
/badInsert/insert/:t badInsert (Key @"key") (Just True) result
```

Trying to `insert` a duplicate key into an `OpenProduct` succeeds. While this
isn't necessarily a *bug*, it's confusing behavior for the user because only one
piece of keyed data will be available to them. We can fix this with a type
family which computes whether a key would be unique.

[code/OpenProduct.hs:UniqueKey](Snip)

`UniqueKey` is the type-level equivalent of `null . filter (== key) . fst`. If
the `key` doesn't exist in `ts`, `UniqueKey` returns `'True`. We can now fix the
implementation of `insert` by adding a constraint to it that `UniqueKey key ts ~
'True`.

[code/OpenProduct.hs](Snip){oldInsert=insert}

GHCi agrees that this fixes the bug.

```{ghci=code/OpenProduct.hs}
@let result = oldInsert (Key @"key") (Just "hello") nil
/oldInsert/insert/:t oldInsert (Key @"key") (Just True) result
```

Informative it is not, but at least it fixes the bug. In the next chapter, we
will look at how to make this error message much friendlier.

To project data out from an open product, we'll first need to write a getter.
This requires doing a lookup in our list of types to figure out which index of
the `Vector` to return. The implementation is very similar to that for
`OpenSum`, except that we compare on the key names instead of the `k`s
themselves.

[code/OpenProduct.hs:FindElem](Snip)

[code/OpenProduct.hs:findElem](Snip)

We will also require another type family to index into `ts` and determine what
type to return from `get`. `LookupType` returns the `k` associated with the
`key`ed `kind:Symbol`.

[code/OpenProduct.hs:LookupType](Snip)

[code/OpenProduct.hs:get](Snip)

At [1](Ann), we say the return type of `get` is `f` indexed by the result of
`LookupType key ts`. Since we've been careful in maintaining our invariant that
the types wrapped in our `Vector` correspond exactly with those in `ts`, we know
it's safe to `unsafeCoerce` at [2](Ann).

As one last example for open products, let's add the ability to modify the value
at a key. There is no constraint that the new value has the same type as the old
one. As usual, we begin with a first-class type family that will compute our new
associated type list. `UpdateElem` scans through `ts` and sets the type
associated with `key` to `t`.

[code/OpenProduct.hs:UpdateElem](Snip)

The implementation of `update` is rather predictable; we update the value stored
in our `Vector` at the same place we want to replace the type in `ts`.

[code/OpenProduct.hs:update](Snip)

Exercise

:   Implement `delete` for `OpenProduct`s.

Solution

:   [code/OpenProduct.hs:delete](Snip)


Exercise

:   Implement `upsert` (update or insert) for `OpenProduct`s.

    Hint: write a type family to compute a `kind:Maybe Nat` corresponding to the
    index of the key in the list of types, if it exists. Use class instances to
    lower this kind to the term-level, and then pattern match on it to implement
    `upsert`.

Solution

:   This is a particularly involved exercise. We begin by writing a FCF to compute
    the resultant type of the `upsert`:

    [code/OpenProduct.hs:UpsertElem](Snip)

    Notice that at [1](Ann) we refer to `Placeholder1Of3`---which is a little
    hack to get around the lack of type-level lambdas in FCFs. Its definition is
    this:

    [code/OpenProduct.hs:Placeholder1Of3](Snip)

    [code/OpenProduct.hs:EvalPlaceholder](Snip)

    The actual implementation of `upsert` requires knowing whether we should
    insert or update. We will need to compute a `Maybe Nat` for the type in
    question:

    [code/OpenProduct.hs:UpsertLoc](Snip)

    And we can use a typeclass to lower this the `kind:Maybe Nat` into a `Maybe
    Int`:

    [code/OpenProduct.hs:FindUpsertElem](Snip)

    [code/OpenProduct.hs:FindUpsertNothing](Snip)

    [code/OpenProduct.hs:FindUpsertJust](Snip)

    Finally, we're capable of writing `upsert`:

    [code/OpenProduct.hs:upsert](Snip)


### Overloaded Labels

We earlier promised to revisit the syntax behind `Key`. Working with
`OpenProduct` doesn't yet bring us joy, mostly due to the syntactic noise behind
constructing `Key`s. Consider `get (Key @"example") foo`; there are nine bytes
of boilerplate syntactic overhead. While this doesn't seem like a lot, it weighs
on the potential users of our library. You'd be surprised by how often things
like these cause users to reach for lighter-weight alternatives.

Thankfully, there *is* a lighter-weight alternative. They're known as overloaded
labels, and can turn our earlier snippet into `get #example foo`.

Overloaded labels are enabled by turning on `-XOverloadedLabels`. This extension
gives us access to the `#foo` syntax, which gets desugared as `fromLabel @"foo"
:: a` and asks the type system to solve a `IsLabel "foo" a` constraint.
Therefore, all we need to do is provide an instance of `IsLabel` for `Key`.

[code/OpenProduct.hs:keyIsLabel](Snip)

Notice that the instance head is *not* of the form `IsLabel key (Key key)`, but
instead has two type variables (`key` and `key'`) which are then asserted to be
the same ([1](Ann)). This odd phrasing is due to a quirk with Haskell's instance
resolution, and is known as the constraint trick.

The machinery behind instance resolution is unintuitive. It will only match
instance heads (the part that comes after the fat constraint arrow `=>`). The
instance head of `(Eq a, Eq b) => Eq (a, b)` is `Eq (a, b)`. Once it has matched
an instance head, Haskell will work backwards and only then try to solve the
context (`(Eq a, Eq b)` in this example.) All of this is to say that the context
is not considered when matching looking for typeclass instances.

It is this unintuitive property of instance resolution that makes the constraint
trick necessary. Notice how when we're looking for key `#foo`, there is nothing
constraining our return type to be `Key "foo"`. Because of this, the instance
Haskell looks for is `IsLabel "foo" (Key a)`.

If our instance definition were of the form `instance IsLabel key (Key key)`,
this head *won't* match `IsLabel "foo" (Key a)`, because Haskell has no
guarantees `"foo" ~ a`. Perhaps we can reason that this must be the case,
because that is the only relevant instance of `IsLabel`---but again, Haskell has
no guarantees that someone won't later provide a different, non-overlapping
instance.

By using the constraint trick, an instance head of the form `IsLabel key (Key
key')` allows Haskell to find this instance when looking for `IsLabel "foo" (Key
a)`. It unifies `key ~ "foo"` and `key' ~ a`, and then will expand the context
of our instance. When it does, it learns that `key ~ key'`, and finally that `a
~ "foo"`. It's roundabout, but it gets there in the end.

The definition of `IsLabel` can be found in `GHC.OverloadedLabels`.

