
## Custom Type Errors 


`OpenSum` and `OpenProduct` are impressive when used correctly. But the
type errors that come along with their misuse are nothing short of nightmarish
and unhelpful. For example:

```{ghci=code/OpenSum.hs}
let foo = inj (Identity True) :: OpenSum Identity '[Bool, String]
prj foo :: Maybe (Identity Int)
```

As a user, when we do something wrong we're barraged by meaningless errors about
implementation details. As library writers, this breakdown in user experience is
nothing short of a failure in our library. A type-safe library is of no value if
nobody knows how to use it.

Fortunately, GHC provides the ability to construct custom type errors. The
module `GHC.TypeLits` defines the type `TypeError` of kind
`ErrorMessage -> k`. The semantics of `TypeError` is that if GHC is ever
asked to solve one, it emits the given type error instead, and refuse to
compile. Because `TypeError` is polykinded, we can put it anywhere we'd like
at the type-level.

The following four means of constructing `ErrorMessage`s are available to
us.



<ul>
  * `'Text` (of kind `Symbol -> ErrorMessage`.) Emits the symbol verbatim. Note that this is *not* `Data.Text.Text`.
  * `'ShowType` (of kind `k -> ErrorMessage`.) Prints the name of the given type.
  * `'(:<>:)` (of kind `ErrorMessage -> ErrorMessage -> ErrorMessage`.) Concatenate two `ErrorMessage`s side-by-side.
  * `'(:\$\$:)` (of kind `ErrorMessage -> ErrorMessage -> ErrorMessage`.) Append one `ErrorMessage` vertically atop another.
</ul>

`TypeError` is usually used as a constraint in an instance context, or as the
result of a type family. As an illustration, we can provide a more helpful error
message when Haskell tries to solve a `Num` instance for functions. Recall
that the usual error message is not particularly useful:

```{ghci=code/OpenProduct.hs}
@:set -XFlexibleContexts
1 True
```

However, by bringing the following instance into scope:

[code/Misc.hs:pragmas](Snip)
[code/Misc.hs:showFunc](Snip)

We now get a more helpful solution to what might be wrong:

```{ghci=code/Misc.hs}
1 True
```

When evaluating `1 True`{.haskell}, Haskell matches the instance head of `Num (a ->
b)`, and then attempts to solve its context. Recall that whenever GHC sees a
`TypeError`, it fails with the given message. We can use this principle to
emit a friendlier type error when using `prj`{.haskell} incorrectly.

[code/OpenSum.hs:FriendlyFindElem](Snip)

Notice that `FriendlyFindElem` is defined as a *type family*, rather
than a *type synonym* as FCFs usually are. This is to delay the expansion
of the type error so GHC doesn't emit the error immediately. We now attempt to
find `t` in `ts`, and use `FromMaybe` to emit a type error in the case
that we didn't find it.

Rewriting `prj`{.haskell} to use `KnownNat (FriendlyFindElem t ts f ts)` instead of
`Member t ts` is enough to fix our error messages.

```{ghci=code/OpenSum.hs}
let foo = inj (Identity True) :: OpenSum Identity '[Bool, String]
friendlyPrj foo :: Maybe (Identity Int)
```

Let's return to the example of `insert`{.haskell} for `OpenProduct`. Recall the
`UniqueKey key ts ~ 'True` constraint we added to prevent duplicate keys.

\snipRename{OpenProduct}{oldInsert}{insert}

This is another good place to add a custom type error; it's likely to happen,
and the default one GHC will emit is unhelpful at best and horrendous at worst.
Because `UniqueKey` is already a type family that can't get stuck (ie. is
total), we can write another type family that will conditionally produce the
error.

[code/OpenProduct.hs:RequireUniqueKey](Snip)

`RequireUniqueKey` is intended to be called as `RequireUniqueKey
(UniqueKey key ts) key t ts`. The `Bool` at [1](Ann) is the result of
calling `UniqueKey`, and it is pattern matched on. At [2](Ann), if it's
`'True`, `RequireUniqueKey` emits the unit constraint `()`
. As a `Constraint`,
`()` is trivially satisfied.

Notice that at [3](Ann) we helpfully suggest a solution. This is good form in
any libraries you write. Your users will thank you for it.

We can now rewrite `insert`{.haskell} with our new constraint.

[code/OpenProduct.hs:insert](Snip)

```exercise
Add helpful type errors to `OpenProduct`'s `update`{.haskell} and
`delete`{.haskell} functions.
```

```solution
[code/OpenProduct.hs:FriendlyFindElem](Snip)
\snipRename{OpenProduct}{friendlyUpdate}{update}
\snipRename{OpenProduct}{friendlyDelete}{delete}

These functions could be cleaned up a little by moving the `FriendlyFindElem`
  constraint to `findElem`, which would remove the need for both constraints.
```


```exercise
Write a closed type family of kind `[k] -> ErrorMessage` that
pretty prints a list. Use it to improve the error message from
`FriendlyFindElem`.
```

```solution
[code/OpenProduct.hs:ShowList](Snip)
[code/OpenProduct.hs:FriendlyFindElem2](Snip)
```


```exercise
See what happens when you directly add a `TypeError` to the context
of a function (eg. `foo :: TypeError ... => a`{.haskell}). What happens? Do you know
why?
```

```solution
GHC will throw the error message immediately upon attempting to compile the
  module.

The reason why is because the compiler will attempt to discharge any extraneous
  constraints (for example, `Show Int` is always in scope, and so it can
  automatically be discharged.) This machinery causes the type error to be seen,
  and thus thrown.
```



