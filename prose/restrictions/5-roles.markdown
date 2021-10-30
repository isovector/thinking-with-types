
## Roles

### Coercions



In Haskell, newtypes are guaranteed to be a zero-cost abstraction. What this
means is that, under the hood, a newtype has exactly the same memory
representation as the type it wraps. At runtime, there is no difference between
a newtype and its wrapped type. The distinction between the two is made up, and
exists only in the type system.

Given the following definitions from `base`, for example,

[code/Roles.hs:ZipList](Snip)
[code/Roles.hs:Sum](Snip)

then the following values are all representationally equal---they have
exactly the same physical representation in memory

<ul>
  * `[54, 46]`{.haskell}
  * `[Sum 54, Sum 46]`{.haskell}
  * `ZipList [54, 46]`{.haskell}
  * `ZipList [Sum 54, Sum 46]`{.haskell}
</ul>

`ZipList [54, 46]`{.haskell} is representationally equal to `[54, 46]`{.haskell} because
the wrapped list consists of the same bytes in memory as its unwrapped
counterpart. Likewise `[Sum 54, Sum 46]`{.haskell} is the same, because point-wise,
each element in the list is representationally equal. In the last example here
we see that representational equality is transitive.

This zero-cost property of newtypes has profound implications for performance.
It gives us the ability to *reinterpret* a value of one type as a value of
another---and do it in $O(0)$ time. This can be performed via the `coerce`{.haskell}
function.

[code/Roles.hs:coerce](Snip)

The `Coercible a b`  constraint is a proof that the types
`a` and `b` do, in fact, have the same runtime representation. Unless
explicitly prevented (discussed later,) a newtype is always `Coercible` with
its underlying type.  `Coercible` is a magic constraint. The compiler will
write instances of it for you, and in fact, insists on this---it's actually an
error to write your own!

```{ghci=code/Roles.hs}
instance Coercible a b
```

Anyway, `coerce`{.haskell} can be used to massage data from one type into another
without paying any runtime cost. As an example, if we wanted to sum a list
of `Int`s, we could use the `Sum Int` monoid instance.

[code/Roles.hs:slowSum](Snip)

While this works, it's not entirely satisfactory; it requires traversing the
entire list with an `fmap`{.haskell} just in order to get the right `Monoid`
instance in scope. This is an $O(n)$ we need to pay, for no reason other than to
satisfy the type system. In such a simple example, list fusion might optimize
away this penalty, but then again, it might not. And without looking at the
generated core, we have no way of knowing.

For comparison, we can instead use `coerce`{.haskell} to transform `[Int]` into
`[Sum Int]` in $O(0)$ time, giving us access to the right `Monoid` for
free.

[code/Roles.hs:fastSum](Snip)

As a general rule, if you ever find yourself writing `fmap NewtypeCtor`{.haskell}, it
should be replaced with `coerce`{.haskell}---unless the functor instance is
polymorphic, in which case the compiler will complain and refuse to compile the
code. Your runtime performance will thank you, and you'll be able to sleep
peacefully with the satisfaction of a job well done.

Because `Coercible` corresponds to representational *equality*, we
should expect it to follow all of the usual laws of equality.

<ul>
  * Reflexivity---`Coercible a a` is true for any type
    `a`
  * Symmetry---`Coercible a b` implies `Coercible b
    a`
  * Transitivity---given `Coercible a b` and
    `Coercible b c` we have `Coercible a c`
</ul>

By this line of reasoning, we see that it's perfectly acceptable to coerce a
`Sum a` into a `Product a`.

```{ghci=code/Roles.hs}
coerce (1867 :: Sum Int) :: Product Int
```

The line of reasoning here is that both `Sum Int` and `Product Int` are
newtypes over `Int`, therefore they are inter-coercible by transitivity.

A natural question about coercions is whether representationally equal types are
always safely interchangeable. They're not. To see why, consider the case of
`Data.Map.Map` from the `containers` package.

`Map k v` is a container providing map lookups with key `k` and value
`v`. It's represented as a balanced tree, ordered via an `Ord k` instance.
For example, look at the type of its `insert`{.haskell} method:

[code/Roles.hs:insert](Snip)

This `Ord k` instance is required in order to know where to put the resulting
`v` in the map. The consequence is that a `Map k v`'s layout in memory is
*entirely dependent* on the `Ord k` instance it was built with.
Normally typeclass coherence prevents us from shooting ourselves in the foot (by
switching out the `Ord k` instance in scope, for example) but `coerce`{.haskell}
softens this invariant.

For example, consider the newtype `Reverse` which flips around an underlying
`Ord` instance.

[code/Roles.hs:Reverse](Snip)
[code/Roles.hs:OrdReverse](Snip)

Even though `Reverse a` is safely `Coercible` with `a`, it is not the
case that `Map (Reverse k) v` can be safely coerced to `Map k v`---they
have completely different layouts in memory! At best, a `Map (Reverse k) v`
interpreted as a `Map k v` will fail to find keys; at worst, it will crash if
the container does unsafe things in the name of performance.

Notice however that the layout of `Map k v` does *not* depend on `v`;
we are free to safely coerce `Map k v` as `Map k v'` to our hearts'
content. Thankfully, Haskell knows both of these facts, and allows us to coerce
only when it's safe.

```{ghci=code/Roles.hs}
coerce (M.singleton 'S' True) :: M.Map Char (Reverse Bool)
coerce (M.singleton 'S' True) :: M.Map (Reverse Char) Bool
```

### Roles

The question, of course, is what differentiates `k` from `v`? Their
roles are different. Just as the type system ensures terms are
used correctly, and the kind system ensures types are logical, the role
system ensures coercions are safe.

Every type parameter for a given type constructor is assigned a role. Roles
describe how a type's representational equality is related to its parameters'
coercion-safety. There are three varieties of roles.

<ul>
  * nominal---the everyday notion of type-equality in Haskell,
    corresponding to the `a ~ b` constraint. For example, `Int` is
    nominally equal *only* to itself.
  * representational---as discussed earlier in this chapter; types
    `a` and `b` are representationally equal if and only if it's safe to
    reinterpret the memory of an `a` as a `b`.
  * phantom---two types are always phantom-ly equal to one another.
</ul>

In the newtype `Sum a`, we say that `a` *is at role*
`representational`{.haskell}; which means that if `Coercible a b => Coercible (Sum
a) (Sum b)`---that `Sum a` and `Sum b` are representationally equal
whenever `a` and `b` are!

This is also the case for `v` in `Map k v`. However, as we've seen above,
`Coercible k1 k2` does not imply `Coercible (Map k1 v) (Map k2 v)`, and
this is because `k` must be at role `nominal`{.haskell}. `Coercible (Map k1
v) (Map k2 v)` is only the case when `k1 ~ k2`, and so this nominal role
on `k` is what keeps `Map` safe.

The other role is `phantom`{.haskell}, and as you might have guessed, it is reserved
for phantom parameters. `Proxy`, for example, has a phantom type variable:

[code/Misc.hs:Proxy](Snip)

The type variable `a` is at role `phantom`{.haskell}, and as expected, `Coercible
(Proxy a) (Proxy b)` is always true. Since `a` doesn't actually ever exist at
runtime, it's safe to change it whenever we'd like.

There is an inherent ordering in roles; `phantom`{.haskell} types can be coerced in
more situations than `representational`{.haskell} types, which themselves can be
coerced more often than `nominal`{.haskell} types. Upgrading from a weaker role (usable
in more situations) to a stronger one is known as
strengthening it.

Just like types, roles are automatically inferred by the compiler, though they
can be specified explicitly if desired. This inference process is relatively
simple, and works as follows:

<ol>
  * All type parameters are assumed to be at role `phantom`{.haskell}.
  * The type constructor `(->)` has two `representational`{.haskell} roles; any
    type parameter applied to a `(->)` gets upgraded to
    `representational`. Data constructors count as applying `(->)`.
  * The type constructor `(~)` has two `nominal`{.haskell} roles; any
    type parameter applied to a `(~)` gets upgraded to `nominal`{.haskell}.
    GADTs and type families count as applying `(~)`.
</ol>

```exercise
  What is the role signature of `Either a b`?
```

```solution
  `type role Either representational representational`{.haskell}
```


```exercise
  What is the role signature of `Proxy a`?
```

```solution
  `type role Proxy phantom`{.haskell}
```


While it's logical that a GADT counts as an application of `(~)`, it
might be less clear why types used by type families must be at role
`nominal`. Let's look at an example to see why.

Consider a type family that replaces `Int` with `Bool`, but otherwise
leaves its argument alone.

[code/Roles.hs:IntToBool](Snip)

Is it safe to say `a` is at role `representational`{.haskell}? Of course
not---`Coercible a b => Coercible (IntToBool a) (IntToBool b)` doesn't hold
in general. In particular, it fails whenever `a ~ Int`. As a result, any
type that a type family can potentially match on must be given role
`nominal`{.haskell}.

While roles are automatically inferred via the compiler, it's possible to
strengthen an inferred role to a less permissive one by providing a role
signature.

For example, binary search trees, like `Map`s, have an implicit memory
dependency on their `Ord` instance. Given a data-type:

[code/Roles.hs:BST](Snip)

After enabling `-XRoleAnnotations`, we're capable of providing a annotation
for it to strengthen the inferred role.

[code/Roles.hs:role](Snip)

The syntax for role annotations is `type role TyCon role1 role2 ...`{.haskell},
where roles are given for type variables in the same order they're defined.

Note that it's only possible to strengthen inferred roles, never weaken
them. For example, because the `v` in `BST v` is inferred to be at role
`representational`{.haskell}, we are unable to assert that it is at role `phantom`{.haskell}.
Attempting to do so will result in an error at compile-time.



