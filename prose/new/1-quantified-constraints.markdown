## Quantified Constraints

Consider the `mtl` class `MonadTrans`:

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

The idea is that `MonadTrans` is the underlying interface for all monad
transformers `t` --- that, given a value in any monad `m`, we can `lift` it into
a value in `t m`. On first blush this sounds adequate, but it's missing an
important postcondition that we'd expect from a true monad transformer: that `t
m` is a monad!

To clarify this intuition, consider a contrived little program that is
polymorphic in `t`:

```haskell
getHead :: MonadTrans t => [a] -> t Maybe a
getHead as = do
  x <- lift $ listToMaybe as
  pure x
```

What does this program do when given the input `[1, 2, 3]`? Trick question ---
it doesn't even compile! Instead, GHC complains that there is no `Monad (t
Maybe)` instance. Which is obviously crazy to us humans, because `t` is a monad
transformer, and `Maybe` is a monad. This is the same issue as above, that
`MonadTrans t` doesn't capture the fact that `t` lifts entire monad instances.

An unsatisfying solution to this problem is to slightly modify the type
signature of `isEmpty`, adding a `Monad (t m)` constraint:

```haskell
getHead :: (Monad (t m), MonadTrans t) => [a] -> t Maybe a
```

It certainly *works*, but it's inelegant. You might think instead that we could
patch the `MonadTrans` class to try and add the necessary evidence:

```haskell
class MonadTrans t where
  lift :: (Monad m, Monad (t m)) => m a -> t m a
```

but this is in the wrong place! Trying to put a `Monad (t m)` constraint here
means we can't call `lift` unless we *already* have a proof that `t m` forms a
monad, which is exactly the problem we're trying to solve. Instead, in
situations like these, where we'd like to derive evidence of a type from the
presence of a typeclass, we should add a *superclass context.* The obvious thing
to try would be:

```haskell
class Monad (t m) => MonadTrans t where
  lift :: Monad m => m a -> t m a
```

Such an attempt is reasonable, but flawed --- unfortunately `m` isn't in scope
here. And even if it were, we don't want to say that `t m` is *always* a monad
--- merely that it's a monad *if and only if* `m` is a monad.

So what can we do? Enter `-XQuantifiedConstraints`, a language extention which
dramatically extends what sorts of `Constraint`s we can talk about. Under
`-XQuantifiedConstraints`, we gain the ability to use `forall` quantifiers in
`Constraint`s, as well as the ability to use implication (`=>`) itself. We will
take lots of time to explore both of these features, but for now, let's see how
this language extension can help with `MonadTrans`. A correct definition of
`MonadTrans` is the following[^not-yet-monad-trans]:

[^not-yet-monad-trans]: As of 2021, the real `MonadTrans` class in `mtl` doesn't
  yet use `-XQuantifiedConstraints`, but this is due only to historical and
  political reasons.

```haskell
class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a
```

The default constraint language (that is, under `-XNoQuantifiedConstraints`)
allows us to talk about *particular* instances, but not *families*[^math-class]
of instances. That is, the defult constraint language is *first-order.*
Quantified constraints changes that.

Our new defintion for `MonadTrans` has a `forall m. Monad m => Monad (t m)`
superclass constraint. What this means is that, if I want to give an instance an
instance of `MonadTrans` for some concrete type constructor `T` (of kind `(Type
-> Type) -> Type -> Type`{.kind}), I must also give an instance of the form:

```haskell
instance Monad (T m)
```

where `m` is a variable. It *will not work* if I instead give a series of
instances for every possible `m`, for example:

```haskell
instance Monad (T Maybe)
instance Monad (T [])
instance Monad (T (Either a))
...
```

What's important about the superclass constraint `forall m. Monad m => Monad (t
m)` is that we must be able to find a `Monad (t m)` instance for *any possible
choice of `m`*. The only way to do that is to let `m` be a variable ---
otherwise I could put on my adversarial hat and define a new type. No longer
would your collection of concrete instances be complete.

Notice however, that I said our instance must be of the form:

```haskell
instance Monad (T m)
```

rather than the more obvious:

```haskell
instance Monad m => Monad (T m)
```

> TODO(sandy): awful upcoming paragraph

The distinction is that both `instance Monad (T m)` and `instance Monad m =>
Monad (T m)` (and even `instance Applicative m => Monad (T m)`!) are allowed
under the quantified constraint `forall m. Monad m => Monad (t m)`. The
quantified `Monad m =>` piece means that instances are *allowed* to require a
`Monad m` (or anything implied by such --- either `Functor` or `Applicative`),
but they are not *required* to. Contrast this against the quantified constraint
`forall m. Monad (t m)` which states the instance *must* be `instance Monad (T
m)` --- in which no instance context is allowed!

