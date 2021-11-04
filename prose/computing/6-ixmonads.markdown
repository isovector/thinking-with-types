## Indexed Monads

### Definition and Necessary Machinery

Indexed monads are a generalization of monads that allow us to enforce pre- and
post-conditions on monadic actions. They're a great tool for describing
*protocols* and *contracts* at the type-level. Despite these great uses, indexed
monads have historically been hampered by issues of ergonomics, making this
technique less popular than it might otherwise be.

In this chapter, we will look at how we can statically enforce resource
allocation linearity. We will build a monad which tracks files that are open and
requires them to be closed exactly once. Failure to adhere to these principles
will result in the program not compiling.

To begin with, we can look at the definition of `IxMonad`, the typeclass which
gives us access to such things. We'll be using the definition from the `indexed`
@cite:indexed package.

[code/IxMonad.hs:classIxMonad](Snip)

`ibind` here is the "enriched" version of `(>>=)`, although note that it's had
its first two arguments swapped. In addition to the usual `m`, `a` and `b` type
variables we're familiar with when working with monads, `ibind` has an
additional three. These other type variables correspond to the "state" of the
monad at different times.

An indexed monadic action `m i j a` is one that produces an `a`, with
precondition `i` and post-condition `j`. `ibind` works by matching up the
post-condition of an action `m i j` with the precondition of another `m j k`. In
doing so, the intermediary condition `j` is eliminated, giving us the
precondition from the first action and the post-condition from the second (`m i
k`).

The `indexed` package provides the `IxMonad` typeclass, but doesn't actually
give us any instances for it. Most of the time we simply want to lift an
underlying monad to have this enriched indexed structure---so we can define a
type to help with that.

[code/IxMonad.hs:Ix](Snip)

Make sure `Ix` is defined as a `newtype` rather than a `data`.

The aggressive proliferation of type parameters in `Ix` might be self-evident to
some, but deserves to be explained.

* `m`---the underlying monad we want to lift into an indexed monad.
* `i`---preconditions on the monadic action.
* `j`---post-conditions on the monadic action.
* `a`---the type we're producing at the end of the day.

Indexed monads have their own indexed-version of the standard typeclass
hierarchy, so we will need to provide instances of all of them for `Ix`. The
first two can be implemented in terms of their `Prelude` definitions, since
their types don't conflict.

[code/IxMonad.hs:IxFunctor](Snip)

[code/IxMonad.hs:IxPointed](Snip)

Applicatives, however, require some special treatment. We notice that since all
of our type variables except for `m` and `a` are phantom, we should be able to
`coerce` the usual `Applicative` function into the right shape.  The
construction, however, is a little more involved due to needing to capture all
of the variables.

[code/IxMonad.hs:IxApplicative](Snip)

The type signature at [1](Ann) requires enabling the `-XInstanceSigs` extension,
in order to use `-XScopedTypeVariables` to capture the `a` and `b` variables.
Once we have them, `(<*>)` is trivially coerced at [2](Ann).

Our instance of `IxMonad` uses the same technique:

[code/IxMonad.hs:IxMonad](Snip)

Finally, with all of this machinery out of the way, we have a working `IxMonad`
to play with. But working directly in terms of `ibind` seems tedious. After all,
`do`-notation was invented for a reason.

Historically, here we were faced with a hard decision between two bad
alternatives---write all of our indexed monad code in terms of `ibind` directly,
or replace `do`-notation with something amenable to `IxMonad` via the
`-XRebindableSyntax`. However, the latter option infects an entire module, which
meant we were unable to use regular monadic `do` blocks anywhere in the same
file.

If you're unfamiliar with the `-XRebindableSyntax` extension, it causes
fundamental Haskell syntax to be desugared in terms of identifiers currently in
scope. For example, `do`-notation is usually desugared in terms of
`(Prelude.>>=)`---even if that function isn't in scope (via
`-XNoImplicitPrelude`.) In contrast, `-XRebindableSyntax` will instead desugar
`do`-notation into whatever `(>>=)` function we have in scope.  This extension
is rarely used in the wild, and its finer uses are outside the scope of this
book.

The indexed monad situation today is thankfully much better. We now have the
`do-notation`@cite:do-notation package, which generalizes `do`-notation to work
with monads and indexed monads simultaneously. This behavior is enabled by
adding the following lines to the top of our module.

[code/IxMonad.hs:usage](Snip)

```warning
  The `do-notation` package replaces the definition of `pure`. If you're
  importing it into the file that defines `Ix`, make sure to use `pure`
  from `Prelude` when defining the `IxPointed` instance.
```

With `do-notation` now enabled, we can witness the glory of what we've
accomplished. We can check the type of a `do`-block once without
`-XRebindableSyntax`, and then once again with it enabled.

```{ghci=code/Linear.hs}
:t do { undefined; undefined }
:set -XRebindableSyntax
:t do { undefined; undefined }
```


### Linear Allocations

Now that we're capable of transforming monads into indexed monads, what can we
do with this?

One particularly compelling use-case of indexed monads is ensuring that monadic
actions occur in the right order. In this section, we will write an `IxMonad`
which allows you to open file handles, and requires you to close each of them
exactly one time. Anything else will refuse to compile.

In order to implement such a thing, we'll need to track whether a file handle is
open or closed---and we'll need to do it at the type-level. The dumbest possible
thing that could work is to keep a type-level list whose elements represent the
open files. As we open files we can insert into it, and as we close them we can
delete from it. Elegant, no, but it will get the job done.

We'll also need a means of generating unique keys for file handles. A strictly
increasing `Nat` will do the trick, which leads us to this data definition:

[code/Linear.hs:LinearState](Snip)

`LinearState` exists solely to be used as a data kind. It will correspond to the
"index" of our indexed monad. Any given monadic operation will be parameterized
by the `LinearState` going into it and the `LinearState` coming out of it. We
can show this directly by writing a newtype around `Ix`, which specializes the
underlying monad, and the kinds of its indices.

[code/Linear.hs:Linear](Snip)

The `s` type parameter here is to be used with the ST trick (@Sec:ST trick) to
prevent file handles from leaking out of the `Linear` context.

`unsafeRunLinear` is unsafe in two ways---it lets us run arbitrary `Linear`
computations, including incomplete ones in which we haven't yet closed all of
our file handles. Additionally, it doesn't existentialize `s`, meaning file
handles can leak out of it. We'll write a safer alternative later.

We can make `Linear` a little more usable by introducing `openFile`.

[code/Linear.hs:openFile](Snip)

`openFile`'s term-level implementation merely lifts `System.IO.openFile`.
What's interesting about it is its type signature. At [1](Ann), we say this
function can be used for any `next` and `open`. However, the post-condition of
`openFile` is that `next` is incremented ([2](Ann)), and that we insert `next`
into the open set ([3](Ann)).

`openFile` returns a lifted `Handle` whose identifier is `next`. The `Handle`
itself is tied to the `Linear` via the ST-trick (the `s` parameter.)

The `Handle` type itself isn't very interesting either, it's simply a wrapper
around `System.IO.Handle` with the two phantom types specified by `openFile`.

[code/Linear.hs:Handle](Snip)

In order to define `closeFile`, we'll first need machinery to determine whether
or not a `Handle` is already in the open set. This turns out to be quite a
straight-forward FCF.

[code/Linear.hs:IsOpen](Snip)

Additionally, we'll need an FCF to compute the result of removing a handle from
the open set.

[code/Linear.hs:Close](Snip)

The definition of `closeFile` is rather uninteresting. It must ensure a file is
already open, and then remove it from the indexed state's open set.

[code/Linear.hs:closeFile](Snip)

Since we increment `next` in `openFile`, should we decrement it here? The answer
is no---`next` is used solely to generate a unique `Nat` for newly opened
handles. If it were decremented as handles were closed it would be pretty
trivial to get two `Handle`s with the same `key`. The type system would get
confused pretty quick if such a thing were to occur.

We're now in a position to write a safe version of `unsafeRunLinear`. It's safe
to run a `Linear` if its final state has no open files, assuming it had no open
files to begin with. We start the next id counter at `0`, and can rely on the
ST-trick to prevent these `Handle`s from leaking out.

[code/Linear.hs:runLinear](Snip)

Let's convince ourselves that everything works. The happy path is one where we
close our file after we're done with it.

```{ghci=code/Linear.hs}
let etcPasswd = openFile "/etc/passwd" ReadMode
:t runLinear (etcPasswd >>= closeFile)
```

No problems. What if we don't close our file?

```{ghci=code/Linear.hs}
@let etcPasswd = openFile "/etc/passwd" ReadMode
:t runLinear etcPasswd
```

This results in a disgusting type error, which could be cleaned up using the
techniques described @Sec:type errors.

What if we try to close a file more than once?

```{ghci=code/Linear.hs}
@let etcPasswd = openFile "/etc/passwd" ReadMode
:t runLinear (etcPasswd >>= \f -> closeFile f >> closeFile f)
```

This also rightfully fails to compile. The final thing we should test is what
happens if we attempt to return a `Handle` from a `Linear` block.

```{ghci=code/Linear.hs}
@let etcPasswd = openFile "/etc/passwd" ReadMode
:t runLinear (etcPasswd >>= \f -> closeFile f >> pure f)
```

As expected, here the ST-trick saves our bacon.

This is a good place to stop. Indexed monads are a great solution to enforcing
invariants on the *ordering* of monadic actions. Because they're slightly
hampered by their syntactic limitations, and as such probably shouldn't be the
first tool you reach for.

