
## Rank-N Types

### Introduction

Sometimes Haskell's default notion of polymorphism simply isn't polymorphic
*enough.* To demonstrate, consider a contrived function which takes the
`id :: a -> a` as an argument, and applies it to the number `5`.  Our
first attempt might look something like this:

[code/RankN.hs:brokenApply](Snip)

The reasoning here is that because `id` has type `a -> a`,
`applyToFive` should have type `(a -> a) -> Int`. Unfortunately,
Haskell disagrees with us when we try to compile this.

```
<interactive>:2:32: error:
    • Couldn't match expected type ‘a’ with actual type ‘Int’
```

We can't apply `f` to `5` because, as the error helpfully points out,
`a` is not an `Int`. Recall that under normal circumstances, the
*caller* of a polymorphic function is responsible for choosing which
concrete types those variables get. The signature `(a -> a) -> Int` promises
that `applyToFive` will happily take any function which returns the same type
it takes.

We wanted `applyToFive` to only be able to take `id` as a parameter, but instead
we've written a function which (if it compiled) would happily take any
*endomorphism*.[^endo] Because the choice of `a` is at the mercy of the caller,
Haskell has no choice but to reject the above definition of `applyToFive`---it
would be a type error to try to apply `5` to `not`, for example.

[^endo]: Functions which take and return the same type. For example, `not ::
  Bool -> Bool`, `show @String :: String -> String` and `id :: a -> a` are all
  endomorphisms, but `words :: String -> [String]` is not.

And so we come to the inevitable conclusion that, as is so often the case, the
compiler is right and we (or at least, our type) is wrong. The type of
`applyToFive` simply doesn't have enough polymorphism. But why not, and what can
we do about it?

The discrepancy comes from a quirk of Haskell's syntax. By default, the language
will automatically quantify our type variables, meaning that the type signature
`a -> a` is really syntactic sugar for `forall a. a -> a`. By enabling
`-XRankNTypes` we can write these desugared types explicitly. Comparing `id` and
`applyToFive` side-by-side is revealing.

[code/RankN.hs:id](Snip)

[code/RankN.hs:explicitBrokenApply](Snip)

Recall that we intended to give the type of `id` for the first parameter of
`applyToFive`. However, due to Haskell's implicit quantification of type
variables, we were lead astray in our attempts. This explains why `applyToFive`
above didn't compile.

The solution is easy: we simply need to move the `forall a.` part inside of the
parentheses.

[code/RankN.hs:applyToFive](Snip)

```{ghci=code/RankN.hs}
applyToFive id
```

In this chapter we will dive into what rank-n types are and what their more
interesting uses can do for us.


### Ranks

The `-XRankNTypes` is best thought of as making polymorphism *first-class*. It
allows us to introduce polymorphism anywhere a type is allowed, rather than only
on top-level bindings.[^let-bindings]

[^let-bindings]: And let-bound expressions, though this polymorphism is usually
  invisible to the everyday Haskell programmer.

While relaxing this restriction is "obviously a good thing", it's not without
its sharp edges. In general, type inference is undecidable in the presence of
higher-rank polymorphism.[^inferrable] Code that doesn't interact with such
things need not worry, but higher-rank polymorphism always requires an explicit
type signature.

[^inferrable]: Theoretically it's possible to infer types for rank-2
  polymorphism, but GHC doesn't at time of writing.

But what exactly *is* a rank?

In type-theory lingo, the rank of a function is the "depth" of its polymorphism.
A function that has no polymorphic parameters is rank 0. However, most, if not
all, polymorphic functions you're familiar with---`const :: a -> b -> a`, `head
:: [a] -> a`, etc---are rank 1.

The function `applyToFive` above is rank 2, because its `f` parameter itself is
rank 1.  In principle there is no limit to how high rank a function can be, but
in practice nobody seems to have gone above rank 3. And for good
reason---higher-rank functions quickly become unfathomable. Rather than
explicitly counting ranks, we usually call any function above rank-1 to be
rank-n or higher rank.

The intuition behind higher-rank types is that they are *functions which take
callbacks*. The rank of a function is how often control gets "handed off". A
rank-2 function will call a polymorphic function for you, while a rank-3
function will run a callback which itself runs a callback.

Because callbacks are used to transfer control from a called function back to
its calling context, there's a sort of a seesaw thing going on. For example,
consider an (arbitrarily chosen) rank-2 function `foo :: forall r. (forall a. a
-> r) -> r`. As the caller of `foo`, we are responsible for determining the
instantiation of `r`. However, the *implementation* of `foo` gets to choose what
type `a` is. The callback you give it must work for whatever choice of `a` it
makes.

This is exactly why `applyToFive` works. Recall its definition:

[code/RankN.hs:applyToFive](Snip)

Notice that the implementation of `applyToFive` is what calls `f`. Because `f`
is rank-1 here, `applyToFive` can instantiate it at `Int`. Compare it with our
broken implementation:

[code/RankN.hs:explicitBrokenApply](Snip)

Here, `f` is rank-0 because it is no longer polymorphic---the caller of
`applyToFive` has already instantiated `a` by the time `applyToFive` gets access
to it---and as such, it's an error to apply it to `5`. We have no guarantees
that the caller decided `a ~ Int`.

By pushing up the rank of `applyToFive`, we can delay who gets to decide the
type `a`. We move it from being the caller's choice to being the *callee's*
choice.

Even higher-yet ranks also work in this fashion. The caller of the function and
the implementations seesaw between who is responsible for instantiating the
polymorphic types. We will look more deeply at these sorts of functions later.


### The Nitty Gritty Details

It is valuable to formalize exactly what's going on with this rank stuff. More
precisely, a function gains higher rank every time a `forall` quantifier exists
on the left-side of a function arrow.

But aren't `forall` quantifiers *always* on the left-side of a function arrow?
While it might seem that way, this is merely a quirk of Haskell's syntax.
Because the `forall` quantifier binds more loosely than the arrow type `(->)`,
the everyday type of `id`,

[code/RankN.hs:forall1](Snip)

has some implicit parentheses. When written in full:

[code/RankN.hs:forall2](Snip)

it's easier to see that the arrow is in fact captured by the `forall`. Compare
this to a rank-*n* type with all of its implicit parentheses inserted:

[code/RankN.hs:forall3](Snip)

Here we can see that indeed the `forall a.` *is* to the left of a function
arrow---the outermost one. And so, the rank of a function is simply the number
of arrows its deepest `forall` is to the left of.

Exercise

:   What is the rank of `Int -> forall a. a -> a`? Hint: try adding the
    explicit parentheses.

Solution

:   `Int -> forall a. a -> a` is rank-1.


Exercise

:   What is the rank of `(a -> b) -> (forall c. c -> a) -> b`? Hint: recall that
    the function arrow is right-associative, so `a -> b -> c` is actually parsed
    as `a -> (b -> c)`.

Solution

:   `(a -> b) -> (forall c. c -> a) -> b` is rank-2.


Exercise

:   What is the rank of `((forall x. m x -> b (z m x)) -> b (z m a)) ->
    m a`? Believe it or not, this is a real type signature we had to write back
    in the bad old days before `MonadUnliftIO`!

Solution

:   Rank-3.


### The Codensity Monad {.rev2}

An interesting fact is that the types `a` and `forall r. (a -> r) -> r` are
isomorphic. This is witnessed by the following functions:

[code/RankN.hs:toCont](Snip)

[code/RankN.hs:fromCont](Snip)

Intuitively, we understand this as saying that having a value is just as good as
having a function that will give that value to a callback. Spend a few minutes
looking at `toCont` and `fromCont` to convince yourself you know why these
things form an isomorphism.

Types of the form `(a -> r) -> r` are known as being in "continuation-passing
style," or more tersely as "CPS."

Recall that isomorphisms are transitive. If we have an isomorphism `t1` &cong;
`t2`, and another `t2` &cong; `t3`, we must also have one `t1` &cong; `t3`.

Since we know that `Identity a` &cong; `a` and that `a` &cong; `forall r. (a ->
r) -> r`, we should expect the transitive isomorphism between `Identity a` and
CPS.  Since we know that `Identity a` is a `Monad` and that isomorphisms
preserve typeclasses, we should expect that CPS also forms a `Monad`.

We'll use a newtype as something to attach this instance to.

[code/RankN.hs:Codensity](Snip)

Exercise

:   Provide a `Functor` instance for `Codensity`. Hint: use lots of type holes,
    and an explicit lambda whenever looking for a function type. The
    implementation is sufficiently difficult that trying to write it point-free
    will be particularly mind-bending.

Solution

:   [code/RankN.hs:contFunctor](Snip)


Exercise

:   Provide the `Applicative` instances for `Codensity`.

Solution

:   [code/RankN.hs:contApplicative](Snip)


Exercise

:   Provide the `Monad` instances for `Codensity`.

Solution

:   [code/RankN.hs:contMonad](Snip)


One of the important use-cases of of `Codensity`'s `Monad` instance is that it
allows us to flatten JavaScript-style "pyramids of doom."

For example, imagine the following functions all perform asynchronous `IO` in
order to compute their values, and will call their given callbacks when
completed.

[code/RankN.hs:withVersionNumber](Snip)

[code/RankN.hs:withTimestamp](Snip)

[code/RankN.hs:withOS](Snip)

We can write a "pyramid of doom"-style function that uses all three callbacks to
compute a value:

[code/RankN.hs:releaseString](Snip)

Notice how the deeper the callbacks go, the further indented this code becomes.
We can instead use the `Codensity` (or `CodensityT` if we want to believe these functions
are actually performing `IO`) to flatten this pyramid.

[code/RankN.hs:releaseStringCodensity](Snip)

When written in continuation-passing style, `releaseStringCont` hides the fact
that it's doing nested callbacks.


Exercise

:   There is also a monad transformer version of `Codensity`. Implement it.

Solution

:   [code/RankN.hs:CodensityT](Snip)

    The `Functor`, `Applicative` and `Monad` instances for `CodensityT` are
    identical to `Codensity`.


### War Story: Improving Asymptotics with Codensity {.rev2}

> TODO(sandy): play up the personal stakes

Several years ago, I became really excited about free monads. This was spurred
on by an *expensive,* yet avoidable, bug stemming from an untestable codebase.
At a high level, free monads represent your program as a tree, allowing you to
inspect or reinterpret pieces of it. The trick is to build a data structure that
separates the *operations* of your monad stack from the monadic "book-keeping."

The book-keeping is done via the `Free` type:

[code/War/Polysemy.hs:Free](Snip)

which admits a `Monad` instance, for only the price of a `Functor f` instance:

[code/War/Polysemy.hs:MonadFree](Snip)

The details here aren't particularly important for the story at hand. Suffice it
to say that this `Monad (Free f)` instance does the monadic book-keeping. We can
engineer the operations that our monad is capable of by picking `f`. For
example, maybe we'd like to be able to read and write to the console:

[code/War/Polysemy.hs:Console](Snip)

Rather surprisingly, we now have enough to write real programs. For example,
here's one that prompts the user for their name, and then says hello:

[code/War/Polysemy.hs:sayHello](Snip)

Keep in mind that `sayHello` is nothing more than a data structure which
describes the above program. This is clearer when we desugar away the
`do`-notation:

[code/War/Polysemy.hs:sayHello2](Snip){sayHello2="sayHello"}

Of course, on its own, `sayHello` is nothing but a piece of syntax. It can't be
actually *run.* For that, we need to give `Console` an interpretation---that is,
a mapping from it into some other monad:

[code/War/Polysemy.hs:interpretConsole](Snip)

Finally, to tie everything together, we need a way of also running our
book-keeping `Free` type. This is given by `foldFree`, which allows us to
transform a `Free f` into any monad `m`, so long as we have a natural
transformation[^nt] from `f` to `m`:

[^nt]: A natural transformation from `f` to `g` is any function with a rank-2
       type of the form `forall a. f a -> g a`.

[code/War/Polysemy.hs:foldFree](Snip)

Programming with free monads is a very lovely experience, which gives us an
indirection layer underneath our business logic. But, *this doesn't all come for
free!* The attentive reader will have noticed accidental quadratic complexity
here! What's going wrong?

You can think of `Free f` as an `f`-shaped tree, where `(>>=)` grafts subtrees
into each `Pure` node. The problem is that every time you bind a `Free` your
tree gets bigger, which is to say, that the leaves get *further away from the
root.* So every bind needs to traverse the structure to get to its leaves, and
then pushes them farther away so the subsequent bind needs to do even more work!

This is exacerbated by the fact that `(>>=)` is left-associative, meaning the
expression `a >>= b >>= c` is parsed as `(a >>= b) >>= c`. Meaning that each
`(>>=)` is associated in a way that requires doing as much work as possible!

Our lovely style of programming with free monads is a no-go if it is going to
degrade our runtime performance asymptotically. What can we do?

The trick is use `Codensity`! Recall the definition of `CodensityT`, and in
particular, its `Monad` instance:

[code/War/Polysemy.hs:CodensityT](Snip)

[code/War/Polysemy.hs:MonadCodensityT](Snip)

`Codensity`'s `Monad` instance does nothing but replace the lambda inside of the
`CodensityT` newtype wrapper. This is very clearly $O(1)$---an asymptotic
improvement over `Free`'s $O(n)$ bind! Thus we can improve any free monad by
building it as a `Codensity (Free f)` rather than `Free f` directly. This is
accomplished by a helper function:

[code/War/Polysemy.hs:liftCodensity](Snip)

and a corresponding change to `sayHello`:

[code/War/Polysemy.hs:sayHelloImproved](Snip){sayHelloImproved="sayHello"}

This is a powerful example of why isomorphisms can be useful. Even though `Free
Console a` is isomorphic to `CodensityT (Free Console) a`, the latter is
significantly more performant! Different representations of types have different
strengths and weaknesses---clarity of formulation and performance are two common
trade-offs---and thus being able to spot isomorphisms can be a super power.

Concluding the story, I manually fused the `Codensity` formulation with the
operational functor I needed, which gave the benefit of fast binds without all
the machinery. After all, the fastest data structure is the one that you never
built in the first place. The library was released as

