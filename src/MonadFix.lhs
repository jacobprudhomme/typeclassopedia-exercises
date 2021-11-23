`MonadFix`
==========

> {-# LANGUAGE InstanceSigs #-}
>
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Typeclass for monads that support fixpoint operation `fix`, specialized to monads
  - Only monads lazy in their argument supported due to infinite recursive nature of `fix` (plenty of external literature about this)
- Allows output of monadic computations to be defined by recursion
- Used to be called `MonadRec`


`do rec`-Notation
-----------------

- Using `-XRecursiveDo` flag in GHC allows us to use `do rec`-notation
  - May have a `rec`-block nested in a `do`-block like so:

> {-
> do
>   x <- foo
>   rec
>     y <- bar
>     z <- baz
>     bob
>   w <- frob
> -}

  - Every variable defined in the `rec`-block is in scope for all other variables in the `rec`-block, much like `let`-blocks

- Use case: think of defining a gate with a feedback loop in a circuit
  - Would need output of gate to be one of inputs (for example: `x <- gate x y`)
  - Impossible to do with regular `do`
  - Recursive-`do` allows these sorts of recursive definitions


Intuition
---------

- Try implementing `maybeFix :: (a -> Maybe a) -> Maybe a` for `Maybe` monad
  - Implementation of non-monadic `fix`: `fix f = f (fix f)`... well, similar
  - Inspired by `fix`: `maybeFix f = maybeFix f >>= f`
  - Something seems wrong here... nothing specific to `Maybe`, this works for all monads, i.e. `maybeFix:: Monad m => (a -> m a) -> m a`! But as said above, not all monads can support `MonadFix`, so how can that be?
    - Has right type, but not right semantics
  - We can see `(>>=)` defined for `Maybe` pattern-matches on its first argument to see if it is `Nothing` or `Just val`, so this will recurse infinitely in an effort to determine the value of that first parameter that it needs
  - The trick: assume `maybeFix` will return `Just`, and go on that assumption:

> {-
> maybeFix :: (a -> Maybe a) -> Maybe a
> maybeFix f = ma
>   where ma = f (fromJust ma)
> -}

- Normally, `fromJust` is bad, but here it is justified
  - Three execution possibilities:
    1. If `f` returns `Nothing` without looking at its args, then `maybeFix f = Nothing`
    2. If `f` returns `Just x`, where `x` depends on its args, then the recursion proceeds
      - `fromJust ma = x`, so `f`'s output is just fed back to it as input
    3. If `f` uses its argument to decide whether to return `Just` or `Nothing`, then `maybeFix f` will not terminate; to get the value of its argument, it needs to keep recursing, which it will do infinitely
  - So, `maybeFix f` will never crash, it may just not terminate
    - Only way it could crash is if we try to run `fromJust ma`, knowing `ma == Nothing`, but this is impossible by the above

- Instances of `MonadFix`:
  - `List` (analogous to `Maybe`)
  - `ST`
  - `IO` (acts kind of like sending a value back in time to itself!):
    1. Creates empty `MVar`
    2. Reads its contents right away using `unsafeInterleaveIO` (lazily delays reading until value is needed)
    3. Uses contents of `MVar` to compute new value
    4. Writes result back into `MVar`


**Exercises**

{*Question 1*}
Implement a `MonadFix` instance for `[]`.

---

> class Monad m => MonadFix m where
>   mfix :: (a -> m a) -> m a
>
> instance MonadFix [] where
>   mfix :: (a -> [a]) -> [a]
>   mfix f = xs
>     where xs = f (head xs)

- Apparently this is not the real definition of the instance, but it seems to match the examples I have tried, and it mirrors the definition for `Maybe`


`mdo` Syntax
------------

- Example from `do rec`-notation section can be written as so with `mdo` syntax:

> {-
> mdo
>   x <- foo
>   y <- bar
>   z <- baz
>   bob
>   w <- frob
> -}

- `mdo` will analyze block to find minimal recursive blocks, whereas `do rec` will just desugar directly into a call to `mfix`


Laws
----

- From some external reading, I found `MonadFix` has 4 laws:

<!-- $$
\begin{aligned}
\text{mfix (return . h)} &= \text{return (fix h)} && \text{(purity)}\\
\text{mfix (\x -> a >>= \y -> f x y)} &= \text{a >>= \y -> mfix (\x -> f x y)} && \text{(left shrinking)}\\
\text{mfix (liftM h . f)} &= \text{liftM h (mfix (f . h))}, && \text{if h is strict} \quad \text{(sliding)}\\
\text{mfix (\x -> mfix (\y -> f x y))} &= \text{mfix (\x -> f x x)} && \text{(nesting)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/fYqqoh0Jcd.svg"></div>

- They are not covered in the Typeclassopedia, so I will leave it at that
