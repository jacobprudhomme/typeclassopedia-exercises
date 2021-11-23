# `Semigroup`

``` haskell
import Prelude hiding ((<>))
import Data.List.NonEmpty (NonEmpty(..))

-- Needed for compilation
main :: IO ()
main = return ()
```

-   Set `S`, with binary operation `⊕` which associatively combines
    elements of `S`
-   Examples:
    -   Any numbers under addition or multiplication
    -   Any numbers under max or min
    -   Booleans under conjuction or disjunction
    -   Lists or strings under concatenation
    -   Functions from a set to itself under composition
    -   So many more!

## Definition

``` haskell
class Semigroup a where
  infixr 6 <>
  (<>) :: a -> a -> a

  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as
    where
      go b (c:cs) = b <> go c cs
      go b []     = b

  stimes :: Integral b => b -> a -> a
  stimes y0 x0
    | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y    = f (x <> x) (y `quot` 2)
        | y == 1    = x
        | otherwise = g (x <> x) (pred y `quot` 2) x
      g x y z
        | even y    = g (x <> x) (y `quot` 2) z
        | y == 1    = x <> z
        | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)
```

-   `(<>)` is the binary operation
-   The other methods have default implementations that use `(<>)`
    -   `sconcat` folds a non-empty list with `(<>)`
        -   Mostly like `foldr1 (<>)` except for idempotent semigroups
            where it is constant-time
    -   `stimes n` is equivalent to `sconcat . replicate n`, but uses
        exponentiation by squaring
        -   Can be a great `O(logn)` optimization for some semigroups
        -   For some semigroups, like lists, it is terrible and is
            better being overridden

## Laws

-   We only need that `(<>)` have (associativity):

<!-- $$
\begin{aligned}
\text{(x <> y) <> z} = \text{x <> (y <> z)}
\end{aligned}
$$ -->

<div align="center">

<img style="background: white;" src="../svg/A6SeAMdXh2.svg">

</div>
