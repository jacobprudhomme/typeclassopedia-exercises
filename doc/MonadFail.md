# `MonadFail`

``` haskell
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (MonadFail(..))

-- Needed for compilation
main :: IO ()
main = return ()
```

-   Relatively unprincipled (few laws)
-   Offers a notion of failure without recovery (as in `MonadPlus`),
    possibly with a basic error-reporting mechanism
-   When `MonadFailDesugaring` extension is enabled, the `fail` method
    from this typeclass is used for pattern-match failure in
    `do`-notation, rather than that in `Monad`
    -   This is because there are many monads that do not really support
        a `fail` method

## Definition

``` haskell
class Monad m => MonadFail m where
  fail :: String -> m a
```

## Laws

<!-- $$
\begin{aligned}
\text{fail s >>= m} \equiv \text{fail s}, && \text{i.e. failure propagates through a chain of computations}
\end{aligned}
$$ -->

<div align="center">

<img style="background: white;" src="../svg/z4cQXLY1RD.svg">

</div>
