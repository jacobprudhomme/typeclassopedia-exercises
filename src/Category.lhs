`Category`
==========

> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE TypeOperators #-}
>
> import Control.Monad ((>=>))
>
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Generalizes function composition to general "morphisms"


Definition
----------

- Will show a definition that uses type operators for clarity first
- `arr` is analogous to `(->)`
- Prior to GHC 7.6.1, `arr` was `(~>)`

> class Category' arr where
>   id'  :: a `arr` a
>   (.>) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)

- Way it is actually defined in base

> class Category cat where
>   id  :: cat a a
>   (.) :: cat b c -> cat a b -> cat a c

- Instance of `Category` should have kind `* -> * -> *`
- Instructive to imagine type variable `cat` replaced by type constructor `(->)`
  - In this case, instance of `id` and `(.)` correspond to those on functions, as defined in `Prelude`


Instances
---------

- As above, a `(->)` instance of `Category` is provided:

> instance Category (->) where
>   id :: a -> a
>   id = Prelude.id
>
>   (.) :: (b -> c) -> (a -> b) -> (a -> c)
>   (.) = (Prelude..)

- Another instance mentioned in the monad laws: `Kleisli m a b` (`newtype` wrapper for `a -> m b`)
- Like function composition for functions `a -> m b`

> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
>
> instance Monad m => Category (Kleisli m) where
>   id :: Kleisli m a a
>   id = Kleisli return
>
>   (.) :: Kleisli m b c -> Kleisli m a b -> Kleisli m a c
>   Kleisli g . Kleisli h = Kleisli (h >=> g)


Laws
----

<!-- $$
\begin{aligned}
\text{id . f} &= \text{f} && \text{(left identity)}\\
\text{f . id} &= \text{f} && \text{(right identity)}\\
\text{f . (g . h)} &= \text{(f . g) . h} && \text{(associativity)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/GKkU4fFFZy.svg"></div>

- Similar to `Monoid` (but for `(.)`), the types have to line up


Utility Functions
-----------------

- `(<<<)` is a synonym for `(.)`
- `(>>>)` is `(.)` with arguments reversed
- Both used to be defined as part of `Arrow` typeclass
