`Bifunctor`
===========

> {-# LANGUAGE InstanceSigs #-}
>
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Recall: a functor is a type of kind `* -> *` where we can "map" over the type parameter, while preserving the structure of the type
- `Functor` has instances for `((,) e)` and `Either e`, but why should we not be able to map over both type parameters?
- This led to bifunctors, for types of kind `* -> * -> *` where we can "map" over both type parameters


Definition
----------

> class Bifunctor p where
>   bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
>
>   first  :: (a -> b) -> p a c -> p b c
>   second :: (c -> d) -> p a c -> p a d

- `bimap` allows mapping over both type arguments at the same time
  - e.g. `bimap (+1) length (4, [1,2,3]) = (5, 3)`
- `first` and `second` are for mapping over a single type argument at a time

- Must define either `bimap`, or both of `first` and `second`
  - Default implementations are provided in terms of each other like so:

> {-
> bimap f g = first f . second g
>
> first  f = bimap f id
> second g = bimap id g
> -}


Laws
----

- The laws for bifunctors are analogous to those for functors
  - (identity)
    - `bimap id id = id`
    - `first id    = id`
    - `second   id = id`
  - (composition)
    - `bimap (f . g) (h . i) = bimap f h . bimap g i`
    - `first (f . g) = first f . first g`
    - `second (f . g) = second f . second g`
    - These actually come for free by parametricity, by the identity laws
    - Default implementation of `bimap` satisfies this <=> `first` and `second` satisfy this

- Additional law that relates `bimap`, `first` and `second`
  - `bimap f g = first f . second g`
  - This should always hold while defining just `bimap` or just `first` and `second`
  - Symmetric version `bimap f g = second g . first f` follows by parametricity

- In summary, many of these laws follow from default implementation or parametricity
  - If implementing only `bimap`, need only worry about `bimap id id = id`
  - If implementing `first` and `second`, need only worry about `first id = id` and `second id = id`


Instances
---------

- `(,)` and `Either` are instances in an obvious way:

> instance Bifunctor (,) where
>   bimap :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
>   bimap f g (x,y) = (f x, g y)
>
> instance Bifunctor Either where
>   bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
>   bimap f _ (Left e)  = Left (f e)
>   bimap _ g (Right x) = Right (g x)

- Some larger tuple constructors like `(,,)` are also instances; this one maps over the last 2 components, leaving the first one be. Not very useful!

- A value of type `Const a b` consists of just a value of type `a`. `bimap f g` maps `f` over the value of type `a` and ignores `g`
