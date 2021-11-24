# `Comonad`

``` haskell
{-# LANGUAGE InstanceSigs #-}

-- Needed for compilation
main :: IO ()
main = return ()
```

-   Categorical dual of monad
    -   That is, like `Monad` but with all the function arrows flipped
-   Not in standard libraries, but has seen some interesting uses

## Definition

-   Defined in `Control.Comonad`, provided by `comonad` library:

``` haskell
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
```

-   `extract` is dual of `return`

-   `duplicate` is dual of `join`

-   `extend` is dual of `(=<<)`

-   Must implement `extract`, but can decide to implement either
    `duplicate` or `extend`

-   Prototypical example of comonad:

``` haskell
data Stream a = Cons a (Stream a)  -- Infinite lazy stream

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Comonad Stream where
  extract :: Stream a -> a
  extract (Cons x _) = x

  duplicate :: Stream a -> Stream (Stream a)
  duplicate s@(Cons _ xs) = Cons s (duplicate xs)

  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f s@(Cons _ xs) = Cons (f s) (extend f xs)  -- fmap f (duplicate s)
```

-   `duplicate` is like the list function `tails`, which creates a list
    of all sequential tails of a list
-   `extend` computes a new `Stream` from an old one, where the element
    at position `n` is computed as a function of everything from
    position `n` onwards in the old `Stream`

## More Information

-   Some more interesting applications or ideas involving comonads:
    -   Relations to cellular automata
    -   Comonadic formulation of FRP
    -   Relationship of comonads and zippers, using them to create a
        menu system for a website
    -   Recursive coalgebras and recursion schemes
    -   Similarities between comonads and objects in OOP
