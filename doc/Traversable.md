# `Traversable`

``` haskell
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Traversable(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))

-- Needed for compilation
main :: IO ()
main = return ()
```

## Definition

``` haskell
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
  sequence  ::       Monad m => t (m a) -> m (t a)
```

-   Only need to implement either `traverse` or `sequenceA`, other
    methods have default implementation in terms of these
-   `mapM` and `sequence` only exist for historical reasons
    -   Now that `Applicative` is a superclass of `Monad`, just copies
        of the former 2 methods, but with more restrictive types

## Intuition

-   Key method of this typeclass is `traverse`
    -   Type leads us to understand that it is an effectful `fmap`: maps
        over structure `t a`, preserving it, but may have some effects
        along the way, captured by `f`
    -   So `Traversable` is a generalization of `Functor`
-   Look at type of `sequenceA`
    -   Answers question: can we commute two functors (e.g. can we turn
        a tree of lists into a list of trees)?
    -   Ability to compose 2 monads depends on this ability to commute
        functors
    -   If we want to compose `Monad` `M a = m (n a)` (`m` and `n` also
        `Monad`s) and implement
        `join :: M (M a) -> M a == m (n (m (n a))) -> m (n a)`, we have
        to be able to swap the inner `m` and `n` by commuting them. Then
        we use `join`s for `m` and `n` individually
-   `traverse` and `sequenceA` are equivalent in power and thus can be
    implemented in terms of the other

**Exercises**

{*Question 1*} There are at least two natural ways to turn a tree of
lists into a list of trees. What are they, and why?

------------------------------------------------------------------------

If we consider the tree of lists to be a non-deterministic tree, that is
a tree with a non-deterministic choice of values at each node, then we
can turn this into a list of trees by considering a list containing each
individual combination of nodes in the tree, i.e. a non-deterministic
collection of “determined” trees that represents all the possible trees
we can have. Another thing we can do is make a simple tree `Leaf x`, for
every single value `x` in every list at every node of the tree of lists.
Thus, we would get a list of simple one-node trees.

{*Question 2*} Give a natural way to turn a list of trees into a tree of
lists.

------------------------------------------------------------------------

Much like the first example above, we can consider a list of trees to be
a non-deterministic collection of possible trees, and we can achieve the
same effect with a tree of lists, where the list of values at each node
is a non-deterministic representation of the possible values that can be
taken on at said node.

{*Question 3*} What is the type of `traverse . traverse`? What does it
do?

------------------------------------------------------------------------

`traverse :: Applicative f => (a -> f b) -> t a -> f (t b) == (a -> f b) -> (t a -> f (t b))`
and `(.) :: (b -> c) -> (a -> b) -> (a -> c)`, so
`traverse . traverse :: Applicative f => (a -> f b) -> (t2 (t1 a) -> f (t2 (t1 b))) == (a -> f b) -> t2 (t1 a) -> f (t2 (t1 b))`.
`traverse . traverse` maps over a twice-nested structure, and may carry
some effects along with it.

{- ***Question 4*** -} Implement `traverse` in terms of `sequenceA`, and
vice versa.

------------------------------------------------------------------------

``` haskell
traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA' . fmap f

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = traverse' id
```

## Instances

-   Example for `Tree` type from `Foldable`:

``` haskell
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l)
                             (f x)
                             (fmap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l
                                 <*> f x
                                 <*> traverse f r
```

-   Compare `Functor` and `Traversable` instances

    -   Structurally identical! Only `Functor` involves normal function
        application, and `Traversable` does it in an `Applicative`
        context

-   We can implement the methods of both `Functor` and `Foldable`
    instances using just the methods from `Traversable` instance

-   Many `Traversable` instances are provided as standard

    -   `[]`
    -   `ZipList`
    -   `Maybe`
    -   `((,) e)`
    -   `Sum`
    -   `Product`
    -   `Either e`
    -   `Map`
    -   `Tree`
    -   `Sequence`

-   Note that `Set` isn’t a `Traversable`, but it is a `Foldable`

**Exercises**

{*Question 1*} Implement `fmap` and `foldMap` using only the
`Traversable` methods. (Note that the `Traversable` module provides
these implementations as `fmapDefault` and `foldMapDefault`.)

------------------------------------------------------------------------

``` haskell
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f = runIdentity . traverse (Identity . f)

foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f = getConst . traverse (Const . f)
```

{*Question 2*} Implement `Traversable` instances for `[]`, `Maybe`,
`((,) e)`, and `Either e`.

------------------------------------------------------------------------

``` haskell
instance Traversable [] where
  traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse _ []     = pure []
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x

instance Traversable ((,) e) where
  traverse :: Applicative f => (a -> f b) -> (e,a) -> f (e,b)
  traverse f (e,x) = (,) e <$> f x

instance Traversable (Either e) where
  traverse :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
  traverse _ (Left e)  = pure $ Left e
  traverse f (Right x) = Right <$> f x
```

{*Question 3*} Explain why `Set` is `Foldable` but not `Traversable`.

------------------------------------------------------------------------

`Set` cannot be made a `Functor` that satisfies the laws. The `map`
function provided for `Set` has type signature
`Ord b => (a -> b) -> Set a -> Set b` in order to preserve the structure
of a `Set`, however we cannot enforce this `Ord a` constraint when
writing an implementation for `fmap`. Because `Set` cannot be made a
`Functor`, it cannot be made a `Traversable` either. However, since with
`Foldables`, we do not care about preserving the structure of the
container, we have no such constraint.

{*Question 4*} Show that `Traversable` functors compose: that is,
implement an instance for `Traversable (Compose f g)` given
`Traversable` instances for `f` and `g`.

------------------------------------------------------------------------

``` haskell
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose x) = Compose <$> traverse (traverse f) x
```

## Laws

-   `Traversable`s have the following two laws, where `Identity` and
    `Compose` are the corresponding `Functor`s of the same name
    -   `traverse Identity = Identity` (identity)
        -   Says traversals cannot add arbitrary effects of their own
    -   `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`
        (composition)
        -   Explains how to collapse two traversals in sequence into a
            single one
-   Additionally, suppose `eta` is an `Applicative` morphism
    (i.e. `(Applicative f, Applicative g) => f a -> g a`) and it
    preserves `Applicative` operations (i.e. `eta (pure x) == pure x`
    and `eta (x <*> y) == eta x <*> eta y`)
    -   By parametricity and `Traversable` satisfying the above 2 laws,
        `eta` will also satisfy `eta . traverse f == traverse (eta . f)`
