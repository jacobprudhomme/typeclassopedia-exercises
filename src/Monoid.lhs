`Monoid`
========

> {-# LANGUAGE InstanceSigs #-}
> 
> import Prelude hiding (Applicative(..), Monoid(..))
> 
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Like a semigroup, but also has an element `e` of `S` that turns the binary operation into an identity operation, i.e. `e ⊕ x == x ⊕ e == x`


Definition
----------

> class Semigroup a => Monoid a where
>   mempty :: a
>   mappend :: a -> a -> a
> 
>   mconcat :: [a] -> a
>   mconcat = foldr mappend mempty

- `mempty` is the identity element
- `mappend` is `(<>)`
- `mconcat` has a default definition. It is a fold that combines a list of elements with the identity element at the end
- These methods have unfortunate naming after the list instance of a `Monoid`, which may not accurately reflect what is going on, since many `Monoid`s don't "append"
  - `mempty = []` (empty list)
  - `mappend = (++)` (append)


Laws
----

- A monoid must satisfy 3 laws:

<!-- $$
\begin{aligned}
\text{mempty `mappend` x} &= \text{x} && \text{(left identity)}\\
\text{x `mappend` mempty} &= \text{x} && \text{(right identity)}\\
\text{(x `mappend` y) `mappend` z} &= \text{x `mappend` (y `mappend` z)} && \text{(associativity)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/EHnn34hXHW.svg"></div>

- This last one is covered by the requirement of also being a semigroup


Instances
---------

- Lists:

> instance Monoid [a] where
>   mempty :: [a]
>   mempty = []
> 
>   mappend :: [a] -> [a] -> [a]
>   mappend = (++)

- Since any numeric type can be made a monoid under addition _and_ multiplication, but we can't have 2 instances for the same type, we use `newtype` wrappers:

> newtype Sum a = Sum { getSum :: a }
> newtype Product a = Product { getProduct :: a }
> 
> instance Num a => Semigroup (Sum a) where
>   (<>) :: Sum a -> Sum a -> Sum a
>   Sum a <> Sum b = Sum (a + b)
> 
> instance Num a => Semigroup (Product a) where
>   (<>) :: Product a -> Product a -> Product a
>   Product a <> Product b = Product (a * b)
> 
> instance Num a => Monoid (Sum a) where
>   mempty :: Sum a
>   mempty = Sum 0
> 
>   mappend :: Sum a -> Sum a -> Sum a
>   mappend = (<>)
> 
> instance Num a => Monoid (Product a) where
>   mempty :: Product a
>   mempty = Product 1
> 
>   mappend :: Product a -> Product a -> Product a
>   mappend = (<>)

  - Allows us to do `getSum (mconcat . map Sum $ [1..5])` or `getProduct (mconcat . map Product $ [1..5])`, but we could also just do `sum [1..5]` or `product [1..5]`, which are much easier

- Similarly, `Any` and `All` are `newtype` wrappers for booleans under disjunction and conjunction, respectively:

> newtype Any = Any { getAny :: Bool }
> newtype All = All { getAll :: Bool }
> 
> instance Semigroup Any where
>   (<>) :: Any -> Any -> Any
>   Any a <> Any b = Any (a || b)
> 
> instance Semigroup All where
>   (<>) :: All -> All -> All
>   All a <> All b = All (a && b)
> 
> instance Monoid Any where
>   mempty :: Any
>   mempty = Any False
> 
>   mappend :: Any -> Any -> Any
>   mappend = (<>)
> 
> instance Monoid All where
>   mempty :: All
>   mempty = All True
> 
>   mappend :: All -> All -> All
>   mappend = (<>)

- There are 3 instances for `Maybe`
  - If the underlying type parameter is a `Semigroup`, the entire thing can logically be made a `Monoid`:

> instance Semigroup a => Monoid (Maybe a) where
>   mempty :: Maybe a
>   mempty = Nothing
> 
>   mappend :: Maybe a -> Maybe a -> Maybe a
>   mappend Nothing b = b
>   mappend a Nothing = a
>   mappend (Just a) (Just b) = Just (a <> b)

  - `First` and `Last` are newtype wrappers where `mappend` only returns the first or last non-`Nothing` item, respectively:

> newtype First a = First { getFirst :: Maybe a }
> newtype Last a = Last { getLast :: Maybe a }
> 
> instance Semigroup (First a) where
>   (<>) :: First a -> First a -> First a
>   First Nothing <> b = b
>   a <> _             = a
> 
> instance Semigroup (Last a) where
>   (<>) :: Last a -> Last a -> Last a
>   a <> Last Nothing = a
>   _ <> b            = b
> 
> instance Monoid (First a) where
>   mempty :: First a
>   mempty = First Nothing
> 
>   mappend :: First a -> First a -> First a
>   mappend = (<>)
> 
> instance Monoid (Last a) where
>   mempty :: Last a
>   mempty = Last Nothing
> 
>   mappend :: Last a -> Last a -> Last a
>   mappend = (<>)

- `Endo a` is a `newtype` wrapper for functions `a -> a` (endomorphisms)

> newtype Endo a = Endo { getEndo :: a -> a }
> 
> instance Semigroup (Endo a) where
>   (<>) :: Endo a -> Endo a -> Endo a
>   Endo f <> Endo g = Endo (f . g)
> 
> instance Monoid (Endo a) where
>   mempty :: Endo a
>   mempty = Endo id
> 
>   mappend :: Endo a -> Endo a -> Endo a
>   mappend = (<>)

- Many ways to "lift" `Monoid`s to have additional structure, like with `Maybe`:
  - For example, tuples:

> instance (Monoid a, Monoid b) => Monoid (a,b) where
>   mempty :: (a,b)
>   mempty = (mempty, mempty)
> 
>   mappend :: (a,b) -> (a,b) -> (a,b)
>   mappend (a,b) (c,d) = (a <> c, b <> d)

- `Ordering` is a monoid in such a way that `mconcat (zipWith compare xs ys)` computes lexicographic ordering of `xs` and `ys`

> instance Monoid Ordering where
>   mempty :: Ordering
>   mempty = EQ
> 
>   mappend :: Ordering -> Ordering -> Ordering
>   mappend LT _ = LT
>   mappend EQ x = x
>   mappend GT _ = GT

- There are instances for many of the data structures in the `containers` library
  - For example, `Map`, `Set` and `Sequence`


- `Monoid`s are also used to construct many other type class instances
  - For example:

> class Functor f => Applicative f where
>   pure :: a -> f a
> 
>   infixl 4 <*>, *>, <*
>   (<*>) :: f (a -> b) -> f a -> f b
> 
>   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
>   liftA2 f x = (<*>) (fmap f x)
> 
>   (*>) :: f a -> f b -> f b
>   a1 *> a2 = (id <$ a1) <*> a2
> 
>   (<*) :: f a -> f b -> f a
>   (<*) = liftA2 const
> 
> instance Monoid e => Applicative ((,) e) where
>   pure :: a -> (e,a)
>   pure x = (mempty, x)
> 
>   (<*>) :: (e, a -> b) -> (e,a) -> (e,b)
>   (u,f) <*> (v,x) = (u <> v, f x)

  - This can also be done to make `((,) e)` an instance of `Monad` (the `Writer` monad)

- `Monoid` is important to the definition of the `Foldable` typeclass
