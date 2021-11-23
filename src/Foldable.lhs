`Foldable`
==========

> {-# LANGUAGE InstanceSigs #-}
>
> import Prelude hiding (Foldable(..))
> import Data.Monoid (All(..), Any(..), Endo(..), First(..), Sum(..), Product(..))
>
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Abstracts over containers that can be "folded" akin to lists


Definition
----------

> class Foldable t where
>   fold    :: Monoid m => t m -> m
>   foldMap :: Monoid m => (a -> m) -> t a -> m
>   foldr   :: (a -> b -> b) -> b -> t a -> b
>   foldr'  :: (a -> b -> b) -> b -> t a -> b
>   foldl   :: (b -> a -> b) -> b -> t a -> b
>   foldl'  :: (b -> a -> b) -> b -> t a -> b
>   foldr1  :: (a -> a -> a) -> t a -> a
>   foldl1  :: (a -> a -> a) -> t a -> a
>   toList  :: t a -> [a]
>   null    :: t a -> Bool
>   length  :: t a -> Int
>   elem    :: Eq a => a -> t a -> Bool
>   maximum :: Ord a => t a -> a
>   minimum :: Ord a => t a -> a
>   sum     :: Num a => t a -> a
>   product :: Num a => t a -> a

- Only need to implement either `foldr` or `foldMap`, all others have default implementations based on these


Instances
---------

- Explaining the type of `foldMap :: Monoid m => (a -> m) -> t a -> m`
  - Given a function to convert the individual contents of a container into a `Monoid` (`a -> m`), and the container itself (`t a`), `foldMap` interates over the container, converting all the data to said `Monoid`, then combines them all with `mappend`

- Example for `[]`

> instance Foldable [] where
>   foldMap :: Monoid m => (a -> m) -> [a] -> m
>   foldMap f = mconcat . map f

- Example for `Tree` from documentation

> data Tree a
>   = Empty
>   | Leaf a
>   | Node (Tree a) a (Tree a)
>
> instance Foldable Tree where
>   foldMap :: Monoid m => (a -> m) -> Tree a -> m
>   foldMap _ Empty        = mempty
>   foldMap f (Leaf x)     = f x
>   foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

- Also provides instances for `Maybe` and `Array`
- Many data structures in `containers` library have `Foldable` instance


**Exercises**

{*Question 1*}
Implement `fold` in terms of `foldMap`.

---

> fold' :: (Foldable t, Monoid m) => t m -> m
> fold' = foldMap id

{*Question 2*}
What would you need in order to implement `foldMap` in terms of `fold`?

---

We would need that `t m` be a container that we can map over (a `Functor`), so we can map the `a -> m` function passed to `foldMap` over it, before calling `fold` on the result.

{*Question 3*}
Implement `foldMap` in terms of `foldr`.

---

> foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
> foldMap' f = foldr (mappend . f) mempty

{*Question 4*}
Implement `foldr` in terms of `foldMap` (hint: use the `Endo` monoid).

---

> foldr'' :: Foldable t => (a -> b -> b) -> b -> t a -> b
> foldr'' f init xs = appEndo (foldMap (Endo . f) xs) init

`f :: (a -> b -> b)`, so if we pass it directly as the argument to `foldMap`, it will be applied to each element of type `a`, giving us a value of `b -> b`. This is an endomorphism, and so can be made a monoid via the `Endo` type. `foldMap` will then compose these endofunctors sequentially (it doesn't matter whether it does this associating from the left or the right, because it is a monoid). In the end, we get one big endofunctor waiting for input of type `b` (the initial value), which then passes through the chain of composition, being appended to using the logic of `f` at each step, until we get the final result of our fold.

{*Question 5*}
What is the type of `foldMap . foldMap`? Or `foldMap . foldMap . foldMap`, etc.? What do they do?

---

`foldMap :: Monoid m => (a -> m) -> t a -> m == (a -> m) -> (t a -> m)` and `(.) :: (b -> c) -> (a -> b) -> (a -> c)`, so `foldMap . foldMap :: Monoid m => (a -> m) -> (t2 (t1 a) -> m) == (a -> m) -> t2 (t1 a) -> m`. Similarly, `foldMap . foldMap . foldMap == foldMap . (foldMap . foldMap) :: Monoid m => (a -> m) -> (t3 (t2 (t1 a)) -> m) == (a -> m) -> t3 (t2 (t1 a)) -> m`. They fold nested structures into a final monoidal value. The amount of `foldMaps` there are corresponds to the depth of the structure.


Derived Folds
-------------

- Because of `Foldable`, we can write generic container-agnostic functions like:

> containerSize :: Foldable t => t a -> Int
> containerSize = getSum . foldMap (const (Sum 1))
>
> filterF :: Foldable t => (a -> Bool) -> t a -> [a]
> filterF pred = foldMap (\x -> if pred x then [x] else [])
>
> stringsWithA :: Foldable t => t String -> [String]
> stringsWithA = filterF (elem 'a')

- Module also exports many predefined folds
- These were `Foldable` generalizations of `Prelude` functions that only worked on lists, but now the `Prelude` ones are re-exported from `Data.Foldable`
- Ex.: `concat`, `concatMap`, `and`, `or`, `any`, `all`, `sum`, `product`, `maximum`(`By`), `minimum`(`By`), `elem`, `notElem`, `find`
- Another ex.: `length` used to have type `[a] -> Int`, now has type `Foldable t => t a -> Int` and is implemented exactly like `containerSize`
- `toList` is another important function, that folds any `Foldable` into a list of its elements from left-to-right

- Also have generic functions that work with `Applicatives` or `Monads` to collect the computations from every element in a container and execute them, while disregarding their return values: `traverse_`, `sequenceA_`, etc.
  - We must disregard the results; `Foldable` is not general enough, since we must make a monoid of the results and append them
    - We cannot do this in general for `Applicatives` or `Monads`, but we can for the `()` instance of them
  - If we do have an `Alternative` or `MonadPlus` (`Applicative` or `Monad` with a `Monoid` structure), we can use `asum` or `msum` to combine the results as well

- Notice we always drop the `Foldable` structure from the final output (i.e. for a `Foldable t`, `t a` will never appear in the output)
- For traversing a generic container while retaining its structure, we want `Traversable`


**Exercises**

{*Question 1*}
Implement `toList :: Foldable f => f a -> [a]` in terms of either `foldr` or `foldMap`.

---

> toList' :: Foldable t => t a -> [a]
> toList' = foldr (:) []
>
> toList'' :: Foldable t => t a -> [a]
> toList'' = foldMap (\x -> [x])

{*Question 2*}
Show how one could implement the generic version of `foldr` in terms of `toList`, assuming we had only the list-specific `foldr :: (a -> b -> b) -> b -> [a] -> b`.

---

> foldr''' :: Foldable t => (a -> b -> b) -> b -> t a -> b
> foldr''' f init = foldr f init . toList

{*Question 3*}
Pick some of the following functions to implement: `concat`, `concatMap`, `and`, `or`, `any`, `all`, `sum`, `product`, `maximum`(`By`), `minimum`(`By`), `elem`, `notElem`, and `find`.  
Figure out how they generalize to `Foldable` and come up with elegant implementations using `fold` or `foldMap` along with appropriate `Monoid` instances.

---

> concat :: Foldable t => t [a] -> [a]
> concat = fold
>
> concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
> concatMap = foldMap
>
> and :: Foldable t => t Bool -> Bool
> and = getAll . foldMap All
>
> or :: Foldable t => t Bool -> Bool
> or = getAny . foldMap Any
>
> any :: Foldable t => (a -> Bool) -> t a -> Bool
> any f = getAny . foldMap (Any . f)
>
> all :: Foldable t => (a -> Bool) -> t a -> Bool
> all f = getAll . foldMap (All . f)
>
> sum' :: (Foldable t, Num a) => t a -> a
> sum' = getSum . foldMap Sum
>
> product' :: (Foldable t, Num a) => t a -> a
> product' = getProduct . foldMap Product
>
> newtype Max a = Max { getMax :: a }
> newtype Min a = Min { getMin :: a }
>
> instance Ord a => Semigroup (Max a) where
>   (<>) :: Max a -> Max a -> Max a
>   (Max a) <> (Max b) = if a >= b then Max a else Max b
> instance Ord a => Semigroup (Min a) where
>   (<>) :: Min a -> Min a -> Min a
>   (Min a) <> (Min b) = if a <= b then Min a else Min b
>
> instance (Bounded a, Ord a) => Monoid (Max a) where
>   mempty :: Max a
>   mempty = Max minBound
> instance (Bounded a, Ord a) => Monoid (Min a) where
>   mempty :: Min a
>   mempty = Min maxBound
>
> maximum' :: (Foldable t, Bounded a, Ord a) => t a -> a
> maximum' = getMax . foldMap Max
>
> minimum' :: (Foldable t, Bounded a, Ord a) => t a -> a
> minimum' = getMin . foldMap Min
>
> maximumBy :: (Foldable t, Monoid a) => (a -> a -> Ordering) -> t a -> a
> maximumBy f cont = foldMap getMax (zip xs (drop 1 (cycle xs)))
>   where
>     xs = toList cont
>     getMax pair =
>       if uncurry f pair == GT
>       then fst pair
>       else snd pair
>
> minimumBy :: (Foldable t, Monoid a) => (a -> a -> Ordering) -> t a -> a
> minimumBy f cont = foldMap getMin (zip xs (drop 1 (cycle xs)))
>   where
>     xs = toList cont
>     getMin pair =
>       if uncurry f pair == LT
>       then fst pair
>       else snd pair
>
> elem' :: (Foldable t, Eq a) => a -> t a -> Bool
> elem' x = getAny . foldMap (Any . (== x))
>
> notElem' :: (Foldable t, Eq a) => a -> t a -> Bool
> notElem' x = getAll . foldMap (All . (/= x))
>
> find' :: Foldable t => (a -> Bool) -> t a -> Maybe a
> find' pred = getFirst . foldMap (First . justIf)
>   where justIf x = if pred x then Just x else Nothing


Utility Functions
-----------------

- `asum` takes a container of computations and combines them using `(<|>)`
- `sequenceA_` takes a container of computations and runs them in sequence, discarding the result
- `traverse_` applies the given function to each element in a container and sequences the effects of the computation, discarding the result
- `for_` is `flip traverse_`, to match the structure of a `foreach`-loop
- For historical reasons, there are also overly restrictive `Monad` versions of the above (`msum`, `sequence_`, `mapM_`, `forM_`)


**Exercises**

{*Question 1*}
Implement `traverse_` in terms of `sequenceA_` and vice versa.  
One of these will need an extra constraint. What is it?

> traverse_' :: (Foldable t, Functor t, Applicative f) => (a -> f b) -> t a -> f ()
> traverse_' f = sequenceA_' . fmap f
>
> sequenceA_' :: (Foldable t, Functor t, Applicative f) => t (f a) -> f ()
> sequenceA_' = traverse_' id

`traverse_'` needs a `Functor` constraint on `t`, because it needs to be able to map the function over every element in `t`. In this case, `sequenceA_'` also needs a `Functor` constraint, but only because we are using `traverse_'` in it.


What It Isn't
-------------

- Term "fold" often used to refer to catamorphism
  - Intuition: given a way to summarize one level of a structure, a catamorphism can summarize an entire recursive structure
- `Foldable` is weaker than a catamorphism
  - Only allows working with left-to-right traversal of elements in a structure, not the structure itself
    - i.e. every use of `Foldable` can be simplified using `toList`, treating the `Foldable` as a simple list
- Good for many tasks, but not for all
  - Ex.: computing height of a tree. But, can be done with a catamorphism!
