`Functor`
=========

> {-# LANGUAGE InstanceSigs #-}
> 
> import Prelude hiding (Functor(..))
> 
> -- Needed for compilation
> main :: IO ()
> main = return ()


Definition
----------

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b
> 
>   infixl 4 <$
>   (<$) :: a -> f b -> f a
>   (<$) = fmap . const

- Kind of type parameter `f` must be `* -> *`, so not a concrete type (i.e. `Maybe` or `[]`)
- `fmap` applies a function to value(s) in a container/context, without altering structure of it
- `(<$)` simply replaces value(s) in a container/context, instead of applying function to it/them


Instances
---------

Some basic instances for `Functor`:

> instance Functor [] where
>   fmap :: (a -> b) -> [a] -> [b]
>   fmap _ [] = []
>   fmap f (x:xs) = f x : fmap f xs
> 
> instance Functor Maybe where
>   fmap :: (a -> b) -> Maybe a -> Maybe b
>   fmap _ Nothing = Nothing
>   fmap f (Just x) = Just (f x)

Data types needed for some of the exercises:

> data Pair a = Pair a a
> 
> data ITree a
>   = Leaf (Int -> a)
>   | Node [ITree a]


**Exercises**

{*Question 1*}  
Implement `Functor` instances for `(Either e)` and `((->) e)`.

---

> instance Functor (Either e) where
>   fmap :: (a -> b) -> Either e a -> Either e b
>   fmap _ (Left e) = Left e
>   fmap f (Right x) = Right (f x)
> 
> instance Functor ((->) e) where
>   fmap :: (a -> b) -> (e -> a) -> (e -> b)
>   fmap f g = f . g

{*Question 2*}  
Implement `Functor` instances for `((,) e)` and `Pair`. Explain their similarities and differences.

---

> instance Functor ((,) e) where
>   fmap :: (a -> b) -> (e,a) -> (e,b)
>   fmap f (x,y) = (x, f y)
> 
> instance Functor Pair where
>   fmap :: (a -> b) -> Pair a -> Pair b
>   fmap f (Pair x y) = Pair (f x) (f y)

`((,) e)` and `Pair` are similar in that they are both type constructors that take 1 argument, and
they both represent pairs of elements. However, with the former, the argument doesn't have to be
the same type as the type variable `e`, whereas with the latter, both elements must be of the same
type. Additionally, since the type variable `e` is part of the `Functor` instance for `((,) e)`, it
cannot change type, so only the second element in the tuple can have the function applied to it.
With `Pair`, both elements must have the function applied to them, because we cannot have a
`Pair` with elements of two different types.

{*Question 3*}  
Implement a `Functor` instance for the type `ITree`.

---

> instance Functor ITree where
>   fmap :: (a -> b) -> ITree a -> ITree b
>   fmap f (Leaf g) = Leaf (f . g)
>   fmap f (Node xs) = Node $ map (fmap f) xs

{*Question 4*}  
Give an example of a type of kind `* -> *` which cannot be made an instance of `Functor` (without using `undefined`).

---

> newtype NotAFunctor a = NotAFunctor (a -> Int)

`NotAFunctor` cannot be a functor because given a function `f : (a -> b)`, it is not possible to take
a function `(a -> Int)` and compose it with `f` to get a function `(b -> Int)`, i.e. it is not possible to take a `NotAFunctor a` and produce a `NotAFunctor b`.

{*Question 5*}  
Is this statement true or false?  
"The composition of two functors is also a functor."  
If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

---

> newtype FunctorComposition f g a = FunctorComposition (f (g a))
> 
> instance (Functor f, Functor g) => Functor (FunctorComposition f g) where
>   fmap f (FunctorComposition x) = FunctorComposition $ fmap (fmap f) x

Since a functor can be thought of as a "container" for a type, then to
compose functors is to compose containers, i.e. to wrap something in a
container (say `g`), then wrap that in another container (say `f`). This would
look like `(f (g a))`, where a is the type variable for the type inside the
innermost container, just as it would be with a single functor. Intuitively, since it
would be possible to map over the outermost container without changing its
structure, and then do the same with the inner container, then it would also
be possible to map over the entire data without altering its structure. This
means that the composition of two functors is also a functor.


Laws
----

- A functor must satisfy the following laws:

<!-- $$
\begin{aligned}
\text{fmap id} &= \text{id}\\
\text{fmap (f . g)} &= \text{(fmap f) . (fmap g)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/2fH9oF2kzB.svg"></div>

- A `Functor` instance that does not satisfy the functor laws:

> data List a = Empty | Cons a (List a)  -- A copy of []
> 
> instance Functor List where
>   fmap :: (a -> b) -> List a -> List b
>   fmap _ Empty = Empty
>   fmap f (Cons x xs) = Cons (f x) $ Cons (f x) $ fmap f xs


**Exercises**

{*Question 1*}  
Although it is not possible for a `Functor` instance to satisfy the first functor law but not the second (excluding `undefined`), the reverse is possible.
Give an example of a (bogus) `Functor` instance which satisfies the second law but not the first.

---

> data Optional a = None | Some a  -- A copy of Maybe
> 
> instance Functor Optional where
>   fmap :: (a -> b) -> Optional a -> Optional b
>   fmap _ _ = None

This `Functor` instance does not satisfy the first functor law, as `fmap id (Just x) == Nothing != Just x == id (Just x)`, but it satisfies the second functor law because

<!-- $$
\begin{aligned}
\text{(fmap f $\circ$ fmap g) x}
&\equiv \text{fmap f Nothing}\\
&\equiv \text{Nothing}\\
&\equiv \text{fmap (f . g) x}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/lAfZh4whUb.svg"></div>

{*Question 2*}  
Which laws are violated by the evil `Functor` instance for list shown above: both laws, or the first law alone?
Give specific counterexamples.

---

The first law is violated; take for example a list `[1]`. This implementation of fmap produces `fmap id [1] == id 1 : id 1 : [] == [1,1]`, so the original value to be mapped over is not preserved when mapping the identity function onto it. The second law is also violated; take as a counterexample `[1]`, with functions `f = (+1)` and `g = (*2)`. Then

<!-- $$
\begin{aligned}
\text{(fmap f . fmap g) [1]}
&\equiv \text{(fmap (+1) . fmap (*2)) [1]}\\
&\equiv \text{fmap (+1) [2, 2]}\\
&\equiv \text{[3, 3, 3, 3]}\\
&\not\equiv \text{[3, 3]}\\
&\equiv \text{fmap ((+1) . (*2)) [1]}\\
&\equiv \text{fmap (f . g) [1]}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/qDh19O3pXa.svg"></div>


Intuition
---------

We can think of `fmap` as:
- Taking a function and a container, it applies the function to the value(s) inside the container; we can also replace "container" with "context"
- Looking at its type signature differently, we can see `fmap :: (a -> b) -> (f a -> f b)`, i.e. `fmap` "lifts" a normal function to operate over the container or context `f`


Utility Functions
-----------------

- `(<$>)` is infix synonym of `fmap` (analogous to `($)`)
- `($>) = flip (<$)`
  - Mnemonic: points to the value that will be kept
- `void x = () <$ x`
  - Useful for computing something but ignoring the value
