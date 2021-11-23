`Applicative`
=============

> {-# LANGUAGE InstanceSigs #-}
>
> import Prelude hiding (Applicative(..), (**))
>
> -- Needed for compilation
> main :: IO ()
> main = return ()


Definition
----------

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

- Every `Applicative` is a `Functor`
- Again, the kind of the type parameter `f` must be `* -> *`, so not a concrete type (i.e. `Maybe` or `[]`)
- `pure` embeds values in a default "effect-free" context, e.g. `pure 5 == [5]` or `Just 5`
- `(<*>)` ('ap' or 'splat') allows us to apply a function over a context, which is itself also in one
- `(*>)` sequences the effects of 2 computations, while dropping the result of the first
- `(<*)` does the same as above, but drops the result of the second


Laws
----

- An applicative functor must satisfy the following laws:

<!-- $$
\begin{aligned}
\text{pure id <*> v} &= \text{v} && \text{(identity)}\\
\text{pure f <*> pure x} &= \text{pure (f x)} && \text{(homomorphism)}\\
\text{u <*> pure y} &= \text{pure ($ y) <*> u} && \text{(interchange)}\\
\text{u <*> (v <*> w)} &= \text{pure (.) <*> u <*> v <*> w} && \text{(composition)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/sXMWDcR8d7.svg"></div>

  - Note for (homomorphism): Applying a non-effectful function f to a non-effectful argument x should be just like injecting f x into a pure context
  - Note for (interchange): When applying an effectful function to a pure argument, the order of evaluation doesn't matter
  - Note for (composition): Sort of like associativity

  - Last 3 rules can be used to rewrite any `Applicative` expression in canonical form, where `pure` is used only once at the very beginning of the expression and only left-nested occurrences of `(<*>)` appear thereafter. (homomorphism) collapses adjacent uses of `pure` into one, (interchange) moves `pure` to the left, and (composition) changes the association of `(<*>)`.

- Also, applicatives relate to functors by the following law:

<!-- $$
\begin{aligned}
\text{g <$> x} &= \text{pure g <*> x}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/m6hay89nlK.svg"></div>

  - i.e. mapping a pure function `g` over a context is the same as injecting said function in a context and `ap`-ing it.


**Exercises**

{*Question 1*}  
One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument.  
Using the above laws, prove that `pure f <*> x == pure (flip ($)) <*> x <*> pure f`.

---

By doing some manipulation of the RHS and using the applicative laws, we can obtain:

<!-- $$
\begin{aligned}
\text{pure (flip ($)) <*> x <*> pure f}
&\equiv \text{(pure (flip ($)) <*> x) <*> pure f}\\
&\equiv \text{pure ($ f) <*> (pure (flip ($)) <*> x)} && \text{(interchange)}\\
&\equiv \text{pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x} && \text{(composition)}
&\equiv \text{((pure (.) <*> pure ($ f)) <*> pure (flip ($))) <*> x}\\
&\equiv \text{(pure ((.) ($ f)) <*> pure (flip ($))) <*> x} && \text{(homomorphism)}\\
&\equiv \text{pure (((.) ($ f)) (flip ($))) <*> x} && \text{(homomorphism)}\\
&\equiv \text{pure ((.) ($ f) (flip ($))) <*> x}\\
&\equiv \text{pure (($ f) . (flip ($))) <*> x}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/QWbCewdaa3.svg"></div>

We can use some logical steps to deduce the type of the expression `($ f) . (flip ($))`. We know:

1. `f :: a -> b`
2. `x :: Applicative a`
3. `($) :: (c -> d) -> c -> d`
4. `(.) :: (f -> g) -> (e -> f) -> e -> g`
5. `(<*>) :: Applicative (h -> i) -> Applicative h -> Applicative i`.

Using these facts, we can find:

1. By (3), `flip ($) :: c -> (c -> d) -> d == c -> ((c -> d) -> d)` (6)
2. By (1) and (3), `($ f) :: ((a -> b) -> d) -> d` (7), where `c` became `(a -> b) -> d` to match accordingly
3. By (4) and (7), `(($ f) .) :: (e -> ((a -> b) -> d)) -> e -> d` (8), where `f` became `(a -> b) -> d` to match accordingly
4. By (6) and (8), `($ f) . (flip ($)) :: a -> b`, where `e` became `c` became `a` and `d` became `b` to match accordingly.

Note that `($ f) . (flip ($))` and `f` have the same type. Since the former uses nothing more than `f` itself and function application (albeit rearranged), we can infer that it is equivalent to `f`, as evidenced by the types. This means `f == ($ f) . (flip ($))`.

Thus, we have that:

<!-- $$
\begin{aligned}
\text{pure f <*> x}
&\equiv \text{pure (($ f) . (flip ($))) <*> x}\\
&\equiv \text{pure (flip ($)) <*> x <*> pure f}, && \text{as needed.}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/Stx6Cvljl7.svg"></div>


Instances
---------

- Most `Functor`s are also `Applicative`s

- `[]` can be made an instance of `Applicative` in two ways
  - When viewed as an ordered collection of elements, we treat `fs <*> xs` as pairwise function application in a context (`ZipList` is a newtype wrapper around `[]` for this purpose):

> newtype ZipList a = ZipList { getZipList :: [a] }
>
> instance Functor ZipList where
>   fmap :: (a -> b) -> ZipList a -> ZipList b
>   fmap f (ZipList xs) = ZipList (fmap f xs)
>
> instance Applicative ZipList where
>   pure :: a -> ZipList a
>   pure x = ZipList (repeat x)  -- See exercise question 2 for explanation
>
>   (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
>   (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)

  - When viewed as the context of the results of nondeterministic operations, we treat `fs <*> xs` as each function applied to each input, collected together:

> instance Applicative [] where
>   pure :: a -> [a]
>   pure x = [x]
>
>   (<*>) :: [a -> b] -> [a] -> [b]
>   fs <*> xs = [ f x | f <- fs, x <- xs ]

- We can write nondeterministic computations in a natural style with applicatives: `(+) <$> [2,3,4] <*> pure 4`

- Other examples of instances:
  - `IO`: computations are executed to give functions/values, then applied
  - `Monoid a => ((,) a)`: `a` values are accumulated with each computation
  - `Monoid a => (Const a)`: `Const a b` just contains an `a`, useful with `Foldable`
  - `WrappedMonad`/`WrappedArrow`: make instances of `Monad`/`Arrow` into `Applicative`s


**Exercises**

{*Question 1*}  
Implement an instance of `Applicative` for `Maybe`.

---

> instance Applicative Maybe where
>   pure :: a -> Maybe a
>   pure = Just
>
>   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
>   (Just f) <*> (Just x) = Just (f x)
>   _ <*> _ = Nothing

{*Question 2*}  
Determine the correct definition of `pure` for the `ZipList` instance of `Applicative`.  
There is only one implementation that satisfies the law relating `pure` and `(<*>)`.

---

See above for answer. We must be able to match the (unknown) length of the list we are zipping with, to fulfill the identity law.


Intuition
---------

- Generalization of function application in context to more than one argument: `f <$> x1 <*> ... <*> xn`
  - Called "applicative style"
  - Can also embed non-effectful arguments in the middle: `f <$> x1 <*> pure x2 <*> ... <*> xn`
  - As of GHC 8, can write applicative style using `ApplicativeDo` extension:

> {-
> do v1 <- x1
>    v2 <- x2
>   ...
>    vn <- xn
>    pure $ f v1 v2 .. vn
> -}


Utility Functions
-----------------

- `liftA` is like a more restrictive `fmap`, because it is constrained with `Applicative`. No real reason to use it
- `liftA2` lifts a 2-argument function to work in the context of an `Applicative`
- `liftA3` is like the above, but for a 3-argument function. There are no higher-arity `liftAn` functions
- `(*>)` sequences two effectful computations (so if the first argument is `Nothing`, for example, the whole thing is `Nothing`), but only keeps the result of the second
- `(<*)` is the same as the above, but only keeps the result of the first computation
- `(<**>)` is like `(<*>)`, but the first argument is a computation that is run first, then whose result is passed to the function produced by the second argument/computation. This is similar to `flip (<*>)`, except the order in which the computations are executed is reversed. Thus, non-commutative `Applicative` instances (such as that for `[]`), will have different results (`(<**>) [1,2] [(+5),(*10)] == [6,10,7,20]` whereas `flip (<*>) [1,2] [(+5),(*10)] == [6,7,10,20]`)
- `when` executes a computation if the given test is `True`, and returns `pure ()` otherwise
- `unless` does the same as `when`, except when the test is `False` instead of `True`


**Exercises**

{*Question 1*}  
Implement a function `sequenceAL :: Applicative f => [f a] -> f [a]`.  
There is a generalized version of this, `sequenceA`, which works for any `Traversable`, but implementing this version specialized to lists is a good exercise.

> sequenceAL :: Applicative f => [f a] -> f [a]
> sequenceAL [] = pure []
> sequenceAL (x:xs) = (:) <$> x <*> sequenceAL xs


Alternate Form
--------------

- Equivalent way of encoding applicative functor, as (lax) monoidal functor:

> class Functor f => Monoidal f where
>   unit :: f ()
>   (**) :: f a -> f b -> f (a,b)

- Intuition: has a default "shape" (`unit`) and supports a certain "combining" operation (`(**)`)
  - Idea: `f` preserves "monoidal structure" of default type `()` and combining type constructor `(,)`
  - Can be more clearly seen if rewritten as follows:

> {-
> unit :: () -> f ()
> (**) :: (f a, f b) -> f (a, b)
> -}

- Monoidal laws, which are simpler than those of applicatives:

<!-- $$
\begin{aligned}
\text{unit ** v} &\cong \text{v} && \text{(left identity)}\\
\text{u ** unit} &\cong \text{u} && \text{(right identity)}\\
\text{u ** (v ** w)} &\cong \text{(u ** v) ** w} && \text{(associativity)}\\
\text{fmap (g *** h) (u ** v)} &= \text{fmap g u ** fmap h v} && \text{(naturality)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/HWMpfG4nHq.svg"></div>

where `g *** h = \(x,y) -> (g x, h y)` and <!-- $\cong$ --> <img style="transform: translateY(0.1em); background: white;" src="../svg/jw5eub6eYx.svg"> means isomorphism (we consider `(x,())`, `x` and `((),x)` to be isomorphic, as well as `((x,y),z)` and `(x,(y,z))`).

  - In Haskell, (naturality) is a free theorem


**Exercises**

{*Question 1*}  
Implement `pure` and `(<*>)` in terms of `unit` and `(**)`, and vice versa.

---

> pure' :: (Applicative f, Monoidal f) => a -> f a
> pure' x = fmap (const x) unit
>
> (<*>>) :: (Applicative f, Monoidal f) => f (a -> b) -> f a -> f b
> f <*>> x = fmap (uncurry ($)) (f ** x)
>
> unit' :: (Applicative f, Monoidal f) => f ()
> unit' = pure ()
>
> (**>) :: (Applicative f, Monoidal f) => f a -> f b -> f (a,b)
> x **> y = (,) <$> x <*> y

{*Question 2*}  
Are there any `Applicative` instances for which there are also functions `f () -> ()` and `f (a,b) -> (f a, f b)`, satisfying some "reasonable" laws?

---

> instance Monoidal [] where
>   unit :: [()]
>   unit = [()]
>
>   (**) :: [a] -> [b] -> [(a,b)]
>   (**) = zip
>
> reverseUnit' :: [()] -> ()
> reverseUnit' _ = ()
>
> reverseStarStar' :: [(a,b)] -> ([a], [b])
> reverseStarStar' = unzip

Possible laws this instance fulfills:

<!-- $$
\begin{aligned}
\text{reverseUnit' (reverseUnit' x)} &= \text{reverseUnit' x} && \text{(idempotence)}\\
\text{reverseUnit' unit} &= \text{()} && \text{(complement)}\\
\text{uncurry (**) . reverseStarStar'} &= \text{id} = \text{reverseStarStar' . uncurry (**)} && \text{(semi-inverse)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/VCE2gnTevH.svg"></div>

{*Question 3*}  
Prove that given your implementations from the first exercise, the usual applicative laws and the monoidal laws stated above are equivalent.

---

(Forward direction)

<!-- $
(\implies)
$ --> <img style="transform: translateY(0.1em); background: white;" src="../svg/vZGEoRoFHr.svg"> Assume applicative laws hold:

1. `pure id <*> v = v`
2. `pure f <*> pure x = pure (f x)`
3. `u <*> pure y = pure ($ y) <*> u`
4. `u <*> (v <*> w) = pure (.) <*> u <*> v <*> w`.

- Proving (left identity):
<!-- $$
\begin{aligned}
\text{unit ** v}
&\equiv \text{(,) <$> pure () <*> v}\\
&\equiv \text{pure (,) <*> pure () <*> v} && \text{changing to canonical form}\\
&\equiv \text{pure ((,) ()) <*> v} && \text{(homomorphism)}\\
&\equiv \text{pure ((), v)}\\
&\cong \text{v}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/UjrsvgrObb.svg"></div>

- Proving (right identity):
<!-- $$
\begin{aligned}
\text{u ** unit}
&\equiv \text{(,) <$> u <*> pure ()}\\
&\equiv \text{pure (,) <*> u <*> pure ()} && \text{changing to canonical form}\\
&\equiv \text{(pure (,) <*> u) <*> pure ()}\\
&\equiv \text{pure ($ ()) <*> pure (,) <*> u} && \text{(interchange)}\\
&\equiv \text{pure (($ ()) (,)) <*> u} && \text{(homomorphism)}\\
&\equiv \text{pure ((,) ()) <*> u}\\
&\equiv \text{pure ((), u)}\\
&\cong \text{u}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/AUjuk7nmSS.svg"></div>

- Proving (associativity):
<!-- $$
\begin{aligned}
\text{u ** (v ** w)}
&\equiv \text{(,) <$> u <*> (v ** w)}\\
&\equiv \text{(,) <$> u <*> ((,) <$> v <*> w)}\\
&\equiv \text{pure (,) <*> u <*> (pure (,) <*> v <*> w)} && \text{changing to canonical form}\\
&\equiv \text{pure (,) <*> u <*> pure (v, w)}\\
&\equiv \text{(u, (v, w))}\\
&\cong \text{((u, v), w)}\\
&\equiv \text{pure (,) <*> (pure (,) <*> u <*> v) <*> w}\\
&\equiv \text{(,) <$> ((,) <$> u <*> v) <*> w} && \text{reversing canonical form}\\
&\equiv \text{(,) <$> (u ** v) <*> w}\\
&\equiv \text{(u ** v) ** w}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/MLLUgiAkHr.svg"></div>

<!-- $
(\impliedby)
$ --> <img style="transform: translateY(0.1em); background: white;" src="../svg/49KpVWi18S.svg"> Assume monoidal functor laws hold:

1. `unit ** v ~= v`
2. `u ** unit ~= u`
3. `u ** (v ** w) ~= (u ** v) ** w`.

- Proving (identity):
<!-- $$
\begin{aligned}
\text{pure id <*> v}
&\equiv \text{fmap (uncurry ($)) (pure id ** v)}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure id ** v)}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure (id, v'))}, && \text{where v' is an "unwrapped" version of v}\\
&\equiv \text{pure ((\(f,x) -> f x) (id, v'))}\\
&\equiv \text{pure (id v')}\\
&\equiv \text{pure v'}\\
&\equiv \text{v}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/swdR5Hlv1Y.svg"></div>

- Proving (homomorphism):
<!-- $$
\begin{aligned}
\text{pure f <*> pure x}
&\equiv \text{fmap (uncurry ($)) (pure f ** pure x)}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure f ** pure x)}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure (f, x))}\\
&\equiv \text{pure ((\(f,x) -> f x) (f, x))}\\
&\equiv \text{pure (f x)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/U07NHImpbG.svg"></div>

- Proving (interchange):
<!-- $$
\begin{aligned}
\text{u <*> pure y}
&\equiv \text{fmap (uncurry ($)) (u ** pure y)}\\
&\equiv \text{fmap (\(f,x) -> f x) (u ** pure y)}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure (u', y))}, && \text{where u' is an "unwrapped" version of u}\\
&\equiv \text{pure ((\(f,x) -> f x) (u', y))}\\
&\equiv \text{pure (u' y)}\\
&\equiv \text{pure (($ y) u')}\\
&\equiv \text{pure ((\(f,x) -> f x) (($ y), u'))}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure (($ y), u'))}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure ($ y) ** u)}\\
&\equiv \text{fmap (uncurry ($)) (pure ($ y) ** u)}\\
&\equiv \text{pure ($ y) <*> u}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/PLvLa5oSv4.svg"></div>

- Proving (composition):
<!-- $$
\begin{aligned}
\text{u <*> (v <*> w)}
&\equiv \text{u <*> (fmap (uncurry ($)) (v ** w))}\\
&\equiv \text{fmap (uncurry ($)) (u ** (fmap (uncurry ($)) (v ** w)))}\\
&\equiv \text{fmap (\(f,x) -> f x) (u ** (fmap (\(f,x) -> f x) (v ** w)))}\\
&\equiv \text{fmap (\(f,x) -> f x) (u ** (fmap (\(f,x) -> f x) (pure (v', w'))))}, && \text{where v', w' are "unwrapped" versions of v, w}\\
&\equiv \text{fmap (\(f,x) -> f x) (u ** (pure ((\(f,x) -> f x) (v', w'))))}\\
&\equiv \text{fmap (\(f,x) -> f x) (u ** (pure (v' w')))}\\
&\equiv \text{fmap (\(f,x) -> f x) (pure (u', v' w'))}, && \text{where u' is an "unwrapped" version of u}\\
&\equiv \text{pure ((\(f,x) -> f x) (u', v' w'))}\\
&\equiv \text{pure (u' (v' w'))}\\
&\equiv \text{pure (.) <*> u <*> v <*> w}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/z9UgKSatpP.svg"></div>
