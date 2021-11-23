`Monad`
=======

> {-# LANGUAGE InstanceSigs #-}
>
> import Prelude hiding (Monad(..))
> import Control.Monad.Identity (Identity(..))
>
> -- Needed for compilation
> main :: IO ()
> main = pure () -- Cannot use return yet!


Definition
----------

> class Applicative m => Monad m where
>   return :: a -> m a
> 
>   (>>=) :: m a -> (a -> m b) -> m b
> 
>   (>>) :: m a -> m b -> m b
>   m >> n = m >>= \_ -> n
> 
>   -- fail :: String -> m a

- Every `Monad` is an `Applicative`
- `return == pure` and `(>>) == (*>)`
  - Still have both for historical reasons
  - Proposal to remove these from `Monad` typeclass (Monad of No Return)
- Notice that `(>>)` ignores the result of `m`, but not its _effects_
- `fail` is an awful hack that has no place here (now taken out into `MonadFail`)
- `(>>=)` is called `bind`


Instances
---------

> instance Monad Identity where
>   return :: a -> Identity a
>   return = Identity
> 
>   (>>=) :: Identity a -> (a -> Identity b) -> Identity b
>   m >>= k = k (runIdentity m)

- In a chain of computations, if one returns `Nothing`, all the rest return `Nothing`
- Models computations which may fail

> instance Monad Maybe where
>   return :: a -> Maybe a
>   return = Just
> 
>   (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
>   Just x >>= f = f x
>   Nothing >>= _ = Nothing

- The `IO` constructor is a monad
  - It is the only "magical" monad, whose implementation is hidden and handled by the compiler (may differ between compilers)
  - All other monads are pure, but allow us to write code as if it had side effects

- `((->) e)` is the `Reader` monad, where `e` is available to computations as a read-only environment
  - `Reader e a` is a `newtype` around `(e -> a)`
  - See the exercises for an implementation

- `((,) w)` is the `Writer` monad, where `w`, a `Monoid`, accumulates information as computations happen (like logging)
  - `Writer w a` is a `newtype` around `(w,a)`

> instance Monoid w => Monad ((,) w) where
>   return :: a -> (w,a)
>   return = ((,) mempty)
> 
>   (>>=) :: (w,a) -> (a -> (w,b)) -> (w,b)
>   (w,x) >>= f = let (w',x') = f x in (w <> w', x')

- `State s a` is a `newtype` wrapper around `s -> (s,a)`
  - It is a computation that produces `a`, but also has a state `s` it can use and modify in the process

> newtype State s a = State { runState :: s -> (a,s) }
> 
> instance Functor (State s) where
>   fmap :: (a -> b) -> State s a -> State s b
>   fmap f (State sx) = State $ \s ->
>     let (x, s') = sx s
>     in (f x, s')
> 
> instance Applicative (State s) where
>   pure :: a -> State s a
>   pure x = State $ \s -> (x, s)
> 
>   (<*>) :: State s (a -> b) -> State s a -> State s b
>   (State sf) <*> sx = State $ \s ->
>     let (f, s') = sf s
>     in runState (f <$> sx) s'
> 
> instance Monad (State s) where
>   return :: a -> State s a
>   return = pure
> 
>   (>>=) :: State s a -> (a -> State s b) -> State s b
>   (State sx) >>= sf = State $ \s ->
>     let (x, s') = sx s
>     in runState (sf x) s'

- `Cont r a` is a `newtype` wrapper around `(a -> r) -> r` and represents computations in CPS (continuation-passing style)
  - Can be used to suspend and resume, implement co-routines and other complex control structures
  - Called the "mother of all monads" due to universal properties

> newtype Cont r a = Cont { runCont :: (a -> r) -> r }
> 
> instance Functor (Cont r) where
>   fmap :: (a -> b) -> Cont r a -> Cont r b
>   fmap f (Cont c) = Cont $ \g -> c (g . f)
> 
> instance Applicative (Cont r) where
>   pure :: a -> Cont r a
>   pure x = Cont $ \f -> f x
> 
>   (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
>   (Cont cFn) <*> (Cont c) = Cont $ \g -> cFn (\f -> c (g . f))
> 
> instance Monad (Cont r) where
>   return :: a -> Cont r a
>   return = pure
> 
>   (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
>   (Cont c) >>= f = Cont $ \g -> c (\x -> (runCont (f x)) g)


**Exercises**

{*Question 1*}  
Implement a `Monad` instance for the list constructor, `[]`.

---

> instance Monad [] where
>   return :: a -> [a]
>   return x = [x]
> 
>   (>>=) :: [a] -> (a -> [b]) -> [b]
>   [] >>= _ = []
>   xs >>= f = concat [ f x | x <- xs ]

{*Question 2*}  
Implement a `Monad` instance for `((->) e)`.

---

> instance Monad ((->) e) where
>   return :: a -> (e -> a)
>   return = const
> 
>   (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
>   f >>= ff = \e -> ff (f e) e

{*Question 3*}  
Implement `Functor` and `Monad` instances for `Free f`, defined as

> data Free f a
>   = Var a
>   | Node (f (Free f a))

You may assume that `f` has a `Functor` instance. This is known as the free monad built from the functor `f`.

---

> instance Functor f => Functor (Free f) where
>   fmap :: (a -> b) -> Free f a -> Free f b
>   fmap f (Var x) = Var (f x)
>   fmap f (Node x) = Node $ fmap (fmap f) x
> 
> instance Functor f => Applicative (Free f) where
>   pure :: a -> Free f a
>   pure = Var
> 
>   (<*>) :: Free f (a -> b) -> Free f a -> Free f b
>   (Var f) <*> x = fmap f x
>   (Node f) <*> x = Node $ fmap (<*> x) f
> 
> instance Functor f => Monad (Free f) where
>   return :: a -> Free f a
>   return = pure
> 
>   (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>   (Var x) >>= f = f x
>   (Node x) >>= f = Node $ fmap (>>= f) x


Intuition
---------

- Combines two computations into a larger computation
  - First argument is computed, then result of that is used to produce second computation
  - In applicative, computations have no way of interacting with each other

- Structure of applicative is fixed, but structure of monad can change based on intermediate results
  - Thus, applicative parsers can only parse context-free langs; monad parser needed for context-sensitive
  - (Actually, because of recursion, can construct infinite grammars that do the same thing)

- Try to implement `(>>=)` in terms of `fmap`, `pure` and `(<*>)`
  - Given monadic value `x :: m a` and function `f :: a -> m b`, so only thing to do is apply `f` to `x`
  - Must use `fmap` to lift `f` to `m a -> m (m b)`
  - We want end result `m b` though; we can add `m`'s using `pure`, but we cannot collapse `m`'s
  - But function exists that does this:
    - `Monad` can be expressed in terms of just `join`:

> class Applicative m => Monad' m where
>   join :: m (m a) -> m a

    - Monads in category theory actually expressed in terms of `return`, `fmap` and `join`
    - Can sometimes be easier to think of instances in terms of `join`, since it is more "atomic" (e.g. `join` for `[]` is just `concat`)


**Exercises**

{*Question 1*}  
Implement `(>>=)` in terms of `fmap` (or `liftM`) and `join`.

---

> (>>=>) :: Monad m => m a -> (a -> m b) -> m b
> x >>=> f = join' (fmap f x)

{*Question 2*}  
Implement `join` and `fmap` (`liftM`) in terms of `(>>=)` and `return`.

---

> join' :: Monad m => m (m a) -> m a
> join' x = x >>= id
> 
> fmap' :: Monad f => (a -> b) -> f a -> f b
> fmap' f fx = fx >>= (\x -> return (f x))


Utility Functions
-----------------

- `join`
- `liftM` is just `fmap` with a more restrictive `Monad` constraint. Relic from when `Functor` wasn't a superclass of `Monad`
- `ap` is just `(<*>).` As above, relic from when `Monad` didn't require an `Applicative` instance
  - We can make any `Monad` an instance of `Applicative` by setting `pure = return` and `(<*>) = ap`
- `sequence` takes a list of computations and merges them into a single computation which amasses a list of the results
  - Has a `Monad` constraint due to history, but can actually be implemented with just an `Applicative` constraint
  - Actually works over any `Traversable`, not just lists
- `replicateM` is like `sequence`, except the input is a single computation whose results have been replicated
- `mapM` maps a function over a list of regular values, and sequences the results to collect them in a single monadic context
  - Can also just be implemented with an `Applicative` constraint (called `traverse` in that case)
  - Also works over any `Traversable`, not just lists
- `forM` is `mapM` with the arguments reversed, to mimic a `for`-loop look
- `(=<<)` is `(>>=)` with arguments reversed
- `(>=>)` is kind of like function composition, but with the functions returning monads and their position in the composition reversed
- `(<=<)` is `(>=>)` with arguments reversed
- Many of these functions have variants ending in an underscore; these throw away the results of the computations passed to them, using them only for their side-effects
- Other ones that might be useful: `filterM`, `zipWithM`, `foldM`, `forever`


Laws
----

- A monad must satisfy the following laws:

<!-- $$
\begin{aligned}
\text{return x >>= f} &= \text{f x} && \text{(left identity)}\\
\text{m >>= return} &= \text{m} && \text{(right identity)}\\
\text{m >>= (\x -> f x >>= g)} &= \text{(m >>= f) >>= g} && \text{(associativity)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/ohBxXfQBw4.svg"></div>

  - Presentation of these laws is more complicated than necessary due to the asymmetry of `(>>=)`

- More elegant version can be expressed using `(>=>)` (the Kleisli composition operator or `fish`):

<!-- $$
\begin{aligned}
\text{return >=> f} &= \text{f} && \text{(left identity)}\\
\text{g >=> return} &= \text{g} && \text{(right identity)}\\
\text{(f >=> g) >=> h} &= \text{f >=> (g >=> h)} && \text{(associativity)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/EVAp9hi4XE.svg"></div>

  - Note: these laws say functions of type `a -> m b` are the arrows of a category where `(>=>)` is composition (the Kleisli category of the monad `m`).

- There is also a formulation of the monad laws using `fmap`, `return` and `join`.


**Exercises**

{*Question 1*}  
Given the definition `g >=> h == \x -> g x >>= h`, prove the equivalence of the above laws and the usual monad laws.

---

- Proving equivalence of (left identity):
<!-- $$
\begin{aligned}
\text{return >=> f}
&\equiv \text{f}\\
\text{\x -> return x >== f}
&\equiv \text{f}\\
\text{(\x -> return x >== f) y}
&\equiv \text{f y}\\
\text{return y >>= f}
&\equiv \text{f y}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/lnCoojWO9U.svg"></div>

- Proving equivalence of (right identity):
<!-- $$
\begin{aligned}
\text{g >=> return}
&\equiv \text{g}\\
\text{\x -> g x >== return}
&\equiv \text{g}\\
\text{g >>= return}
&\equiv \text{g}, && \text{since return doesn't perform any additional computation}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/N7THU3UAke.svg"></div>

- Proving equivalence of (associativity):
<!-- $$
\begin{aligned}
\text{(f >=> g) >=> h}
&\equiv \text{f >=> (g >=> h)}\\
\text{(\x -> f x >>= g) >=> h}
&\equiv \text{f >=> (\x -> g x >>= h)}\\
\text{\y -> (\x -> f x >>= g) y >>= h}
&\equiv \text{\y -> f y >>= (\x -> g x >>= h)}\\
\text{\y -> (f y >>= g) >>= h}
&\equiv \text{\y -> f y >>= (\x -> g x >>= h)}\\
\text{(f >>= g) >>= h}
&\equiv \text{\y -> f y >>= (\x -> g x >>= h)}\\
\text{(f >>= g) >>= h}
&\equiv \text{f >>= (\x -> g x >>= h)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/8Ss6iIE2Na.svg"></div>


`do`-Notation
-------------

- Syntactic sugar for chains of monadic expressions, that allows writing them as steps in an imperative style
- Comes from fact that `a >>= \x -> b >> c >>= \y -> d` can be made more readable by putting each computation on a separate line:

> {-
> a >>= \x ->
> b >>
> c >>= \y ->
> d
> -}

  - Emphasizes the 4 separate computations, and that the result of `a` is bound to `x`/the result of `c` is bound to `y`
  - Notice that `b`, `c` and `d` are allowed to refer to `x`, and `d` is allowed to refer to `y`
- We can take this and imagine an even nicer representation:

> {-
> do {
>   x <- a;
>        b;
>   y <- c;
>        d
> }
> -}

  - `do`-blocks are recursively translated into monad operations approximately by the following rules:
    - `do e -> e`
    - `do { e; stmts } -> e >> do { stmts }`
    - `do { v <- e; stmts } -> e >>= \v -> do { stmts }`
    - `do { let decls; stmts } -> let decls in do { stmts }`
  - There is also the possibility to pattern match. For example, `do { (x:xs) <- foo; bar x }`
    - What happens if `foo` is an empty list? The (ugly) `fail` function happens!

  - Sometimes, `Monad` methods are not actually needed to desugar `do`-notation. For example:

> {-
> do x <- foo
>    y <- bar
>    z <- baz
>    return (g x y z)
> -}

    - This is equivalent to `g <$> foo <*> bar <*> baz` and therefore does not need to be desugared following the above transformations
    - With `ApplicativeDo` enabled, GHC tries to desugar `do`-blocks using `Applicative` methods where it can
      - This may lead to efficiency gains since in general applicative computations are not interdependent and thus may run in parallel, whereas monadic ones may not

  - Note that `do`-notation likens itself more to the intuition of a "computational context" instead of "container"
    - The notation `x <- m` is like taking a single `x` from `m` and doing something with it
