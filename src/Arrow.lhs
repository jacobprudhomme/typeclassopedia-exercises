`Arrow`
=======

> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE RebindableSyntax #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE TypeOperators #-}
>
> import Control.Category (Category(..), (>>>))
> import Control.Monad ((>=>))
> import Prelude hiding ((.))
>
> -- Needed for compilation
> main :: IO ()
> main = return ()

- Abstraction of computation, similar to `Applicative` and `Monad`
  - But doesn't just reflect type of output, reflects type of input too
- Generalization of functions
  - If `arr` is an instance of `Arrow`, ``a `arr` b`` can be thought of as a computation that takes values of type `a` as input, and produces values of type `b`
- `(->)` instance of `Arrow` is just a pure function, but in general, can represent an "effectful" computation


Definition
----------

> class Category arr => Arrow arr where
>   arr :: (b -> c) -> (b `arr` c)
>   first :: (b `arr` c) -> ((b,d) `arr` (c,d))
>   second :: (d `arr` c) -> ((b,d) `arr` (b,c))
>   (***) :: (b `arr` c) -> (b' `arr` c') -> ((b,b') `arr` (c,c'))
>   (&&&) :: (b `arr` c) -> (b `arr` d) -> (b `arr` (c,d))

- Because of `Category` constraint, get identity arrows and arrow composition for free
  - Suppose have 2 arrows ``g :: b `arr` c`` and ``h :: c `arr` d``, then their composition ``g >>> h :: b `arr` d``

- Only methods that must be defined are `arr` and `first`, others have default definitions in terms of these

- `first` and `second` conflict with methods of same name for `Bifunctor`
  - Must import one (or both) qualified if they are to be used together
  - Used to be common to import `Control.Arrow` just for `(->)` instance, in order to use `first` and `second` to modify parts of tuples, but it is recommended to import `Data.Bifunctor` for this instead
    - Notice for `(->)` instance of `Arrow` and `(,)` instance of `Bifunctor`, `first` and `second` specialize to the same type


Intuition
---------

- `arr` takes a standard function `b -> c` and turns it into an arrow ``b `arr` c``
  - `arr f` is "pure" in that it only computes `f`, with no effects
- `first` turns any arrow from `b` to `c` into one from `(b,d)` to `(c,d)` (i.e. `first f` uses `f` to process the first element in a tuple, passing the second one on untouched)
- `second` is similar to `first`, but acts on the second element of a tuple
  - Can be defined `swap . first . swap`
- `(***)` is "parallel/split composition" of arrows
  - Takes 2 arrows and makes them into 1 arrow on tuples, which uses the first arrow on the first element, and the second arrow on the second element
  - Mnemonic: `f *** g` is the "product" of `f` and `g` (hence the `*`)
  - Default implementation is in terms of `first`, `second` and sequential arrow composition `(>>>)`
- `(&&&)` is "fanout composition" of arrows
  - Takes 2 arrows and makes them into 1 arrow `f &&& g`, that takes a single input and supplies it to both arrows separately, returning the results as a tuple
  - Mnemonic: `f &&& g` performs both `f` "and" `g` (hence the `&`)


Instances
---------

- `Control.Arrow` only provides 2 `Arrow` instances itself, `(->)` and `Kleisli m`:

> instance Arrow (->) where
>   arr :: (b -> c) -> (b -> c)
>   arr f = f
>
>   first :: (b -> c) -> ((b,d) -> (c,d))
>   first f (x,y) = (f x, y)
>
> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
>
> instance Monad m => Category (Kleisli m) where
>   id :: Kleisli m a a
>   id = Kleisli return
>
>   (.) :: Kleisli m b c -> Kleisli m a b -> Kleisli m a c
>   Kleisli g . Kleisli h = Kleisli (h >=> g)
>
> instance Monad m => Arrow (Kleisli m) where
>   arr :: (b -> c) -> Kleisli m b c
>   arr f = Kleisli (return . f)
>
>   first :: Kleisli m b c -> Kleisli m (b,d) (c,d)
>   first (Kleisli f) = Kleisli (\ ~(b,d) -> do
>     c <- f b
>     return (c,d))


Laws
----

- Arrows must satisfy quite a few laws (I made up names where possible):

<!-- $$
\begin{aligned}
\text{arr id} &= \text{id} && \text{(identity)}\\
\text{arr (g . f)} &= \text{arr f >>> arr g} && \text{(composition)}\\
\text{first (arr f)} &= \text{arr (f ** id)}\\
\text{first (f >>> g)} &= \text{first f >>> first g}\\
\text{first f >>> arr (id *** g)} &= \text{arr (id *** g) >>> first f}\\
\text{first f >>> arr fst} &= \text{arr fst >>> f}\\
\text{first (first f) >>> arr assoc} &= \text{arr assoc >>> first f}\\
\text{assoc ((x,y),z)} &= \text{(x,(y,z))}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/wvk94N1LyY.svg"></div>

- These laws are a bit different from those in the literature, since some are now covered under the category laws

- Not too important to understand arrow laws in order to use them

- Additional laws exist for `ArrowChoice`, `ArrowApply` and `ArrowLoop`


`ArrowChoice`
-------------

- Computations built using `Arrow` class are a bit inflexible, like `Applicative` ones
  - Structure of computation is fixed, no ability to choose between alternate execution paths based on intermediate results
  - `ArrowChoice` provides this ability

> class Arrow arr => ArrowChoice arr where
>   left  :: (b `arr` c) -> (Either b d `arr` Either c d)
>   right :: (d `arr` c) -> (Either b d `arr` Either b c)
>   (+++) :: (b `arr` c) -> (b' `arr` c') -> (Either b b' `arr` Either c c')
>   (|||) :: (b `arr` d) -> (c `arr` d) -> (Either b c `arr` d)

- There are some similarities between these and `Arrow` methods
  - These are the dual of the `Arrow` methods; latter operate on product types (tuples), these operate on sum types
- In general, these functions create arrows whose input is tagged with `Left` or `Right`, and can choose how to proceed based on these tags

- On inputs tagged with `Left`, `left f` has the behaviour of `f`. For inputs tagged with `Right`, it behaves as identity
- `right` is similar, but for inputs tagged with `Right` (acting as identity for those tagged with `Left`)
- `(+++)` performs "multiplexing"
  - `f +++ g` behaves as `f` on inputs tagged with `Left`, and as g for inputs tagged with `Right`, preserving the tags
  - Mnemonic: `f +++ g` is the "sum" of 2 arrows (hence the `+`)
- `(|||)` is "merge/fanin"
  - `f ||| g` behaves as `f` on inputs tagged with `Left`, and as `g` for inputs tagged with `Right`, but the tags are discarded and so `f` and `g` must have the same return type
  - Mnemonic: `f ||| g` performs either `f` "or" `g` (hence the `|`)

- `ArrowChoice` allows computations to choose between a finite amount of execution paths based on intermediate results
- The possible execution paths must be known in advance and assembled with `(+++)` and `(|||)`
- Sometimes require more flexibility; for example, if we want to compute an arrow from intermediate results, then use this computed arrow to continue the computation
  - Given to us by `ArrowApply`


`ArrowApply`
------------

> class Arrow arr => ArrowApply arr where
>   app :: (b `arr` c, b) `arr` c
>
>   app2 :: b `arr` ((b `arr` c) `arr` c)
>   app2 = arr (\x -> app . arr (,x))

- I'm not sure how I determined `app2`... I adapted the `ArrowApply` instance of `Schoenfinkel` from `acme-schoenfinkel`, and eventually the types fit

- Notion of being able to compute a new computation may sound familiar... it's what `(>>=)` does!
  - `ArrowApply` and `Monad` are actually equivalent!
  - `Kleisli m` can be made an instance of `ArrowApply`, and any instance of `ArrowApply` can be made a `Monad` via the `newtype` wrapper `ArrowMonad`

> instance Monad m => ArrowApply (Kleisli m) where
>   app :: Kleisli m (Kleisli m b c, b) c
>   app = Kleisli (\(Kleisli f, x) -> f x)  -- Look! It "unwraps" the arrow, applies it to get a new value, and wraps that, much like bind!
>
> newtype ArrowMonad a b = ArrowMonad (a () b)  -- Let arr = a, then this is ArrowMonad (() `arr` b)
>
> instance Arrow a => Functor (ArrowMonad a) where
>   fmap :: (b -> c) -> ArrowMonad a b -> ArrowMonad a c
>   fmap f (ArrowMonad g) = ArrowMonad $ g >>> arr f
>
> instance Arrow a => Applicative (ArrowMonad a) where
>   pure :: b -> ArrowMonad a b
>   pure = ArrowMonad . arr . const
>
>   (<*>) :: ArrowMonad a (b -> c) -> ArrowMonad a b -> ArrowMonad a c
>   (ArrowMonad f) <*> (ArrowMonad g) = ArrowMonad $ (f &&& g) >>> arr (uncurry Prelude.id)
>
> instance ArrowApply a => Monad (ArrowMonad a) where
>   return :: b -> ArrowMonad a b
>   return = ArrowMonad . arr . const
>
>   (>>=) :: ArrowMonad a b -> (b -> ArrowMonad a c) -> ArrowMonad a c
>   (ArrowMonad f) >>= g = ArrowMonad $ f >>> arr (\x -> let (ArrowMonad h) = g x in (h,())) >>> app


`ArrowLoop`
-----------

> class Arrow arr => ArrowLoop arr where
>   loop :: (b,d) `arr` (c,d) -> b `arr` c
>
> trace :: ((b,d) -> (c,d)) -> b -> c
> trace f b = let (c,d) = f (b,d) in c

- Describes arrows that use recursion to compute results
- Used to desugar `rec` in arrow notation

- `loop` is intended to be a generalization of the `trace` function shown
  - `d` component of the first arrow's output is fed back in as input
  - So, the arrow `loop g` is obtained by recursively fixing the second component of the input to `g`


Arrow Notation
--------------

- Can be difficult to program with arrow combinators directly, especially when needing to use many intermediate results
  - Normally, intermediate results would have to be kept in nested tuples, which programmer must keep track of

- Problem solved with special arrow notation supported by GHC (similar to `do` for monads)
- Allows names to be assigned to intermediate results
- Example, modelling a recursively defined counter circuit with a reset line:

> -- Have to redefined ifThenElse and returnA due to RebindableSyntax extension
> ifThenElse cond ifBranch elseBranch
>   | cond = ifBranch
>   | otherwise = elseBranch
>
> returnA :: Arrow a => a b b
> returnA = arr Control.Category.id
>
> class ArrowLoop arr => ArrowCircuit arr where
>   delay :: b -> (b `arr` b)
>
> counter :: ArrowCircuit arr => Bool `arr` Int
> counter =
>   proc reset -> do
>     rec output <- returnA -< if reset then 0 else next
>         next   <- delay 0 -< output + 1
>     returnA -< output


More Information
----------------

- Invention of `Arrow` was to generalize `Monad`
- Has been said that it lies between `Applicative` and `Monad` in power, but they are not directly comparable
- Calculus of arrows was invented that iterated on lambda calculus, which simplifies presentation of arrow laws
- There exists a precise technical sense in which `Arrow` can be seen as the intersection of `Applicative` and `Category`
- Some extensions to `Arrow` such as `BiArrow` have been explored, for two-way computation (instead of one-way)
