Monad Transformers
====================

- Often would like to combine two monads into one:
  - Stateful + non-deterministic computations (`State` + `[]`)
  - Computations with possible failure and that can consult a read-only env (`Maybe` + `Reader`)
  - etc.
- Unfortunately, monads do not compose as nicely as applicatives (so stick to the latter if you can), but some can be combined in certain ways


Transformers
------------

- `transformers` library provides some standard monad transformers, which add a particular capability to an existing monad

- `IdentityT` maps a monad to something isomorphic to itself
  - Useful in a similar sense to `id`; can be passed as an argument to things which are parametrized over an arbitrary transformer
- `StateT` adds a read-write state
- `ReaderT` adds a read-only environment
- `WriterT` adds a write-only log
- `RWST` combines the previous 3 into one
- `MaybeT` adds the possibility of failure
- `ErrorT` adds the possibility of failure with an arbitrary type to represent errors
- `ListT` adds non-determinism if its parameter is commutative (so `ListT []` is not a monad!)
- `ContT` adds continuation handling

- Example of transformer: `StateT s Maybe` is an instance of `Monad`; computations of type `StateT s Maybe a` may fail, and have access to mutable state of type `s`
- Transformers can be stacked multiple times
- Order of composition matters. For example, when a `StateT s Maybe a` fails, the state ceases to be updated. However, the state of `MaybeT (State s) a` can continue to be modified even after failure
  - This is because transformers compose monads inside-out; `MaybeT (State s) a ~= s -> (Maybe a, s)`
  - Basically, monads become more fundamental the further inside the stack they are, and their effects have precendence over that of outer ones

- There exist other ways to compose monads, such as with coproducts


Definition and Laws
-------------------

- All transformers should implement `MonadTrans`:

> class MonadTrans t where
>   lift :: Monad m => m a -> t m a

  - Allows arbitrary computations in base monad `m` to be lifted into computations in transformed monad `t m`
    - Note: type application associates like function application: `t m a = (t m) a`

- `lift` must satisfy these laws:

<!-- $$
\begin{aligned}
\text{lift . return}
&\equiv \text{return}\\
\text{lift (m >>= f)}
&\equiv \text{lift m >>= (lift . f)}
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="../svg/DL0W20q3os.svg"></div>

- Intuitively, these mean that `lift` transforms `m a` computations to `t m a` ones in a sensible way by sending the `return` and `(>>=)` of `m` to that of `t m`


**Exercises**

{*Question 1*}  
What is the kind of `t` in the declaration of `MonadTrans`?

---

`t :: (* -> *) -> * -> *`, because `m :: * -> *` and `a :: *`.


Capability
----------

- `mtl` package provides a typeclass for each transformer ability, e.g. `MonadState` exposes state-related methods like
   `get` and `put`, allowing you to use them any instance of `MonadState`—`State`, `MaybeT (State s)`, `StateT s (ReaderT r IO)`, etc.
  - Similar typeclasses exist for `Reader`, `Writer`, `Cont`, `IO`, etc.
  - The only issue is that as we gain more standard transformers, there will have to be more and more instances written for these typeclasses

- These typeclasses mostly get rid of explicitly needing to call `lift`, as the instance will determine the appropriate number of applications of `lift` dependeing on the stack
  - Also allow writing functions polymorphic in their monad stack. For example,

> foo :: State Int Char
> foo = modify (*2) >> return 'x'

  can be generalized to

> foo' :: MonadState Int m => m Char
> foo' = modify (*2) >> return 'x'

  This could be useful if later on you needed to introduce failure `(State Int => MaybeT (State Int))`, as an example

- This can be described as "capability-based" style, where functions are defined to work for any monad with a given capability—in the above case, state
  - This may run into issues as you try to scale it up; for example, if you need to maintain two independent states
    - Framework for solving this exists in the package `Monatron`


Composition
-----------

- The composition of two monads is not always a monad
- Applicative functors are closed under composition, so the problem must be with `join`
  - Let `m`, `n` be arbitrary `Monad`s. To make a `Monad` out of their composition, we would need to implement` join :: m (n (m (n a))) -> m (n a)`
    - Not known how this can be done in general. We cannot delegate to the `join` implementation for `m` or `n`, because neither instances of a given monad occur next to each other
    - Can be done if `n` distributes over `m`, i.e. if there is a function `distrib :: n (m a) -> m (n a)` that satisfies certain laws


**Exercises**

{*Question 1*}  
Implement `join' :: M (N (M (N a))) -> M (N a)`, given `distrib :: N (M a) -> M (N a)` and assuming `M` and `N` are instances of `Monad`.

---

1. `fmap distrib :: M b -> M c`, where `b = N (M (N a))` and `c = M (N (N a))`
2. `join :: M (M (N (N a))) -> M (N (N a))`
3. `fmap join :: M b -> M c`, where `b = N (N a)` and `c = N a`
4. In the end, we have `join' = fmap join . join . fmap distrib :: M (N (M (N a))) -> M (N a)`
