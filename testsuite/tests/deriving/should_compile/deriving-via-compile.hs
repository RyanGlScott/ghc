{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DerivingViaCompile where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor
import Data.Functor.Identity
import Data.Monoid
import Data.Kind

type f ~> g = forall xx. f xx -> g xx

-----
-- Simple example
-----

data Foo a = MkFoo a a
  deriving Show
       via (Identity (Foo a))

-----
-- Eta reduction at work
-----

newtype Flip p a b = Flip { runFlip :: p b a }

instance Bifunctor p => Bifunctor (Flip p) where
  bimap f g = Flip . bimap g f . runFlip

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip

newtype Bar a = MkBar (Either a Int)
  deriving Functor
       via (Flip Either Int)

-----
-- Monad transformers
-----

type MTrans = (Type -> Type) -> (Type -> Type)

-- From `constraints'
data Dict c where
  Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

infixl 1 \\
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

-- With `-XQuantifiedConstraints' this just becomes
--
--    type Lifting cls  trans = forall mm. cls mm => cls (trans mm)
--
--    type LiftingMonad trans = Lifting Monad trans
--
class LiftingMonad (trans :: MTrans) where
  proof :: Monad m :- Monad (trans m)

instance LiftingMonad (StateT s :: MTrans) where
  proof :: Monad m :- Monad (StateT s m)
  proof = Sub Dict

instance Monoid w => LiftingMonad (WriterT w :: MTrans) where
  proof :: Monad m :- Monad (WriterT w m)
  proof = Sub Dict

instance (LiftingMonad trans, LiftingMonad trans') => LiftingMonad (ComposeT trans trans' :: MTrans) where
  proof :: forall m. Monad m :- Monad (ComposeT trans trans' m)
  proof = Sub (Dict \\ proof @trans @(trans' m) \\ proof @trans' @m)

newtype Stack :: MTrans where
  Stack :: ReaderT Int (StateT Bool (WriterT String m)) a -> Stack m a
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Int
    , MonadState Bool
    , MonadWriter String
    )
  deriving (MonadTrans, MFunctor)
       via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

class MFunctor (trans :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (trans m ~> trans m')

instance MFunctor (ReaderT r :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (ReaderT r m ~> ReaderT r m')
  hoist nat = ReaderT . fmap nat . runReaderT

instance MFunctor (StateT s :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (StateT s m ~> StateT s m')
  hoist nat = StateT . fmap nat . runStateT

instance MFunctor (WriterT w :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (WriterT w m ~> WriterT w m')
  hoist nat = WriterT . nat . runWriterT 

infixr 9 `ComposeT`
newtype ComposeT :: MTrans -> MTrans -> MTrans where
  ComposeT :: { getComposeT :: f (g m) a } -> ComposeT f g m a
  deriving newtype (Functor, Applicative, Monad)

instance (MonadTrans f, MonadTrans g, LiftingMonad g) => MonadTrans (ComposeT f g) where
  lift :: forall m. Monad m => m ~> ComposeT f g m
  lift = ComposeT . lift . lift
    \\ proof @g @m

instance (MFunctor f, MFunctor g, LiftingMonad g) => MFunctor (ComposeT f g) where
  hoist :: forall m m' xx yy. Monad m => (forall xx. m xx -> m' xx) -> ComposeT f g m yy -> ComposeT f g m' yy
  hoist f = ComposeT . hoist (hoist f) . getComposeT 
    \\ proof @g @m

-----
-- Using tuples in a `via` type
-----

newtype X a = X (a, a)
  deriving (Semigroup, Monoid)
       via (Product a, Sum a)

  deriving (Show, Eq)
       via (a, a)

-----
-- Abstract data types
-----

class C f where
  c :: f a -> Int

newtype X2 f a = X2 (f a)

instance C (X2 f) where
  c = const 0

deriving via (X2 IO) instance C IO

----
-- Testing parser
----

newtype P0 a = P0 a             deriving Show via a
newtype P1 a = P1 [a]           deriving Show via [a]
newtype P2 a = P2 (a, a)        deriving Show via (a, a)
newtype P3 a = P3 (Maybe a)     deriving Show via (First a)
newtype P4 a = P4 (Maybe a)     deriving Show via (First $ a)
newtype P5 a = P5 a             deriving Show via (Identity $ a)
newtype P6 a = P6 [a]           deriving Show via ([] $ a)
newtype P7 a = P7 (a, a)        deriving Show via (Identity $ (a, a))
newtype P8 a = P8 (Either () a) deriving Functor via (($) (Either ()))

newtype f $ a = APP (f a) deriving newtype Show deriving newtype Functor 
