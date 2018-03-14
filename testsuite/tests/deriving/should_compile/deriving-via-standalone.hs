{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaStandalone where

import Control.Applicative
import Data.Functor.Compose
import Data.Proxy
import Data.Semigroup

newtype App (f :: * -> *) a = App (f a)
  deriving newtype
    (Functor, Applicative)

instance (Applicative f, Semigroup a) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

deriving via (App (Compose (f :: * -> *) g) a)
         instance (Applicative f, Applicative g, Semigroup a)
               => Semigroup (Compose f g a)

class C (a :: k -> *)
instance C Proxy

newtype MyProxy a = MyProxy (Proxy a)
deriving via (Proxy :: * -> *) instance C MyProxy
