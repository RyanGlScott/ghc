{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaStandalone where

import Control.Applicative
import Data.Functor.Compose
import Data.Semigroup

newtype App f a = App (f a)
  deriving newtype
    (Functor, Applicative)

instance (Applicative f, Semigroup a) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

deriving via (App (Compose f g) a)
         instance (Applicative f, Applicative g, Semigroup a)
               => Semigroup (Compose f g a)
