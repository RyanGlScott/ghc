-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.OnOff.Type
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- TODO RGS Docs
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module GHC.OnOff.Type ( OnOff(..) ) where

import Data.Data
import GHC.Generics

-- TODO RGS Docs
data OnOff a = On a
             | Off a
   deriving (Eq, Ord, Show, Data, Generic)
