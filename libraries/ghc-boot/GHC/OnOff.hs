{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | See @GHC.LanguageExtensions@ for an explanation
-- on why this is needed
module GHC.OnOff
  ( module GHC.OnOff.Type
  ) where

import Data.Binary
import GHC.OnOff.Type

instance Binary a => Binary (OnOff a)
