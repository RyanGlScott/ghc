{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
--
-- (c) The University of Glasgow
--

#include "HsVersions.h"

module Avail (
    Avails,
    AvailInfo(..),
    avail,
    availsToNameSet,
    availsToNameSetWithSelectors,
    availsToNameEnv,
    availName, availNames, availNonFldNames,
    availNamesWithSelectors,
    availTCFieldLabels,
    availFldFieldLabel,
    stableAvailCmp,
    plusAvail,
    trimAvail,
    filterAvail,
    filterAvails,
    nubAvails


  ) where

import GhcPrelude

import Name
import NameEnv
import NameSet

import FieldLabel
import Binary
import ListSetOps
import Outputable
import Util

import Data.Data ( Data )
import Data.List ( find )
import Data.Function

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are "available", i.e. in scope
data AvailInfo = Avail Name      -- ^ An ordinary identifier in scope
               | AvailFld FieldLabel
                                 -- ^ A record field that is not bundled with
                                 --   another type or class. Patteryn synonym
                                 --   record fields often fall under this
                                 --   umbrella. See also
                                 --   Note [Parents for record fields]
                                 --   in RdrName.
               | AvailTC Name
                         [Name]
                         [FieldLabel]
                                 -- ^ A type or class in scope. Parameters:
                                 --
                                 --  1) The name of the type or class
                                 --  2) The available pieces of type or class,
                                 --     excluding field selectors.
                                 --  3) The record fields of the type
                                 --     (see Note [Representing fields in AvailInfo]).
                                 --
                                 -- The AvailTC Invariant:
                                 --   * If the type or class is itself
                                 --     to be in scope, it must be
                                 --     *first* in this list.  Thus,
                                 --     typically: @AvailTC Eq [Eq, ==, \/=]@
                deriving( Eq, Data )
                        -- Equality used when deciding if the
                        -- interface has changed

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

{-
Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -XDuplicateRecordFields is disabled (the normal case), a
datatype like

  data T = MkT { foo :: Int }

gives rise to the AvailInfo

  AvailTC T [T, MkT] [FieldLabel "foo" False foo],

whereas if -XDuplicateRecordFields is enabled it gives

  AvailTC T [T, MkT] [FieldLabel "foo" True $sel:foo:MkT]

since the label does not match the selector name.

The labels in a field list are not necessarily unique:
data families allow the same parent (the family tycon) to have
multiple distinct fields with the same label. For example,

  data family F a
  data instance F Int  = MkFInt { foo :: Int }
  data instance F Bool = MkFBool { foo :: Bool}

gives rise to

  AvailTC F [F, MkFInt, MkFBool]
    [FieldLabel "foo" True $sel:foo:MkFInt, FieldLabel "foo" True $sel:foo:MkFBool].

Moreover, note that the flIsOverloaded flag need not be the same for
all the elements of the list.  In the example above, this occurs if
the two data instances are defined in different modules, one with
`-XDuplicateRecordFields` enabled and one with it disabled.  Thus it
is possible to have

  AvailTC F [F, MkFInt, MkFBool]
    [FieldLabel "foo" True $sel:foo:MkFInt, FieldLabel "foo" False foo].

If the two data instances are defined in different modules, both
without `-XDuplicateRecordFields`, it will be impossible to export
them from the same module (even with `-XDuplicateRecordfields`
enabled), because they would be represented identically.  The
workaround here is to enable `-XDuplicateRecordFields` on the defining
modules.
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail n1)       (Avail n2)   = n1 `stableNameCmp` n2
stableAvailCmp (AvailFld f1)    (AvailFld f2) = ((stableNameCmp `on` flSelector) f1 f2)
stableAvailCmp (Avail {})         (AvailFld {})  = LT
stableAvailCmp (Avail {})         (AvailTC {})   = LT
stableAvailCmp (AvailFld {})      (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns nfs) (AvailTC m ms mfs) =
    (n `stableNameCmp` m) `thenCmp`
    (cmpList stableNameCmp ns ms) `thenCmp`
    (cmpList (stableNameCmp `on` flSelector) nfs mfs)
stableAvailCmp (AvailFld {})      (Avail {})     = GT
stableAvailCmp (AvailTC {})       (AvailFld {})  = GT
stableAvailCmp (AvailTC {})       (Avail {})     = GT

avail :: Name -> AvailInfo
avail n = Avail n

-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNames avail)

availsToNameSetWithSelectors :: [AvailInfo] -> NameSet
availsToNameSetWithSelectors avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNamesWithSelectors avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName (Avail n)       = n
availName (AvailFld f)    = flSelector f
availName (AvailTC n _ _) = n

-- | All names made available by the availability information (excluding overloaded selectors)
availNames :: AvailInfo -> [Name]
availNames (Avail n)         = [n]
availNames (AvailFld f)      = [ flSelector f | not (flIsOverloaded f) ]
availNames (AvailTC _ ns fs) = ns ++ [ flSelector f | f <- fs, not (flIsOverloaded f) ]

-- | All names made available by the availability information (including overloaded selectors)
availNamesWithSelectors :: AvailInfo -> [Name]
availNamesWithSelectors (Avail n)         = [n]
availNamesWithSelectors (AvailFld f)      = [flSelector f]
availNamesWithSelectors (AvailTC _ ns fs) = ns ++ map flSelector fs

-- | Names for non-fields made available by the availability information
availNonFldNames :: AvailInfo -> [Name]
availNonFldNames (Avail n)        = [n]
availNonFldNames (AvailFld _)     = []
availNonFldNames (AvailTC _ ns _) = ns

-- | Fields made available from 'AvailTC', i.e., where the fields
-- have parent names (contrast this with 'availFldFieldLabel', which only
-- returns a name if it is from 'AvailFld', i.e., it has no parent).
availTCFieldLabels :: AvailInfo -> [FieldLabel]
availTCFieldLabels (Avail {})       = []
availTCFieldLabels (AvailFld {})    = []
availTCFieldLabels (AvailTC _ _ fs) = fs

-- | 'Just' a 'FieldLabel' if given an 'AvailFld', 'Nothing' otherwise.
availFldFieldLabel :: AvailInfo -> Maybe FieldLabel
availFldFieldLabel (Avail {})   = Nothing
availFldFieldLabel (AvailFld f) = Just f
availFldFieldLabel (AvailTC {}) = Nothing

-- -----------------------------------------------------------------------------
-- Utility

plusAvail :: AvailInfo -> AvailInfo -> AvailInfo
plusAvail a1 a2
  | debugIsOn && availName a1 /= availName a2
  = pprPanic "RnEnv.plusAvail names differ" (hsep [ppr a1,ppr a2])
plusAvail a1@(Avail {})         (Avail {})        = a1
plusAvail a1@(AvailFld {})      (AvailFld {})     = a1
plusAvail (AvailTC _ [] [])     a2@(AvailTC {})   = a2
plusAvail a1@(AvailTC {})       (AvailTC _ [] []) = a1
plusAvail (AvailTC n1 (s1:ss1) fs1) (AvailTC n2 (s2:ss2) fs2)
  = case (n1==s1, n2==s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionLists` ss2))
                                   (fs1 `unionLists` fs2)
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionLists` (s2:ss2)))
                                   (fs1 `unionLists` fs2)
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionLists` ss2))
                                   (fs1 `unionLists` fs2)
       (False,False) -> AvailTC n1 ((s1:ss1) `unionLists` (s2:ss2))
                                   (fs1 `unionLists` fs2)
plusAvail (AvailTC n1 ss1 fs1) (AvailTC _ [] fs2)
  = AvailTC n1 ss1 (fs1 `unionLists` fs2)
plusAvail (AvailTC n1 [] fs1)  (AvailTC _ ss2 fs2)
  = AvailTC n1 ss2 (fs1 `unionLists` fs2)
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

-- | trims an 'AvailInfo' to keep only a single name
trimAvail :: AvailInfo -> Name -> AvailInfo
trimAvail (Avail n)         _ = Avail n
trimAvail (AvailFld f)      _ = AvailFld f
trimAvail (AvailTC n ns fs) m = case find ((== m) . flSelector) fs of
    Just x  -> AvailTC n [] [x]
    Nothing -> ASSERT( m `elem` ns ) AvailTC n [m] []

-- | filters 'AvailInfo's by the given predicate
filterAvails  :: (Name -> Bool) -> [AvailInfo] -> [AvailInfo]
filterAvails keep avails = foldr (filterAvail keep) [] avails

-- | filters an 'AvailInfo' by the given predicate
filterAvail :: (Name -> Bool) -> AvailInfo -> [AvailInfo] -> [AvailInfo]
filterAvail keep ie rest =
  case ie of
    Avail n | keep n    -> ie : rest
            | otherwise -> rest
    AvailFld f | keep (flSelector f) -> ie : rest
               | otherwise           -> rest
    AvailTC tc ns fs ->
        let ns' = filter keep ns
            fs' = filter (keep . flSelector) fs in
        if null ns' && null fs' then rest else AvailTC tc ns' fs' : rest


-- | Combines 'AvailInfo's from the same family
-- 'avails' may have several items with the same availName
-- E.g  import Ix( Ix(..), index )
-- will give Ix(Ix,index,range) and Ix(index)
-- We want to combine these; addAvail does that
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = nameEnvElts (foldl add emptyNameEnv avails)
  where
    add env avail = extendNameEnv_C plusAvail env (availName avail) avail

-- -----------------------------------------------------------------------------
-- Printing

instance Outputable AvailInfo where
   ppr = pprAvail

pprAvail :: AvailInfo -> SDoc
pprAvail (Avail n)
  = ppr n
pprAvail (AvailFld fl)
  = ppr (flLabel fl)
pprAvail (AvailTC n ns fs)
  = ppr n <> braces (sep [ fsep (punctuate comma (map ppr ns)) <> semi
                         , fsep (punctuate comma (map (ppr . flLabel) fs))])

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailFld ab) = do
            putByte bh 1
            put_ bh ab
    put_ bh (AvailTC ac ad ae) = do
            putByte bh 2
            put_ bh ac
            put_ bh ad
            put_ bh ae
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              1 -> do ab <- get bh
                      return (AvailFld ab)
              _ -> do ac <- get bh
                      ad <- get bh
                      ae <- get bh
                      return (AvailTC ac ad ae)
