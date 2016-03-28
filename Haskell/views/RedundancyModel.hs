{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module RedundancyModel (
  ChangeView(..)
  , View(..)
  , VM(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data ChangeView = ChangeView {
  current :: [VM]
  , additions :: [VM]
  , terminations :: [VM]
  } deriving (Show, Eq)

data View = View {vms :: [VM]} deriving (Show, Eq)

data VM = VM {
  vmID :: String
  , securityGroupRef :: String
  } deriving (Show, Eq)

instance Ord VM where
  compare vm1 vm2 = compare (vmID vm1) (vmID vm2)

deriveBiGULGeneric ''ChangeView
deriveBiGULGeneric ''View
deriveBiGULGeneric ''VM
