{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module RedundancyModel (
  ChangeView(..)
  , Instance(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data ChangeView = ChangeView {
  current :: [Instance]
  , additions :: [Instance]
  , terminations :: [Instance]
  } deriving (Show, Eq)

data View = View {instances :: [Instance]} deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , securityGroupRef :: String
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

deriveBiGULGeneric ''ChangeView
deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''View
