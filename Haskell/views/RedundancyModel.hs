{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module RedundancyModel (
  Instance(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data View = View {instances :: [Instance]} deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , securityGroupRef :: String
  , status :: Int
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''View
