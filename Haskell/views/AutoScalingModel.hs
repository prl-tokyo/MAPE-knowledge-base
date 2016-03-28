{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module AutoScalingModel(
  View(..)
  , Instance(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data View = View {
  current :: [Instance]
  , additions :: [Instance]
  , terminations :: [Instance]
  } deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , instload :: Double
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''View
