{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module AutoScalingModel(
  ChangeView(..)
  , Instance(..)
  , InstanceType(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data ChangeView = ChangeView {
  current :: [Instance]
  , additions :: [Instance]
  , terminations :: [Instance]
  , cvInstanceTypes :: [InstanceType]
  } deriving (Show, Eq)

data View = View {
  instances :: [Instance]
  , vInstanceTypes :: [InstanceType]
  } deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , instload :: Double
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

data InstanceType = InstanceType {
  typeID :: String
  , typeCPUs :: Int
  , typeRAM :: Double
  , typeCost :: Double
  } deriving (Show, Eq)

instance Ord InstanceType where
  compare inst1 inst2 = compare (typeID inst1) (typeID inst2)


deriveBiGULGeneric ''ChangeView
deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''InstanceType
deriveBiGULGeneric ''View
