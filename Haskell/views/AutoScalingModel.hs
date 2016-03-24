{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}
module AutoScalingModel(
  View(..)
  , VVM(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data View = View {
  current :: [VVM]
  , additions :: [VVM]
  , terminations :: [VVM]
  } deriving (Show, Eq)

data VVM = VVM {
  vvmID :: String
  , vvmType :: String
  , vload :: Double
  } deriving (Show, Eq)

instance Ord VVM where
  compare vm1 vm2 = compare (vvmID vm1) (vvmID vm2)

deriveBiGULGeneric ''VVM
deriveBiGULGeneric ''View
