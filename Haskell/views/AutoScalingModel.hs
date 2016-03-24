{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}
module AutoScalingModel(
  View(..)
  , VVM(..)
  , asView1
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

asView1 = [VVM {vvmID = "vm1", vvmType = "t2.micro", vload = 1.12}
        ,VVM {vvmID = "vm2", vvmType = "t2.micro", vload = 0.42}
        ,VVM {vvmID = "vm3", vvmType = "t4.large", vload = 1.32}]