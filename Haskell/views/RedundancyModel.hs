{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module RedundancyModel (
    RView(..)
  , RVM(..)
  , rView1
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data RView = RView {rvms :: [RVM]} deriving (Show, Eq)

data RVM = RVM {
    rvmID :: String
  , rSecurityGroupRef :: String
  } deriving (Show, Eq)

instance Ord RVM where
  compare vm1 vm2 = compare (rvmID vm1) (rvmID vm2)

deriveBiGULGeneric ''RView
deriveBiGULGeneric ''RVM

rView1 = [RVM {rvmID = "vm1"
              , rSecurityGroupRef = "sg-123"}
         ,RVM {rvmID = "vm2"
              , rSecurityGroupRef = "sg-123"}
         ,RVM {rvmID = "vm3"
              , rSecurityGroupRef = "sg-123"}]
