{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module RedundancyModel (
    RView(..)
  , RVM(..)
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
