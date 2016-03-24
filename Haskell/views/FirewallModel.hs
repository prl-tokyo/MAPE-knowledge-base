{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FirewallModel (
  FWView(..)
  , FWRule(..)
  , FWSG(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data FWView = FWView {
  fwRules :: [FWRule]
  , fwSecurityGroups :: [FWSG]
  } deriving (Show, Eq)

data FWRule = FWRule {
  fwSecurityGroupRefFrom :: String,
  fwSecurityGroupRefTo :: String,
  fwPort :: Int,
  fwProtocol :: String
  } deriving (Show, Eq)

data FWSG = FWSG {
  fwsgID :: String
  } deriving (Show, Eq)

deriveBiGULGeneric ''FWView
deriveBiGULGeneric ''FWRule
deriveBiGULGeneric ''FWSG
