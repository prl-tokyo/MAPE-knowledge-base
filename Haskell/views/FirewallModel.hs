{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FirewallModel (
  FWView(..)
  , FWRule(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data FWView = FWView {
  current :: [FWRule]
  , additions :: [FWRule]
  , deletions :: [FWRule]
  } deriving (Show, Eq)

data FWRule = FWRule {
  fwSecurityGroupRefFrom :: String,
  fwSecurityGroupRefTo :: String,
  fwPort :: Int,
  fwProtocol :: String
  } deriving (Show, Eq)

deriveBiGULGeneric ''FWView
deriveBiGULGeneric ''FWRule
