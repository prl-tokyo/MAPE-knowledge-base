{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FirewallModel (
  Rule(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data View = View {
  rules :: [Rule]
  } deriving (Show, Eq)

data Rule = Rule {
  securityGroupRefFrom :: String
  , securityGroupRefTo :: String
  , port :: Int
  , protocol :: String
  } deriving (Show, Eq)

deriveBiGULGeneric ''View
deriveBiGULGeneric ''Rule
