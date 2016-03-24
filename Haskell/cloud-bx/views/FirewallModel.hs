{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, TemplateHaskell, ScopedTypeVariables #-}

module FirewallModel (
  FWView(..)
  , FWRule(..)
  , FWSG(..)
  ) where

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
