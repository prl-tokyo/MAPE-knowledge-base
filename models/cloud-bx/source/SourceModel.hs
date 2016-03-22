{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances, DeriveGeneric, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module SourceModel(
  Model(..)
  , Reservation(..)
  , VM(..)
  , SecurityGroup(..)
  , FirewallRule(..)
  ) where

data Model = Model {
    reservations :: [Reservation]
  , securityGroups :: [SecurityGroup]
  } deriving (Show, Eq)

data Reservation = Reservation {
    resID :: String
  , securityGroupRefs :: [String]
  , vms :: [VM]
  } deriving (Show, Eq)

instance Ord Reservation where
  compare res1 res2 = compare (resID res1) (resID res2)

data VM = VM {
    vmID :: String
  , vmType :: String
  , cost :: Double
  , cpu :: Int
  , ram :: Int
  , ami :: String
  , state :: Int
  , securityGroupRef :: String
  , load :: Double
} deriving (Show, Eq)

instance Ord VM where
  compare vm1 vm2 = compare (vmID vm1) (vmID vm2)

data SecurityGroup = SecurityGroup {
    sgID :: String
  , vmRefs :: [String]
  , firewallRules :: [FirewallRule]
  } deriving (Show, Eq)

instance Ord SecurityGroup where
  compare sg1 sg2 = compare (sgID sg1) (sgID sg2)

data FirewallRule = FirewallRule {
    fwID :: String
  , outbound :: Bool
  , port :: String
  , ip :: String
  , protocol :: String
  } deriving (Show, Eq)

instance Ord FirewallRule where
  compare fw1 fw2 = compare (fwID fw1) (fwID fw2)
