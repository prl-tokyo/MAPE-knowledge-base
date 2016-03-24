{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module SourceModel(
  Model(..)
  , Current(..)
  , Addition(..)
  , Deletion(..)
  , Reservation(..)
  , VM(..)
  , SecurityGroup(..)
  , FirewallRule(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data Model = Model {
  current :: Current
  , additions :: [Addition]
  , deletions :: [Deletion]
  } deriving (Show, Eq)

data Current = Current {
  reservations :: [Reservation]
  , securityGroups :: [SecurityGroup]
  } deriving (Show, Eq)

data Addition = Addition {
  addFW :: [FirewallRule]
  , addVM :: [VM]
  } deriving (Show, Eq)

data Deletion = Deletion {
  delFW :: [FirewallRule]
  , delVM :: [VM]
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

deriveBiGULGeneric ''Model
deriveBiGULGeneric ''Current
deriveBiGULGeneric ''Addition
deriveBiGULGeneric ''Deletion
deriveBiGULGeneric ''VM
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''Reservation
deriveBiGULGeneric ''SecurityGroup
