{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

-- This model defines the structure of the source model. The root is `Model`.
-- It is assumed that the put behaviour updating this model will not directly
-- modify model elements, but add them to the `Addition` or `Deletion` lists,
-- as appropriate.

module SourceModel(
  Model(..)
  , Current(..)
  , Addition(..)
  , Deletion(..)
  , Reservation(..)
  , VM(..)
  , SecurityGroup(..)
  , FirewallRule(..)
  , InstanceType(..)
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
  , instanceTypes :: [InstanceType]
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

data InstanceType = InstanceType {
  typeID :: String
  , typeCPUs :: Int
  , typeRAM :: Int
  , typeCost :: Double
  } deriving (Show, Eq)

instance Ord InstanceType where
  compare inst1 inst2 = compare (typeID inst1) (typeID inst2)

deriveBiGULGeneric ''Model
deriveBiGULGeneric ''Current
deriveBiGULGeneric ''Addition
deriveBiGULGeneric ''Deletion
deriveBiGULGeneric ''VM
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''Reservation
deriveBiGULGeneric ''SecurityGroup
deriveBiGULGeneric ''InstanceType
