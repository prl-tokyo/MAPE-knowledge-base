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
  , Instance(..)
  , SecurityGroup(..)
  , FirewallRule(..)
  , InstanceType(..)
  , Static(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data Model = Model {
  current :: Current
  , additions :: [Addition]
  , deletions :: [Deletion]
  , static :: Static
  } deriving (Show, Eq)

data Current = Current {
  reservations :: [Reservation]
  , securityGroups :: [SecurityGroup]
  } deriving (Show, Eq)

data Addition = Addition {
  addFW :: [FirewallRule]
  , addInst :: [Instance]
  } deriving (Show, Eq)

data Deletion = Deletion {
  delFW :: [FirewallRule]
  , delInst :: [Instance]
  } deriving (Show, Eq)

data Reservation = Reservation {
  resID :: String
  , securityGroupRefs :: [String]
  , instances :: [Instance]
  } deriving (Show, Eq)

instance Ord Reservation where
  compare res1 res2 = compare (resID res1) (resID res2)

data Instance = Instance {
  instID :: String
  , instType :: String
  , ami :: String
  , state :: Int
  , securityGroupRef :: String
  , load :: Double
} deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

data SecurityGroup = SecurityGroup {
  sgID :: String
  , instRefs :: [String]
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

data Static = Static {
  instanceTypes :: [InstanceType]
  } deriving (Show, Eq)

data InstanceType = InstanceType {
  typeID :: String
  , typeCPUs :: Int
  , typeRAM :: Double
  , typeCost :: Double
  } deriving (Show, Eq)

instance Ord InstanceType where
  compare inst1 inst2 = compare (typeID inst1) (typeID inst2)

deriveBiGULGeneric ''Model
deriveBiGULGeneric ''Current
deriveBiGULGeneric ''Addition
deriveBiGULGeneric ''Deletion
deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''Reservation
deriveBiGULGeneric ''SecurityGroup
deriveBiGULGeneric ''InstanceType
deriveBiGULGeneric ''Static
