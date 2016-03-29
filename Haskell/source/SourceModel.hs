{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

-- This model defines the structure of the source model. The root is `Model`.
-- It is assumed that the put behaviour updating this model will not directly
-- modify model elements, but add them to the `Addition` or `Deletion` lists,
-- as appropriate.

module SourceModel(
  Model(..)
  , Instance(..)
  , SecurityGroup(..)
  , FirewallRule(..)
  , InstanceType(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics

data Model = Model {
  instances :: [Instance]
  , securityGroups :: [SecurityGroup]
  , instanceTypes :: [InstanceType]
  } deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , ami :: String
  , state :: Int
  , instStatus :: Int
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
  outbound :: Bool
  , port :: String
  , ip :: String
  , protocol :: String
  , fwStatus :: Int
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
deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''SecurityGroup
deriveBiGULGeneric ''InstanceType
