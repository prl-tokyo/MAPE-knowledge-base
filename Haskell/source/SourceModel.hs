{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

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
  , Root(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data Root = Root {
  model :: Model
} deriving (Show, Eq)

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
  fwRuleID :: String
  , outbound :: Bool
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

instance FromJSON InstanceType where
    parseJSON (Object v) = InstanceType <$>
                           v .: "typeID" <*>
                           v .: "typeCPUs" <*>
                           v .: "typeRAM" <*>
                           v .: "typeCost"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON InstanceType where
    -- this generates a Value
    toJSON (InstanceType typeID typeCPUs typeRAM typeCost) =
        object ["typeID" .= typeID
                , "typeCPUs" .= typeCPUs
                , "typeRAM" .= typeRAM
                , "typeCost" .= typeCost]

instance FromJSON FirewallRule where
    parseJSON (Object v) = FirewallRule <$>
                           v .: "fwRuleID" <*>
                           v .: "outbound" <*>
                           v .: "port" <*>
                           v .: "ip" <*>
                           v .: "protocol" <*>
                           v .: "fwStatus"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON FirewallRule where
    -- this generates a Value
    toJSON (FirewallRule fwRuleID outbound port ip protocol fwStatus) =
        object ["fwRuleID" .= fwRuleID
                , "outbound" .= outbound
                , "port" .= port
                , "ip" .= ip
                , "protocol" .= protocol
                , "fwStatus" .= fwStatus]

instance FromJSON SecurityGroup where
    parseJSON (Object v) = SecurityGroup <$>
                           v .: "sgID" <*>
                           v .: "instRefs" <*>
                           v .: "firewallRules"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON SecurityGroup where
    -- this generates a Value
    toJSON (SecurityGroup sgID instRefs firewallRules) =
        object ["sgID" .= sgID
                , "instRefs" .= instRefs
                , "firewallRules" .= firewallRules]

instance FromJSON Instance where
    parseJSON (Object v) = Instance <$>
                           v .: "instID" <*>
                           v .: "instType" <*>
                           v .: "ami" <*>
                           v .: "state" <*>
                           v .: "instStatus" <*>
                           v .: "securityGroupRef" <*>
                           v .: "load"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Instance where
    -- this generates a Value
    toJSON (Instance instID instType ami state instStatus securityGroupRef load) =
        object ["instID" .= instID
                , "instType" .= instType
                , "ami" .= ami
                , "state" .= state
                , "instStatus" .= instStatus
                , "securityGroupRef" .= securityGroupRef
                , "load" .= load]

instance FromJSON Model where
    parseJSON (Object v) = Model <$>
                           v .: "instances" <*>
                           v .: "securityGroups" <*>
                           v .: "instanceTypes"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Model where
    -- this generates a Value
    toJSON (Model instances securityGroups instanceTypes) =
        object ["instances" .= instances
                , "securityGroups" .= securityGroups
                , "instanceTypes" .= instanceTypes]

instance FromJSON Root where
    parseJSON (Object v) = Root <$>
                           v .: "model"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Root where
    -- this generates a Value
    toJSON (Root model) =
        object ["model" .= model]

deriveBiGULGeneric ''Model
deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''SecurityGroup
deriveBiGULGeneric ''InstanceType
deriveBiGULGeneric ''Root

