{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module AutoScalingModel(
  Instance(..)
  , InstanceType(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data View = View {
  instances :: [Instance]
  , instanceTypes :: [InstanceType]
  } deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , instLoad :: Double
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

data InstanceType = InstanceType {
  typeID :: String
  , typeCPUs :: Int
  , typeRAM :: Double
  , typeCost :: Double
  } deriving (Show, Eq)

instance Ord InstanceType where
  compare inst1 inst2 = compare (typeID inst1) (typeID inst2)

instance FromJSON View where
    parseJSON (Object v) = View <$>
                           v .: "instances" <*>
                           v .: "instanceTypes" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON View where
    -- this generates a Value
    toJSON (View instances instanceTypes) =
        object ["instances" .= instances
                , "instanceTypes" .= instanceTypes]

instance FromJSON Instance where
    parseJSON (Object v) = Instance <$>
                           v .: "instID" <*>
                           v .: "instType" <*>
                           v .: "instLoad"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Instance where
    -- this generates a Value
    toJSON (Instance instID instType instLoad) =
        object ["instID" .= instID
                , "instType" .= instType
                , "instLoad" .= instLoad]

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

deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''InstanceType
deriveBiGULGeneric ''View
