{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module AutoscalingFailureModel(
  Instance(..)
  , InstanceType(..)
  , AutoscalingFailure(..)
) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data AutoscalingFailure = AutoscalingFailure {
  instance :: [Instance]
  , instanceTypes :: [InstanceType]
} deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , instLoad :: Double
  , instResponseTime :: Int
  , instStatus :: Int
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
                           v .: "instLoad" <*>
                           v .: "instResponseTime" <*>
                           v .: "instStatus"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Instance where
    -- this generates a Value
    toJSON (Instance instID instType instLoad instResponseTime instStatus) =
        object ["instID" .= instID
                , "instType" .= instType
                , "instLoad" .= instLoad
                , "instResponseTime" .= instResponseTime
                , "instStatus .= instStatus"]

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
deriveBiGULGeneric ''AutoscalingFailure
