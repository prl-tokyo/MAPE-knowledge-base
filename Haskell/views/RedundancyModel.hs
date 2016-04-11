{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module RedundancyModel (
  Instance(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data View = View {instances :: [Instance]} deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , securityGroupRef :: String
  , status :: Int
  } deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

instance FromJSON View where
    parseJSON (Object v) = View <$>
                           v .: "instances"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON View where
    -- this generates a Value
    toJSON (View instances) =
        object ["instances" .= instances]

instance FromJSON Instance where
    parseJSON (Object v) = Instance <$>
                           v .: "instID" <*>
                           v .: "securityGroupRef" <*>
                           v .: "status"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Instance where
    -- this generates a Value
    toJSON (Instance instID securityGroupRef status) =
        object ["instID" .= instID
                , "securityGroupRef" .= securityGroupRef
                , "status" .= status]

deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''View

