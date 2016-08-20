{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module FailureModel(
  Instance(..)
  , FailureView(..)
) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data FailureView = FailureView {
  instances :: [Instance]
} deriving (Show, Eq)

data Instance = Instance {
  instID :: String
  , instType :: String
  , instResponseTime :: Int
} deriving (Show, Eq)

instance Ord Instance where
  compare inst1 inst2 = compare (instID inst1) (instID inst2)

instance FromJSON FailureView where
  parseJSON (Object v) = FailureView <$>
                         v .: "instances"
  parseJSON _          = mempty

instance ToJSON FailureView where
  toJSON(FailureView instances) =
    object["instances" .= instance]

instance FromJSON Instance where
  parseJSON (Object v) = Instance <$>
                         v .: "instID" <*>
                         v .: "instType" <*>
                         v .: "instResponseTime"
  parseJSON _          = mempty

instance ToJSON Instance where
  toJSON (Instance instID intType instResponseTime) =
    object ["instID" .= instID
            , "instType" .= instType
            , "instResponseTime" .= instResponseTime]

deriveBiGULGeneric ''Instance
deriveBiGULGeneric ''FailureView
