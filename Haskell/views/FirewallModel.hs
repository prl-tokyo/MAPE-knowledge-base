{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module FirewallModel (
  Rule(..)
  , View(..)
  ) where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data View = View {
  rules :: [Rule]
  } deriving (Show, Eq)

data Rule = Rule {
  ruleID :: String
  , securityGroupRefFrom :: String
  , securityGroupRefTo :: String
  , port :: String
  , protocol :: String
  } deriving (Show, Eq)

instance FromJSON View where
    parseJSON (Object v) = View <$>
                           v .: "rules"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON View where
    -- this generates a Value
    toJSON (View rules) =
        object ["rules" .= rules]

instance FromJSON Rule where
    parseJSON (Object v) = Rule <$>
                           v .: "ruleID" <*>
                           v .: "securityGroupRefFrom" <*>
                           v .: "securityGroupRefTo" <*>
                           v .: "port" <*>
                           v .: "protocol"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Rule where
    -- this generates a Value
    toJSON (Rule ruleID securityGroupRefFrom securityGroupRefTo port protocol) =
        object ["ruleID" .= ruleID
                , "securityGroupRefFrom" .= securityGroupRefFrom
                , "securityGroupRefTo" .= securityGroupRefTo
                , "port" .= port
                , "protocol" .= protocol]

deriveBiGULGeneric ''View
deriveBiGULGeneric ''Rule
