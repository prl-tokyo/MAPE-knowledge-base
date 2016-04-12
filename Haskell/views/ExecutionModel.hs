{-# LANGUAGE TemplateHaskell
, TypeFamilies
, OverloadedStrings #-}

module ExecutionModel where

import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson

data View = View {
    additions :: Changes
    , terminations :: Changes
} deriving (Show, Eq)

data Changes = Changes {
    instances :: [Instance]
} deriving (Show, Eq)

data Instance = Instance {
    instID :: String
    , instType :: String
    , ami :: String
    , state :: Int
    , securityGroupRef :: String
} deriving (Show, Eq)

instance Ord Instance where
    compare inst1 inst2 = compare (instID inst1) (instID inst2)

instance FromJSON Instance where
    parseJSON (Object v) = Instance <$>
                           v .: "instID" <*>
                           v .: "instType" <*>
                           v .: "ami" <*>
                           v .: "state" <*>
                           v .: "securityGroupRef"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON Instance where
    -- this generates a Value
    toJSON (Instance instID instType ami state securityGroupRef) =
        object ["instID" .= instID
                , "instType" .= instType
                , "ami" .= ami
                , "state" .= state
                , "securityGroupRef" .= securityGroupRef]

data FirewallRule = FirewallRule {
    fwRuleID :: String
    , outbound :: Bool
    , port :: String
    , ip :: String
    , protocol :: String
} deriving (Show, Eq)

instance Ord FirewallRule where
    compare fw1 fw2 = compare (fwRuleID fw1) (fwRuleID fw2)

instance FromJSON FirewallRule where
    parseJSON (Object v) = FirewallRule <$>
                           v .: "fwRuleID" <*>
                           v .: "outbound" <*>
                           v .: "port" <*>
                           v .: "ip" <*>
                           v .: "protocol"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance ToJSON FirewallRule where
    -- this generates a Value
    toJSON (FirewallRule fwRuleID outbound port ip protocol) =
        object ["fwRuleID" .= fwRuleID
                , "outbound" .= outbound
                , "port" .= port
                , "ip" .= ip
                , "protocol" .= protocol]