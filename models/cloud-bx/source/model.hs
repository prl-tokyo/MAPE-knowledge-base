{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, TemplateHaskell #-}

module SourceModel (Model, Reservation, VM, SecurityGroup, FirewallRule)
       where

data Model = Model {
    reservations :: [Reservation]
  , securityGroups :: [SecurityGroup]
  } deriving (Show)

data Reservation = Reservation {
    resID :: String
  , securityGroupRefs :: [String]
  , vms :: [VM]
  } deriving (Show)

data VM = VM {
    vmID :: String
  , vmType :: String
  , cost :: Double
  , cpu :: Int
  , ram :: Int
  , ami :: String
  , state :: Int
  , securityGroupRef :: String
  , load :: Double
} deriving (Show)

data SecurityGroup = SecurityGroup {
    sgID :: String
  , vmRefs :: [String]
  , firewallRules :: [FirewallRule]
  } deriving (Show)

data FirewallRule = FirewallRule {
    fwID :: String
  , outbound :: Bool
  , port :: String
  , ip :: String
  , protocol :: Int
  } deriving (Show)
