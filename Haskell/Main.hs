{-# LANGUAGE TemplateHaskell
, TypeFamilies, OverloadedStrings #-}

module Main where

import System.Environment
import Generics.BiGUL.TH
import GHC.Generics
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import SourceModel
import qualified AutoScalingBX as ASBX
import qualified FirewallBX as FWBX
import qualified RedundancyBX as REDBX
import qualified ExecutionBX as EXBX
import qualified AutoScalingModel as ASV
import qualified FirewallModel as FWV
import qualified RedundancyModel as REDV
import qualified ExecutionModel as EXV
import qualified AutoscalingFailureModel as ASFV
import qualified AutoscalingFailureBX as ASFBX
import qualified FailureModel as FAV
import qualified FailureBX as FABX

doGet bx source param = case bx of
  "autoscalingFailure" -> case result of
    Right res -> encode res
    where result = (ASFBX.get (ASFBX.autoscalingFailureUpd param) source)
  "redundancy" -> case result of
    Right res -> encode res
    where result = (REDBX.get REDBX.redundancyUpd source)
  "firewall" -> case result of
    Right res -> encode res
    where result = (FWBX.get FWBX.firewallUpd source)
  "execution" -> encode (EXBX.getExecution source)

doGet' bx source = case bx of
  "failure" -> case result of
    Right res -> encode res
    where result = (FABX.get FABX.failureUpd source)
  "autoScaling" -> case result of
    Right res -> encode res
    where result = (ASBX.get ASBX.autoScalingUpd source)

doASPut source view = case result of
  Right res -> encode res
  where result = (ASBX.put ASBX.autoScalingUpd source view)

doREDPut source view = case result of
  Right res -> encode res
  where result = (REDBX.put REDBX.redundancyUpd source view)

doFWPut source view = case result of
  Right res -> encode res
  where result = (FWBX.put FWBX.firewallUpd source view)

doEXPut source view = case result of
  Right res -> encode res
  where result = (EXBX.put EXBX.executionUpd source view)

doASFPut source view param = case result of
  Right res -> encode res
  where result = (ASFBX.put (ASFBX.autoscalingFailureUpd param) source view)

doFAPut source view = case result of
  Right res -> encode res
  where result = (FABX.put FABX.failureUpd source view)

-- arguments are:
-- dir: the direction, either get or put
-- bx: the name of the transformation
-- param: a parameter to pass to the bx (not all BX need this)
-- Example: ./Main get autoScaling as.json
main :: IO ()
main = do
  [dir, bx, param] <- getArgs
  putStrLn "Starting"
  case bx of
    "autoscalingFailure" -> do
      doAutoscalingFailure dir param
    "autoScaling" -> do
      doAutoScaling dir
    "failure" -> do
      doFailure dir
    "execution" -> do
      doExecution dir
    "firewall" -> do
      doFirewall dir

doExecution dir = do
  src <- (eitherDecode <$> (B.readFile "source.json")) :: IO (Either String Model)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile "execution.json" (doGet "execution" source "")
      "put" -> do
        v <- (eitherDecode <$> (B.readFile "execution.json")) :: IO (Either String EXV.View)
        case v of
          Left err -> do
            putStrLn "Error parse execution.json"
            putStrLn err
          Right vw -> B.writeFile "source.json" (doEXPut source vw)

doAutoscalingFailure dir param = do
  src <- (eitherDecode <$> (B.readFile "source.json")) :: IO (Either String Model)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile "autoscalingFailure.json" (doGet "autoscalingFailure" source param)
      "put" -> do
        v <- (eitherDecode <$> (B.readFile "autoscalingFailure.json")) :: IO (Either String ASFV.AutoscalingFailure)
        case v of
          Left err -> do
            putStrLn "Error parse autoscalingFailure.json"
            putStrLn err
          Right vw -> B.writeFile "source.json" (doASFPut source vw param)

doAutoScaling dir = do
  src <- (eitherDecode <$> (B.readFile "autoscalingFailure.json")) :: IO (Either String ASFV.AutoscalingFailure)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile "autoScaling.json" (doGet' "autoScaling" source)
      "put" -> do
        v <- (eitherDecode <$> (B.readFile "autoScaling.json")) :: IO (Either String ASV.View)
        case v of
          Left err -> do
            putStrLn "Error parse autoScaling.json"
            putStrLn err
          Right vw -> B.writeFile "autoscalingFailure.json" (doASPut source vw)

doFailure dir = do
  src <- (eitherDecode <$> (B.readFile "autoscalingFailure.json")) :: IO (Either String ASFV.AutoscalingFailure)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile "failure.json" (doGet' "failure" source)
      "put" -> do
        v <- (eitherDecode <$> (B.readFile "failure.json")) :: IO (Either String FAV.FailureView)
        case v of
          Left err -> do
            putStrLn "Error parse failure.json"
            putStrLn err
          Right vw -> B.writeFile "autoscalingFailure.json" (doFAPut source vw)

doFirewall dir = do
  src <- (eitherDecode <$> (B.readFile "source.json")) :: IO (Either String Model)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile "firewall.json" (doGet "firewall" source "")
      "put" -> do
        v <- (eitherDecode <$> (B.readFile "firewall.json")) :: IO (Either String FWV.View)
        case v of
          Left err -> do
            putStrLn "Error parse firewall.json"
            putStrLn err
          Right vw -> B.writeFile "source.json" (doFWPut source vw)


{-
Zhenjiang: the following two functions can be used to generate the Haskell
representations from the JSON representation of soruce/view for simple
testing of FirewallBX.hs.
-}

jsonSource2Haskell :: IO ()
jsonSource2Haskell = do
  src <- (eitherDecode <$> B.readFile "source.json") :: IO (Either String Model)
  case src of
    Right s -> putStrLn (show s)
    Left _  -> putStrLn "JSON parse error (source)"

jsonFirewallView2Haskell :: IO ()
jsonFirewallView2Haskell = do
  view <- (eitherDecode <$> B.readFile "firewall.json") :: IO (Either String FWV.View)
  case view of
    Right v -> putStrLn (show v)
    Left _  -> putStrLn "JSON parse error (firewall)"
