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

--sourceFile :: FilePath
--sourceFile = "source.json"

getJSON :: IO B.ByteString
getJSON sourceFile = B.readFile sourceFile

doGet bx source param = case bx of
  "autoscalingFailure" -> case result of
    Right res -> encode res
    where result = (ASFBX.get (ASFBX.autoscalingFailureUpd param) source)
  "failure" -> case result of
    Right res -> encode res
    where result = (FABX.get FABX.failureUpd source)
  "autoScaling" -> case result of
    Right res -> encode res
    where result = (ASBX.get ASBX.autoScalingUpd source)
  "redundancy" -> case result of
    Right res -> encode res
    where result = (REDBX.get REDBX.redundancyUpd source)
  "firewall" -> case result of
    Right res -> encode res
    where result = (FWBX.get FWBX.firewallUpd source)
  "execution" -> encode (EXBX.getExecution source)

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
-- view: the filename of the view, expected to be in JSON
-- param: a parameter to pass to the bx (not all BX need this)
-- Example: ./Main get autoScaling as.json
main :: IO ()
main = do
  [dir, bx, sourceFile, view, param] <- getArgs
  putStrLn "Start"
  src <- (eitherDecode <$> getJSON sourceFile) :: IO (Either String Model)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile view (doGet bx source param)
        putStrLn "Done"
      "put" -> do
        putStrLn "put"
        case bx of
          "autoscalingFailure" -> do
            putStrLn "autoscalingFailure: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String ASFV.AutoscalingFailure)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> B.writeFile sourceFile (doASFPut source vw param)
          "autoScaling" -> do
            putStrLn "autoscaling: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String ASV.View)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> B.writeFile sourceFile (doASPut source vw)
          "failure" -> do
            putStrLn "failure: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String FAV.FailureView)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> B.writeFile sourceFile (doFAPut source vw)
          "redundancy" -> do
            putStrLn "redundancy: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String REDV.View)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> do
                putStrLn "redundancy: running put transformation and writing file"
                B.writeFile sourceFile (doREDPut source vw)
          "firewall" -> do
            putStrLn "firewall: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String FWV.View)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> do
                putStrLn "firewall: running put transformation and writing file"
                B.writeFile sourceFile (doFWPut source vw)
          "execution" -> do
            putStrLn "execution: reading JSON file"
            v <- (eitherDecode <$> (B.readFile view)) :: IO (Either String EXV.View)
            case v of
              Left err -> do
                putStrLn "JSON parse error"
                putStrLn err
              Right vw -> B.writeFile sourceFile (doEXPut source vw)


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
    Left _  -> putStrLn "wrong in passing JSON"

jsonFirewallView2Haskell :: IO ()
jsonFirewallView2Haskell = do
  view <- (eitherDecode <$> B.readFile "firewall.json") :: IO (Either String FWV.View)
  case view of
    Right v -> putStrLn (show v)
    Left _  -> putStrLn "wrong in passing JSON"
