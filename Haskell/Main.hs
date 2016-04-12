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
import qualified AutoScalingModel as ASV
import qualified FirewallModel as FWV
import qualified RedundancyModel as REDV

sourceFile :: FilePath
sourceFile = "source.json"

getJSON :: IO B.ByteString
getJSON = B.readFile sourceFile

doGet bx source = case bx of
  "autoScaling" -> case result of
    Right res -> encode res
    where result = (ASBX.get ASBX.autoScalingUpd source)
  "redundancy" -> case result of
    Right res -> encode res
    where result = (REDBX.get REDBX.redundancyUpd source)

doASPut source view = case result of
  Right res -> encode res
  where result = (ASBX.put ASBX.autoScalingUpd source view)

doREDPut source view = case result of
  Right res -> encode res
  where result = (REDBX.put REDBX.redundancyUpd source view)

-- arguments are:
-- dir: the direction, either get or put
-- bx: the name of the transformation
-- view: the filename of the view, expected to be in JSON
main :: IO ()
main = do 
  [dir, bx, view] <- getArgs
  src <- (eitherDecode <$> getJSON) :: IO (Either String Model)
  case src of
    Left err -> putStrLn err
    Right source -> case dir of
      "get" -> do
        B.writeFile view (doGet bx source)
        putStrLn "Done"
      "put" -> do
        case bx of
          "autoScaling" -> do
            v <- (eitherDecode <$> getJSON) :: IO (Either String ASV.View)
            case v of
              Left err -> putStrLn err
              Right vw -> B.writeFile sourceFile (doASPut source vw)
          "redundancy" -> do
            v <- (eitherDecode <$> getJSON) :: IO (Either String REDV.View)
            case v of
              Left err -> putStrLn err
              Right vw -> B.writeFile sourceFile (doREDPut source vw)

