{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module ExecutionBX(
	put,
	get,
	addUpd,
	termUpd,
	executionUpd
	) where

import Generics.BiGUL.AST
import Generics.BiGUL.Error
import Generics.BiGUL.Interpreter
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Control.Monad
import Data.Char
import Data.List
import GHC.Generics
import Control.Arrow
import Data.Maybe
import Utils
import qualified SourceModel as S
import qualified ExecutionModel as V

executionUpd :: BiGUL S.Model V.View
executionUpd = addUpd

addUpd :: BiGUL S.Model V.View
addUpd = $(update [p| V.View {
                       V.terminations = terminations,
                       V.additions = additions
               }|] [p| S.Model {
                       S.instances = additions
               }|] [d| additions = instListAddAlign;
               		   terminations = Skip
                |])

termUpd :: BiGUL S.Model V.View
termUpd = $(update [p| V.View {
                       V.terminations = terminations,
                       V.additions = additions
               }|] [p| S.Model {
                       S.instances = terminations
               }|] [d| terminations = instListTermAlign;
                       additions = Skip
                |])

data Instance = Instance {
    instID :: String
    , instType :: String
    , ami :: String
    , state :: Int
    , securityGroupRef :: String}

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.ami = ami,
                       V.state = state,
                       V.securityGroupRef = securityGroupRef
               }|] [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.ami = ami,
                       S.state = state,
                       S.securityGroupRef = securityGroupRef
               }|] [d| instID = Replace;
                       instType = Replace;
                       ami = Replace;
                       state = Replace;
                       securityGroupRef = Replace
  |])

instListAddAlign :: BiGUL [S.Instance] [V.Instance]
instListAddAlign = align (\s -> S.instStatus s == 1)
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = V.instType v
      , S.instStatus = 0
      , S.ami = V.ami v
      , S.state = V.state v
      , S.load = 0.00
      , S.securityGroupRef = V.securityGroupRef v
      })
  (\s -> Nothing)

instListTermAlign :: BiGUL [S.Instance] [V.Instance]
instListTermAlign = align (\s -> S.instStatus s == 2)
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = V.instType v
      , S.instStatus = 0
      , S.ami = V.ami v
      , S.state = V.state v
      , S.load = 0.00
      , S.securityGroupRef = V.securityGroupRef v
      })
  (\s -> Nothing)