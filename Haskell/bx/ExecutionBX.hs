{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}

module ExecutionBX (
  put,
  get,
  getExecution,
  executionUpd
) where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH 
import Generics.BiGUL.Lib

import Control.Monad
import Data.Char
import Data.List
import GHC.Generics
import Control.Arrow
import Data.Maybe
import Data.Either.Extra
import Utils
import qualified SourceModel as S
import qualified ExecutionModel as V

executionUpd :: BiGUL S.Model V.View
executionUpd = addUpd

addUpd :: BiGUL S.Model V.View
addUpd = $(update [p| S.Model {
                       S.instances = additions
               }|] [p| V.View {
                       V.terminations = [],
                       V.additions = additions
               }|] [d| additions = instListAddAlign;
                |])

getExecution s = V.View {
  V.terminations = fromJust (getTermUpd s),
  V.additions = fromJust (getAddUpd s)
}

getTermUpd s = case s' of
  Just ps -> get instListTermAlign ps
  where s' = get sInstListUpd s

getAddUpd s = case s' of
  Just ps -> get instListAddAlign ps
  where s' = get sInstListUpd s

sInstListUpd :: BiGUL S.Model [S.Instance]
sInstListUpd = $(update [p| S.Model {
                            S.instances = instances
                            }|]
                        [p| instances |]
                        [d| instances = Replace |])

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.ami = ami,
                       S.securityGroupRef = securityGroupRef
               }|] [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.ami = ami,
                       V.securityGroupRef = securityGroupRef
               }|] [d| instID = Replace;
                       instType = Replace;
                       ami = Replace;
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
      , S.state = 0
      , S.instResponseTime = -1
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
      , S.state = 0
      , S.instResponseTime = -1
      , S.load = 0.00
      , S.securityGroupRef = V.securityGroupRef v
      })
  (\s -> Nothing)
