{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module ExecutionBX(
	put,
	get,
	getExecution,
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
import Data.Either.Extra
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

getExecution s = V.View {
	V.terminations = fromRight (getTermUpd s),
	V.additions = fromRight (getAddUpd s)
}

getTermUpd s = case s' of
	Right ps -> get instListTermAlign ps
	where s' = get sInstListUpd s

getAddUpd s = case s' of
	Right ps -> get instListAddAlign ps
	where s' = get sInstListUpd s

sInstListUpd :: BiGUL S.Model [S.Instance]
sInstListUpd = $(update [p| instances |]
	               [p| S.Model {
	                    S.instances = instances
	           }|] [d| instances = Replace
	            |])

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.ami = ami,
                       V.securityGroupRef = securityGroupRef
               }|] [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.ami = ami,
                       S.securityGroupRef = securityGroupRef
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
