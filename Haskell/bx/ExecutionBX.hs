{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module ExecutionBX (executionUpd) where

import Generics.BiGUL.AST
import Generics.BiGUL.Error
import Generics.BiGUL.Interpreter
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Control.Monad
import Data.Char
import Data.List
import GHC.Genericsx
import Control.Arrow
import Data.Maybe
import Utils
import qualified SourceModel as S
import qualified ExecutionModel as V

executionUpd :: BiGUL S.Model V.View

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
        V.instID = instID
        , V.instType = instType
        , V.ami = ami
        , V.state = state
        , V.securityGroupRef = securityGroupRef
    }|] [p| S.Instance {
        S.instID = instID
        , S.instType = instType
        , S.ami = ami
        , S.state = state
        , S.securityGroupRef = securityGroupRef
    }|] [d| instID = Replace;
            instType = Replace;
            ami = Replace;
            state = Replace;
            securityGroupRef = Replace
    |])

fwUpd :: BiGUL S.FirewallRule V.FirewallRule
fwUpd = $(update [p| V.FirewallRule {
		V.fwRuleID = fwRuleID
		, V.outbound = outbound
		, V.port = port
		, V.ip = ip
		, V.protocol = protocol
	} |] [p| S.FirewallRule {
		S.fwRuleID = fwRuleID
		, S.outbound = outbound
		, S.port = port
		, S.ip = ip
		, S.protocol = protocol
	} |] [d| fwRuleID = Replace;
		     outbound = Replace;
		     port = Replace;
		     ip = Replace;
		     protocol = Replace;
	}|])

instListAlign :: BiGUL [S.Instance] [V.Instance]
instListAlign = align (const True)
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
  (\s -> const Nothing)

fwListAlign :: BiGUL [S.FirewallRule] [V.FirewallRule]