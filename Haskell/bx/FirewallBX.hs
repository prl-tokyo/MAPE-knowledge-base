{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FirewallBX(
  ruleUpd
  , get
  , put
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
import qualified FirewallModel as V

ruleUpd :: BiGUL (String, S.FirewallRule) V.Rule
ruleUpd = $(update [p|  V.Rule {
	                   	V.ruleID = ruleID
						, V.securityGroupRefFrom = from
						, V.securityGroupRefTo = to
						, V.port = port
						, V.protocol = protocol
						}
	            |] [p|  (to
	            		, S.FirewallRule {
	            		S.ip = from
	            		, S.port = port
	            		, S.fwRuleID = ruleID
	            		, S.protocol = protocol
	            		})
	            |] [d|  ruleID = Replace;
	            		from = Replace;
	            		to = Replace;
	            		port = Replace;
	            		protocol = Replace
	            |])

