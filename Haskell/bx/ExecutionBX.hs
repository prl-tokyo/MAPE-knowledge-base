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

instListAlign :: BiGUL [S.Instance] [V.Instance]

fwListAlign :: BiGUL [S.FirewallRule] [V.FirewallRule]