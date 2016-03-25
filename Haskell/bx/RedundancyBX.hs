{-# LANGUAGE TemplateHaskell,
TypeFamilies #-}

module RedundancyBX(
  rvmUpd,
  rvmListAlign,
  get,
  put
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
import qualified RedundancyModel as V

rvmUpd :: BiGUL S.VM V.RVM
rvmUpd = $(update [p| V.RVM {
        V.rvmID = vmID,
        V.rSecurityGroupRef = securityGroupRef
    }|] [p| S.VM {
        S.vmID = vmID,
        S.securityGroupRef = securityGroupRef
    }|] [d| vmID = Replace;
            securityGroupRef = Replace
    |])

rvmListAlign :: BiGUL [S.VM] [V.RVM]
rvmListAlign = align (const True)
  (\ s v -> S.vmID s == V.rvmID v)
  ($(update [p| v |] [p| v |] [d| v = rvmUpd |]))
  (\v -> S.VM {
      S.vmID = V.rvmID v,
      S.vmType = "t2.micro",
      S.load = 0.00,
      S.cost = 0.00,
      S.cpu = 0,
      S.ram = 0,
      S.ami = "0000",
      S.state = 0,
      S.securityGroupRef = V.rSecurityGroupRef v
      })
  (const Nothing)
