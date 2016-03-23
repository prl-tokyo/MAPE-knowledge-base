{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, TypeOperators, ViewPatterns #-}

module RedundancyBX(
  rvmUpd,
  rvmListAlign,
  V.rView1
  ) where

--import Generics.BiGUL
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
import AWSModel
import Utils
import qualified SourceModel as S
import qualified RedundancyModel as V --(RView(..), RVM(..))


deriveBiGULGeneric ''V.RVM
deriveBiGULGeneric ''V.RView

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

svm :: [S.VM]
svm = [vm1, vm2]

rvm :: [V.RVM]
rvm = [rvm1, rvm2, rvm3]

rvm1 = V.RVM {
  V.rvmID = "vm1",
  V.rSecurityGroupRef = "sg-123"
  }

rvm2 = V.RVM {
  V.rvmID = "vm2",
  V.rSecurityGroupRef = "sg-NEW"
  }

rvm3 = V.RVM {
  V.rvmID = "vm3",
  V.rSecurityGroupRef = "sg-456"
  }
