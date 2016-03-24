{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, TypeOperators, ViewPatterns #-}

module AutoscalingBX(
  vmUpd,
  vmListAlign,
  svm,
  V.asView1,
  svm2
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
import qualified AutoScalingModel as V

vmUpd :: BiGUL S.VM V.VVM
vmUpd = $(update [p| V.VVM {
        V.vvmID = vmID,
        V.vvmType = vmType,
        V.vload = vmLoad
    }|] [p| S.VM {
            S.vmID = vmID,
            S.vmType = vmType,
            S.load = vmLoad
    }|] [d| vmID = Replace;
            vmType = Replace;
            vmLoad = Replace
    |])

vmListAlign :: BiGUL [S.VM] [V.VVM]
vmListAlign = align (const True)
  (\ s v -> S.vmID s == V.vvmID v)
  ($(update [p| v |] [p| v |] [d| v = vmUpd |]))
  (\v -> S.VM {
      S.vmID = V.vvmID v,
      S.vmType = V.vvmType v,
      S.load = V.vload v,
      S.cost = 0.00,
      S.cpu = 0,
      S.ram = 0,
      S.ami = "0000",
      S.state = 0,
      S.securityGroupRef = "sg-123"
      })
  (const Nothing)

svm :: [S.VM]
svm = [vm1, vm2]
