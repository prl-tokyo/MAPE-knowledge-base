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

deriveBiGULGeneric ''V.VVM
deriveBiGULGeneric ''V.View

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

-- svm3 :: [VM]
-- svm3 = [vm1, vm2, vm3]

-- vvm :: [VirtualMachineLoad]
-- vvm = [vmview1, vmview2]

-- vvmDelete :: [VirtualMachineLoad]
-- vvmDelete = [vmview1]

-- vvmNoOrder :: [VirtualMachineLoad]
-- vvmNoOrder = [vmview1, vmview3, vmview2]

-- vvmAdd :: [VirtualMachineLoad]
-- vvmAdd = [vmview1, vmview2, vmview3]

-- vmDefault = VirtualMachine {
-- svmID = "0000",
-- svmType = "0000",
-- svmLoad = 0.00,
-- svmCost = 0.00,
-- svmCPU = 0,
-- svmRAM = 0,
-- svmAMI = "0000",
-- svmState = 0,
-- svmSecurityGroupRef = "0000",
-- svmFirewallRules = []
-- }

-- vmview1 = VirtualMachineLoad {
-- vvmID = "vm1",
-- vvmType = "t2.large",
-- vvmLoad = 1.12
-- }

-- vmview2 = VirtualMachineLoad {
-- vvmID = "vm2",
-- vvmType = "t2.xxlarge",
-- vvmLoad = 0.42
-- }

-- vmview3 = VirtualMachineLoad {
-- vvmID = "vm3",
-- vvmType = "t4.xlarge",
-- vvmLoad = 10.22
-- }
