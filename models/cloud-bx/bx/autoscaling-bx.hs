{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, TypeOperators, ViewPatterns #-}

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
import qualified SourceModel as S
import qualified AutoScalingModel as V (View(..), VVM(..))

deriveBiGULGeneric ''S.Model
deriveBiGULGeneric ''S.VM
deriveBiGULGeneric ''S.FirewallRule
deriveBiGULGeneric ''S.Reservation
deriveBiGULGeneric ''S.SecurityGroup
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

align :: (a -> Bool)
      -> (a -> b -> Bool)
      -> BiGUL a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> BiGUL [a] [b]
align p match b create conceal = Case
  [ $(normalSV [| null . filter p |] [p| [] |])$
      $(rearrV [| \[] -> () |])$ Skip
  , $(adaptiveV [p| [] |])$
      \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)
  -- view is necessarily nonempty in the cases below
  , $(normalS [p| (p -> False):_ |])$
      $(rearrS [| \(s:ss) -> ss |])$
        align p match b create conceal
  , $(normal' [| \ss vs -> not (null ss) && p (head ss) && match (head ss) (head vs) |]
              [| \ss    -> not (null ss) && p (head ss) |])$
      $(rearrV [| \(v:vs) -> (v, vs) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` align p match b create conceal
  , $(adaptive [| \ss (v:_) -> isJust (findFirst (\s -> p s && match s v) ss) ||
                               let s = create v in p s && match s v |])$
      \ss (v:_) -> maybe (create v:ss) (uncurry (:)) (findFirst (\s -> p s && match s v) ss)
  ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findFirst p [] = Nothing
    findFirst p (x:xs) | p x       = Just (x, xs)
    findFirst p (x:xs) | otherwise = fmap (id *** (x:)) (findFirst p xs)
