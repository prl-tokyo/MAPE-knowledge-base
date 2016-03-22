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

data Autoscaling = Autoscaling {
vvms :: [VirtualMachineLoad]
} deriving (Show, Eq)

data VirtualMachineLoad = VirtualMachineLoad {
vvmID :: String,
vvmType :: String,
vvmLoad :: Double
} deriving (Show, Eq)

data VirtualMachine = VirtualMachine {
svmID :: String,
svmType :: String,
svmLoad :: Double,
svmCost :: Double,
svmCPU :: Int,
svmRAM :: Int,
svmAMI :: String,
svmState :: Int,
svmSecurityGroupRef :: String,
svmFirewallRules :: [FirewallRule]
} deriving (Show, Eq)

data FirewallRule = FirewallRule {
fwOutbound :: Bool,
fwIP :: String,
fwPort :: String,
fwProtocol :: String
} deriving (Show, Eq)

data Model = Model {
mVMs :: [VirtualMachine],
mSecurityGroups :: [SecurityGroup]
} deriving (Show, Eq)

data SecurityGroup = SecurityGroup {
sgID :: String,
sgVMs :: [String],
sgFirewallRules :: [String]
} deriving (Show, Eq)


deriveBiGULGeneric ''VirtualMachine
deriveBiGULGeneric ''VirtualMachineLoad
deriveBiGULGeneric ''FirewallRule
deriveBiGULGeneric ''Model
deriveBiGULGeneric ''SecurityGroup

vmUpd :: BiGUL VirtualMachine VirtualMachineLoad
vmUpd = $(update [p| VirtualMachineLoad {
        vvmID = vmID,
        vvmType = vmType,
        vvmLoad = vmLoad
    }|] [p| VirtualMachine {
            svmID = vmID,
            svmType = vmType,
            svmLoad = vmLoad
    }|] [d| vmID = Replace;
            vmType = Replace;
            vmLoad = Replace
    |])

vmListAlign :: BiGUL [VirtualMachine] [VirtualMachineLoad]
vmListAlign = align (const True)
  (\ s v -> svmID s == vvmID v)
  ($(update [p| v |] [p| v |] [d| v = vmUpd |]))
  (\v -> VirtualMachine {
      svmID = vvmID v,
      svmType = vvmType v,
      svmLoad = vvmLoad v,
      svmCost = 0.00,
      svmCPU = 0,
      svmRAM = 0,
      svmAMI = "0000",
      svmState = 0,
      svmSecurityGroupRef = "sg-123",
      svmFirewallRules = []
      })
  (const Nothing)

instance Ord VirtualMachineLoad where
  compare vm1 vm2 = compare (vvmID vm1) (vvmID vm2)

instance Ord VirtualMachine where
  compare vm1 vm2 = compare (svmID vm1) (svmID vm2)

svm :: [VirtualMachine]
svm = [vm1, vm2]

svm3 :: [VirtualMachine]
svm3 = [vm1, vm2, vm3]

vvm :: [VirtualMachineLoad]
vvm = [vmview1, vmview2]

vvmDelete :: [VirtualMachineLoad]
vvmDelete = [vmview1]

vvmNoOrder :: [VirtualMachineLoad]
vvmNoOrder = [vmview1, vmview3, vmview2]

vvmAdd :: [VirtualMachineLoad]
vvmAdd = [vmview1, vmview2, vmview3]

vmDefault = VirtualMachine {
svmID = "0000",
svmType = "0000",
svmLoad = 0.00,
svmCost = 0.00,
svmCPU = 0,
svmRAM = 0,
svmAMI = "0000",
svmState = 0,
svmSecurityGroupRef = "0000",
svmFirewallRules = []
}


vm1 = VirtualMachine {
svmID = "vm1",
svmType = "t2.micro",
svmLoad = 1.12,
svmCost = 0.02,
svmCPU = 4,
svmRAM = 8,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

vm2 = VirtualMachine {
svmID = "vm2",
svmType = "t2.micro",
svmLoad = 0.42,
svmCost = 0.01,
svmCPU = 2,
svmRAM = 4,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

vm3 = VirtualMachine {
svmID = "vm3",
svmType = "c2.4xlarge",
svmLoad = 10.22,
svmCost = 0.50,
svmCPU = 16,
svmRAM = 64,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

vmview1 = VirtualMachineLoad {
vvmID = "vm1",
vvmType = "t2.large",
vvmLoad = 1.12
}

vmview2 = VirtualMachineLoad {
vvmID = "vm2",
vvmType = "t2.xxlarge",
vvmLoad = 0.42
}

vmview3 = VirtualMachineLoad {
vvmID = "vm3",
vvmType = "t4.xlarge",
vvmLoad = 10.22
}

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
