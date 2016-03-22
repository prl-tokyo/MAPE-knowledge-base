module AutoScalingModel(
  View(..)
  , VM(..)
  ) where

data View = View {
  vms :: [VM]} deriving (Show, Eq)

data VM = VM {
  vmID :: String
  , vmType :: String
  , load :: Double
  } deriving (Show, Eq)

instance Ord VM where
  compare vm1 vm2 = compare (vmID vm1) (vmID vm2)
