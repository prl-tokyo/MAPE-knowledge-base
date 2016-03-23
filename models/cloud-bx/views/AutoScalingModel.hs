module AutoScalingModel(
  View(..)
  , VVM(..)
  ) where

data View = View {
  vvms :: [VVM]} deriving (Show, Eq)

data VVM = VVM {
  vvmID :: String
  , vvmType :: String
  , vload :: Double
  } deriving (Show, Eq)

instance Ord VVM where
  compare vm1 vm2 = compare (vvmID vm1) (vvmID vm2)
