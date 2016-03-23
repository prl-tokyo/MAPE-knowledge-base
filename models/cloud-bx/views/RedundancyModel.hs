module RedundancyModel(RView(..), RVM(..)) where

data RView = RView {rvms :: [RVM]} deriving (Show, Eq)

data RVM = RVM {
    rvmID :: String
  , rSecurityGroupRef :: String
  } deriving (Show, Eq)

instance Ord RVM where
  compare vm1 vm2 = compare (rvmID vm1) (rvmID vm2)
