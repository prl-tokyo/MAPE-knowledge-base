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
import qualified RedundancyModel as V --(RView(..), RVM(..))

deriveBiGULGeneric ''S.Model
deriveBiGULGeneric ''S.VM
deriveBiGULGeneric ''S.FirewallRule
deriveBiGULGeneric ''S.Reservation
deriveBiGULGeneric ''S.SecurityGroup
deriveBiGULGeneric ''V.RVM
deriveBiGULGeneric ''V.RView

main :: IO ()
main = do
  putStrLn "Hello, world!"

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
