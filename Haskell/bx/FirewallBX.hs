{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

-- Under the directory Haskell:
--   > ghci -ibx:models:source:views

module FirewallBX(
--  dupAndZip
    ruleListUpd
  , ruleUpd
  , get
  , put
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
import qualified FirewallModel as V

-- firewallUpd :: BiGUL S.Model V.View
-- firewallUpd =

-- Dropping unnecessary data from the source
--sourceToList :: BiGUL S.Model [(String, [S.FirewallRule])]
--sourceToList = $(update [p|
-- |][p|
-- |][d|
-- |])

-- In a way this is a form of flattening. We want to duplicate the String into
-- however many elements there are in the paired list. dupAndZip does it for
-- a single pair in the source, but it needs to be done for a list of pairs.
-- dupAndZipList :: BiGUL [(String, [S.FirewallRule])] [(String, S.FirewallRule)]
-- dupAndZipList =

{-

    [(100,[1,2,3]),(200,[4,5])] s

  <-> upListWithGroup
    [(100,1),(100,2),(100,3),(100,0),(200,4),(200,5),(200,0)]
  <-> upFilterSep

    [(100,1),(100,2),(100,3),(200,4),(200,5)] v

-}

sourceToSGList :: BiGUL S.Model [S.SecurityGroup]
sourceToSGList = $(update [p| sgs
                        |][p| S.Model {
                              S.securityGroups = sgs
                       }|][d| sgs = Replace;
                        |])

sgListToTuplesList :: BiGUL [S.SecurityGroup] [(String, [S.FirewallRule])]
sgListToTuplesList = align (const True)
  (\s v -> S.sgID s == fst v)
  ($(update [p| v |] [p| v |] [d| v = sgToTuple |]))
  (\v -> S.SecurityGroup {
    S.sgID = fst v,
    S.instRefs = [],
    S.firewallRules = snd v
  })
  (\s -> Nothing)

sgToTuple :: BiGUL S.SecurityGroup (String, [S.FirewallRule])
sgToTuple = $(update [p| (id, rules)
                  |] [p| S.SecurityGroup {
                         S.sgID = id,
                         S.firewallRules = rules
                 }|] [d| id = Replace;
                         rules = Replace;
                 |])

emptyFirewallRule = S.FirewallRule {
  S.fwRuleID = "SEP"
  , S.outbound = True
  , S.port = "80"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
}

flatten :: BiGUL [(String, [S.FirewallRule])] [(String, S.FirewallRule)]
flatten = dupAndZipList emptyFirewallRule

dupAndZipList :: (Eq k, Eq c) => c -> BiGUL [(k,[c])] [(k,c)]
dupAndZipList c0 = emb g p
  where
    g s = get' (upFilterSep c0) $ put' (upListWithGroup c0) [] s
    p s v = get' (upListWithGroup c0) $
              put' (upFilterSep c0) (put' (upListWithGroup c0) [] s) v

    get' bx s = case get bx s of Right v -> v
    put' bx s v = case put bx s v of Right s -> s

{-

*FirewallBX> get (dupAndZipList 0) [(100,[1,2,3]),(200,[4,5])]
Right [(100,1),(100,2),(100,3),(200,4),(200,5)]
*FirewallBX> put (dupAndZipList 0) [(100,[1,2,3]),(200,[4,5])] [(100,1),(100,2),(100,3),(200,4),(200,5)]
Right [(100,[1,2,3]),(200,[4,5])]
*FirewallBX> put (dupAndZipList 0) [(100,[1,2,3]),(200,[4,5])] [(100,1),(100,3),(200,4),(200,5)]
Right [(100,[1,3]),(200,[4,5])]
*FirewallBX> put (dupAndZipList 0) [(100,[1,2,3]),(200,[4,5])] [(100,1),(100,3),(200,4)]
Right [(100,[1,3]),(200,[4])]
*FirewallBX> put (dupAndZipList 0) [(100,[1,2,3]),(200,[4,5])] [(100,1),(100,3),(200,4),(300,5)]
Right [(100,[1,3]),(200,[4]),(300,[5])]

-}

type ListWithSep k c = [(k,c)]

{-

upFilterSep:

    [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)]
 <->
    [(100,1),(100,2),(100,3),(200,5),(200,6)]

** We assume that new group can only be added from the end.

-}

upFilterSep :: (Eq k, Eq c) => c -> BiGUL (ListWithSep k c) [(k,c)]
upFilterSep c0 =
  Case [
    $(normalSV [p| [] |] [p| [] |]) $ Replace,

    $(adaptive [| \s v -> length s == 0 && length v /= 0 |]) $
      \[] ((k,c):v) -> [(k,c),(k,c0)],

    $(normal [| \((k,c):s) v -> length v == 0 && c==c0 |]) $
      $(rearrS [| \(a:s) -> (a,s) |]) $
         $(rearrV [| \v -> ((),v) |]) $
            Prod Skip (upFilterSep c0),

    $(adaptive [| \((k,c):s) v -> length v == 0 && c/=c0 |]) $
      \(a:s) v -> s,

    $(normal [| \((k,c):s) ((k',c'):v) -> k==k' && c/=c0 |]) $
      $(rearrS [| \(a:s) -> (a,s) |]) $
         $(rearrV [| \(b:v) -> (b,v) |]) $
            Prod Replace (upFilterSep c0),

    $(adaptive [| \((k,c):s) ((k',c'):v) -> k==k' && c==c0 |]) $
      \s (b:v) -> b:s,

    $(normal [| \((k,c):s) ((k',c'):v) -> k/=k' && c==c0 |]) $
      $(rearrS [| \(a:s) -> (a,s) |]) $
         $(rearrV [| \v -> ((),v) |]) $
            Prod Skip (upFilterSep c0),

    $(adaptive [| \((k,c):s) ((k',c'):v) -> k/=k' && c/=c0 |]) $
      \(a:s) _ -> s
  ]

{--

*FirewallBX> get (upFilterSep 0) [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)]
Right [(100,1),(100,2),(100,3),(200,5),(200,6)]
*FirewallBX> put (upFilterSep 0) [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)] [(100,1),(100,2),(100,3),(200,5),(200,6)]
Right [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)]
*FirewallBX> put (upFilterSep 0) [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)] [(100,11),(100,2),(100,3),(200,6)]
Right [(100,11),(100,2),(100,3),(100,0),(200,6),(200,0)]
*FirewallBX> put (upFilterSep 0) [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)] [(100,11),(100,2),(100,3),(200,6),(200,9)]
Right [(100,11),(100,2),(100,3),(100,0),(200,6),(200,9),(200,0)]
*FirewallBX> put (upFilterSep 0) [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)] [(100,11),(100,2),(100,3),(200,6),(300,9)]
Right [(100,11),(100,2),(100,3),(100,0),(200,6),(200,0),(300,9),(300,0)]

--}

{--

upListWithGroup:

    source [(100,1),(100,2),(100,3),(100,0),(200,5),(200,6),(200,0)]
 <->
    view [(100,[1,2,3]), (200,[5,6])]

--}

upListWithGroup :: (Eq k, Eq c) => c -> BiGUL (ListWithSep k c) [(k,[c])]
upListWithGroup c0 =
  Case [
    $(normalSV [p| [] |] [p| [] |]) $
      $(rearrV [| \[] -> () |]) $ Skip,

    $(adaptive [| \s v -> length v == 0 |]) $
      \s v -> [],

    $(adaptive [| \s (b:bs) -> not (fst b `elem` map fst s) |]) $
      (\s ((k,cs):v) -> case cs of
                          [] -> (k,c0):s
                          c:_ -> (k,c):s),

    $(adaptive [| \(a:as) (b:bs) -> fst a /= fst b |]) $
      \s ((k,_):v) -> moveHd s k,

    $(normal [| \(a:s) ((k,cs):v) -> fst a == k && length cs > 0 |]) $
      $(rearrS [| \(a:s) -> (a,s) |]) $
         $(rearrV [| \((k,c:cs):v) -> ((k,c),(k,cs):v) |]) $
            Prod Replace (upListWithGroup c0),

   $(adaptive [| \(a:s) ((k,cs):v) -> fst a == k && length cs == 0 && snd a /= c0 |]) $
     \(a:s) v -> s,

   $(normal [| \(a:s) ((k,cs):v) -> fst a == k && length cs == 0 |]) $
      $(rearrS [| \(a:s) -> (a,s) |]) $
         $(rearrV [| \((k,[]):v) -> ((k,()),v) |]) $
            Prod (Prod Replace Skip) (upListWithGroup c0)
  ]


addVElem :: [(k,c)] -> [(k,[c])] -> [(k,c)]
addVElem s ((k,c:cs):_) = (k,c) : s

moveHd :: Eq a => [(a,b)] -> a -> [(a,b)]
moveHd ((a,b):s) k
  | a==k = (a,b):s
  | otherwise = moveHd s k ++ [(a,b)]

{-

> get (upListWithGroup 0) [(100,1),(100,2),(100,0),(200,3),(200,0)]
> put (upListWithGroup 0) [(100,1),(100,2),(100,0),(200,3),(200,0)] [(100,[11,12]),(200,[30])]
> put (upListWithGroup 0) [(100,1),(100,2),(100,0),(200,3),(200,0)] [(100,[11]),(200,[30])]

-}

{-
mergeFst :: Eq a => BiGUL (a,[b]) [(a,b)]
mergeFst =
  Case [
    $(normalV [p| [] |]) $
      $(rearrV [| \[] -> ((), [])|]) $ (Prod Skip Replace),
    $(adaptiveSV [p| (a, [])|] [p| b:bs |]) $
      \(a, _) ((a',b'):_) -> (a, [b']),
    $(normalSV [p| (a, (b: bs)) |] [p| ab': abs' |]) $
      $(rearrS [| \(a, (b: bs)) -> ((a, b), (a, bs)) |])
        ($(rearrV [| \(ab':abs') -> (ab',abs')|]) (Prod Replace mergeFst))
  ]

{-

> get mergeFst (100,[1,2,3])
> put mergeFst (100, [1,2]) [(100,1),(100,20),(100,3)]
> put mergeFst (100,[1,2]) [(100,1)]

-}

dupAndZip :: BiGUL (String, [S.FirewallRule]) [(String, S.FirewallRule)]
dupAndZip  =
  Case [
    $(normalV [p| [] |]) $
      $(rearrV [| \[] -> ((), [])|]) $ (Prod Skip Replace),
    $(adaptiveSV [p| (sn, [])|] [p| v:vs |]) $
      \(sn, _) ((vn, vrule) :vs) -> (sn, [vrule]),
    $(normalSV [p| (sn, [srule]) |] [p| [(vn, vrule)]|]) $
      $(rearrV [| \[(vn, vrule)] -> (vn, [vrule]) |]) Replace,
    $(normalSV [p| (sn, (srule: srest)) |] [p| v: vs |]) $
      $(rearrS [| \(sn, (srule: srest)) -> ((sn, srule), (sn, srest)) |])
        ($(rearrV [| \(v:vs) -> (v,vs)|]) (Prod Replace dupAndZip))
  ]

-}

ruleListUpd :: BiGUL [(String, S.FirewallRule)] [V.Rule]
ruleListUpd = align (\(id, s) -> S.fwStatus s /= 2)
    (\ (id, s) v -> S.fwRuleID s == V.ruleID v)
    ($(update [p| v |] [p| v |] [d| v = ruleUpd |]))
    (\v -> (V.securityGroupRefTo v, S.FirewallRule {
        S.fwRuleID = V.ruleID v
        , S.outbound = False
        , S.ip = V.securityGroupRefFrom v
        , S.port = V.port v
        , S.protocol = V.protocol v
        , S.fwStatus = 1
        }))
    (\(id, s) -> Just (id, S.FirewallRule {
        S.fwRuleID = S.fwRuleID s
        , S.outbound = False
        , S.ip = S.ip s
        , S.port = S.port s
        , S.protocol = S.protocol s
        , S.fwStatus = 2
        }))

ruleUpd :: BiGUL (String, S.FirewallRule) V.Rule
ruleUpd = $(update [p|  V.Rule {
                            V.ruleID = ruleID
                            , V.securityGroupRefFrom = from
                            , V.securityGroupRefTo = to
                            , V.port = port
                            , V.protocol = protocol
                        }
                |] [p|  (to
                        , S.FirewallRule {
                            S.ip = from
                            , S.port = port
                            , S.fwRuleID = ruleID
                            , S.protocol = protocol
                        })
                |] [d|  ruleID = Replace;
                        from = Replace;
                        to = Replace;
                        port = Replace;
                        protocol = Replace
                |])


-- src = S.Model {instances = [S.Instance {instID = "i-cd33dc69", instType = "t2.micro", ami = "ami-59bdb937", state = 16, instStatus = 0, securityGroupRef = "sg-b8d400dc", load = 5.0},S.Instance {instID = "i-6834dbcc", instType = "t2.micro", ami = "ami-59bdb937", state = 16, instStatus = 0, securityGroupRef = "sg-77d40013", load = 6.0},S.Instance {instID = "i-6a34dbce", instType = "t2.micro", ami = "ami-59bdb937", state = 16, instStatus = 0, securityGroupRef = "sg-77d40013", load = 3.0},S.Instance {instID = "i-6b34dbcf", instType = "t2.micro", ami = "ami-59bdb937", state = 16, instStatus = 0, securityGroupRef = "sg-77d40013", load = 1.0},S.Instance {instID = "820114b8-f0b4-44b7-8cc8-40a483cd1c68", instType = "t2.nano", ami = "0000", state = 0, instStatus = 1, securityGroupRef = "sg-123", load = 0.0},S.Instance {instID = "29c00ebc-92b7-43a4-8a24-3e363784618e", instType = "t2.nano", ami = "0000", state = 0, instStatus = 1, securityGroupRef = "sg-123", load = 0.0},S.Instance {instID = "acc11663-075f-4b9d-ae2c-fc6664dbbd54", instType = "t2.nano", ami = "0000", state = 0, instStatus = 1, securityGroupRef = "sg-123", load = 0.0}], securityGroups = [S.SecurityGroup {sgID = "sg-4864f92d", instRefs = [], firewallRules = []},S.SecurityGroup {sgID = "sg-88c92fec", instRefs = [], firewallRules = [S.FirewallRule {fwRuleID = "fw1", outbound = True, port = "80", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0},S.FirewallRule {fwRuleID = "fw2", outbound = True, port = "22", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0},S.FirewallRule {fwRuleID = "fw3", outbound = True, port = "443", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0}]},S.SecurityGroup {sgID = "sg-e9195c8c", instRefs = [], firewallRules = [S.FirewallRule {fwRuleID = "fw4", outbound = True, port = "22", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0}]},S.SecurityGroup {sgID = "sg-b8d400dc", instRefs = [], firewallRules = [S.FirewallRule {fwRuleID = "fw5", outbound = True, port = "22", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0},S.FirewallRule {fwRuleID = "fw6", outbound = True, port = "3306", ip = "sg-77d40013", protocol = "tcp", fwStatus = 0}]},S.SecurityGroup {sgID = "sg-77d40013", instRefs = [], firewallRules = [S.FirewallRule {fwRuleID = "fw7", outbound = True, port = "80", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0},S.FirewallRule {fwRuleID = "fw8", outbound = True, port = "22", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0},S.FirewallRule {fwRuleID = "fw9", outbound = True, port = "443", ip = "0.0.0.0/0", protocol = "tcp", fwStatus = 0}]}], instanceTypes = [S.InstanceType {typeID = "t2.nano", typeCPUs = 1, typeRAM = 0.5, typeCost = 1.0e-2},S.InstanceType {typeID = "t2.micro", typeCPUs = 1, typeRAM = 1.0, typeCost = 2.0e-2},S.InstanceType {typeID = "t2.small", typeCPUs = 1, typeRAM = 2.0, typeCost = 4.0e-2},S.InstanceType {typeID = "t2.medium", typeCPUs = 2, typeRAM = 4.0, typeCost = 8.0e-2},S.InstanceType {typeID = "t2.large", typeCPUs = 2, typeRAM = 8.0, typeCost = 0.16},S.InstanceType {typeID = "m4.large", typeCPUs = 2, typeRAM = 8.0, typeCost = 0.174},S.InstanceType {typeID = "m4.xlarge", typeCPUs = 4, typeRAM = 16.0, typeCost = 0.348},S.InstanceType {typeID = "m4.2xlarge", typeCPUs = 8, typeRAM = 32.0, typeCost = 0.695},S.InstanceType {typeID = "m4.4xlarge", typeCPUs = 16, typeRAM = 64.0, typeCost = 1.391},S.InstanceType {typeID = "m4.10xlarge", typeCPUs = 40, typeRAM = 160.0, typeCost = 3.477},S.InstanceType {typeID = "m3.medium", typeCPUs = 1, typeRAM = 3.75, typeCost = 9.6e-2},S.InstanceType {typeID = "m3.large", typeCPUs = 2, typeRAM = 7.5, typeCost = 0.193},S.InstanceType {typeID = "m3.xlarge", typeCPUs = 4, typeRAM = 15.0, typeCost = 0.385},S.InstanceType {typeID = "m3.2xlarge", typeCPUs = 8, typeRAM = 30.0, typeCost = 0.77}]}

-- view = View {rules = [Rule {ruleID = "fw1", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-88c92fec", port = "80", protocol = "tcp"},Rule {ruleID = "fw2", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-88c92fec", port = "22", protocol = "tcp"},Rule {ruleID = "fw3", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-88c92fec", port = "443", protocol = "tcp"},Rule {ruleID = "fw4", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-e9195c8c", port = "22", protocol = "tcp"},Rule {ruleID = "fw5", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-b8d400dc", port = "22", protocol = "tcp"},Rule {ruleID = "fw6", securityGroupRefFrom = "sg-77d40013", securityGroupRefTo = "sg-b8d400dc", port = "3306", protocol = "tcp"},Rule {ruleID = "fw7", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-77d40013", port = "80", protocol = "tcp"},Rule {ruleID = "fw8", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-77d40013", port = "22", protocol = "tcp"},Rule {ruleID = "fw9", securityGroupRefFrom = "0.0.0.0/0", securityGroupRefTo = "sg-77d40013", port = "443", protocol = "tcp"}]}
