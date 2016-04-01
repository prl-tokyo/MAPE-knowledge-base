{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FirewallBX(
  dupAndZip
  , ruleListUpd
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
