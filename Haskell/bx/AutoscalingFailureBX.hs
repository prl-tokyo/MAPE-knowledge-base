{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module AutoscalingFailureBX(
  autoscalingFailureUpd,
  get,
  put
) where

import Generics.BiGUL
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Control.Monad
import Data.Char
import Data.List
import GHC.Generics
import Control.Arrow
import Data.Maybe
import Utils
import qualified SourceModel as S
import qualified AutoscalingFailureModel as V

autoscalingFailureUpd :: String -> BiGUL S.Model V.AutoscalingFailure
autoscalingFailureUpd sg = $(update [p| S.Model {
                                        S.instances = instances
                                        , S.instanceTypes = instanceTypes
                                }|] [p| V.AutoscalingFailure {
                                        V.instances = instances
                                        , V.instanceTypes = instanceTypes
                                }|] [d| instances = instListAlign sg;
                                        instanceTypes = instTypesAlign
                                 |])

instTypesAlign :: BiGUL [S.InstanceType] [V.InstanceType]
instTypesAlign = align (\s -> True)
  (\ s v -> S.typeID s == V.typeID v)
  ($(update [p| v |] [p| v |] [d| v = instTypeUpd |]))
  (\v -> S.InstanceType {
      S.typeID = V.typeID v,
      S.typeCPUs = V.typeCPUs v,
      S.typeRAM = V.typeRAM v,
      S.typeCost = V.typeCost v
      })
  (const Nothing)

instTypeUpd :: BiGUL S.InstanceType V.InstanceType
instTypeUpd = $(update [p| S.InstanceType {
                           S.typeID = typeID,
                           S.typeCPUs = typeCPUs,
                           S.typeRAM = typeRAM,
                           S.typeCost = typeCost
                   }|] [p| V.InstanceType {
                           V.typeID = typeID,
                           V.typeCPUs = typeCPUs,
                           V.typeRAM = typeRAM,
                           V.typeCost = typeCost
                   }|] [d| typeID = Replace;
                           typeCPUs = Replace;
                           typeRAM = Replace;
                           typeCost = Replace
  |])

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.load = instLoad,
                       S.instResponseTime = instResponseTime,
                       S.instStatus = instStatus
               }|] [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.instLoad = instLoad,
                       V.instResponseTime = instResponseTime,
                       V.instStatus = instStatus
               }|] [d| instID = Replace;
                       instType = Replace;
                       instLoad = Replace;
                       instResponseTime = Replace;
                       instStatus = Replace
  |])

instListAlign :: String -> BiGUL [S.Instance] [V.Instance]
instListAlign sg = align (\s -> (S.instStatus s /= 2) && (S.securityGroupRef s == sg))
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = V.instType v
      , S.load = V.instLoad v
      , S.instStatus = V.instStatus v
      , S.ami = "0000"
      , S.state = 0
      , S.instResponseTime = V.instResponseTime v
      , S.securityGroupRef = sg
      })
  (\s -> Just S.Instance {
      S.instID = S.instID s
      , S.instType = S.instType s
      , S.load = S.load s
      , S.instStatus = S.instStatus s
      , S.ami = S.ami s
      , S.state = S.state s
      , S.instResponseTime = S.instResponseTime s
      , S.securityGroupRef = S.securityGroupRef s
      })
