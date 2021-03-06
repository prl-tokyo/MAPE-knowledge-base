{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}

module AutoScalingBX(
  instUpd,
  instListAlign,
  autoScalingUpd,
  get,
  put
  ) where

--import Generics.BiGUL
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
import qualified AutoscalingFailureModel as S
import qualified AutoScalingModel as V

autoScalingUpd :: BiGUL S.AutoscalingFailure V.View
autoScalingUpd = $(update [p| S.AutoscalingFailure {
                              S.instances = instances
                              , S.instanceTypes = instanceTypes
                      }|] [p| V.View {
                              V.instances = instances
                              , V.instanceTypes = instanceTypes
                      }|] [d| instances = instListAlign;
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
                       S.instLoad = instLoad
               }|] [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.instLoad = instLoad
               }|] [d| instID = Replace;
                       instType = Replace;
                       instLoad = Replace
  |])

instListAlign :: BiGUL [S.Instance] [V.Instance]
instListAlign = align (\s -> (S.instStatus s /= 2))
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = V.instType v
      , S.instLoad = V.instLoad v
      , S.instResponseTime = -1
      , S.instStatus = 1
      })
  (\s -> Just S.Instance {
      S.instID = S.instID s
      , S.instType = S.instType s
      , S.instLoad = S.instLoad s
      , S.instResponseTime = S.instResponseTime s
      , S.instStatus = 2
      })
