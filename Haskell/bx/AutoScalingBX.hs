{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module AutoScalingBX(
  instUpd,
  instListAlign,
  autoScalingUpd,
  get,
  put
  ) where

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
import Utils
import qualified SourceModel as S
import qualified AutoScalingModel as V

autoScalingUpd :: String -> BiGUL S.Model V.View
autoScalingUpd sg = $(update [p| V.View {
                                 V.instances = instances
                                 , V.instanceTypes = instanceTypes
                         }|] [p| S.Model {
                                 S.instances = instances
                                 , S.instanceTypes = instanceTypes
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
instTypeUpd = $(update [p| V.InstanceType {
                           V.typeID = typeID,
                           V.typeCPUs = typeCPUs,
                           V.typeRAM = typeRAM,
                           V.typeCost = typeCost
                   }|] [p| S.InstanceType {
                           S.typeID = typeID,
                           S.typeCPUs = typeCPUs,
                           S.typeRAM = typeRAM,
                           S.typeCost = typeCost
                   }|] [d| typeID = Replace;
                           typeCPUs = Replace;
                           typeRAM = Replace;
                           typeCost = Replace
  |])

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.instLoad = instLoad
               }|] [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.load = instLoad
               }|] [d| instID = Replace;
                       instType = Replace;
                       instLoad = Replace
  |])

instListAlign :: String -> BiGUL [S.Instance] [V.Instance]
instListAlign sg = align (\s -> (S.instStatus s /= 2) && (S.securityGroupRef s == sg))
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v,
      S.instType = V.instType v,
      S.load = V.instLoad v,
      S.instStatus = 1,
      S.ami = "0000",
      S.state = 0,
      S.securityGroupRef = "sg-123"
      })
  (\s -> Just S.Instance {
      S.instID = S.instID s
      , S.instType = S.instType s
      , S.load = S.load s
      , S.instStatus = 2
      , S.ami = S.ami s
      , S.state = S.state s
      , S.securityGroupRef = S.securityGroupRef s
      })
