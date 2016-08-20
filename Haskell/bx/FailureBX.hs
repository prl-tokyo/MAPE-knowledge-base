{-# LANGUAGE TemplateHaskell
, TypeFamilies #-}

module FailureBX(
  failureUpd,
  get,
  put
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
import qualified AutoscalingFailureModel as S
import qualified FailureModel as V

failureUpd :: BiGUL S.AutoscalingFailure V.FailureView
failureUpd = $(update [p| V.FailureView {
                          V.instances = instances
                   }|][p| S.AutoscalingFailure {
                          S.instances = instances
                   }|][d| instances = instListAlign
                    |])
instListAlign :: BiGUL [S.Instance] [V.Instance]
instListAlign = align (\s -> (S.instStatus s /= 2))
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |][p| v |][d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = V.instType v
      , S.load = 0
      , S.instResponseTime = V.instResponseTime
      , S.instStatus = 1
  })
  (\s -> Just S.Instance {
      S.instID = S.instID s
      , S.instType = S.instType s
      , S.load = S.load s
      , S.instResponseTime = S.instResponseTime s
      , S.instStatus = 2
  })

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
                       V.instID = instID,
                       V.instType = instType,
                       V.instResponseTime = instResponseTime
               }|] [p| S.Instance {
                       S.instID = instID,
                       S.instType = instType,
                       S.instResponseTime = instResponseTime
               }|] [d| instID = Replace;
                       instType = Replace;
                       instResponseTime = Replace
  |])