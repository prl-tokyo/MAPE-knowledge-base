{-# LANGUAGE TemplateHaskell,
TypeFamilies #-}

module RedundancyBX(
  instUpd,
  instListAlign,
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
import qualified SourceModel as S
import qualified RedundancyModel as V

instUpd :: BiGUL S.Instance V.Instance
instUpd = $(update [p| V.Instance {
        V.instID = instID
        , V.securityGroupRef = securityGroupRef
        , V.status = status
    }|] [p| S.Instance {
        S.instID = instID
        , S.securityGroupRef = securityGroupRef
        , S.instStatus = status
    }|] [d| instID = Replace;
            securityGroupRef = Replace;
            status = Replace
    |])

instListAlign :: BiGUL [S.Instance] [V.Instance]
instListAlign = align (const True)
  (\ s v -> S.instID s == V.instID v)
  ($(update [p| v |] [p| v |] [d| v = instUpd |]))
  (\v -> S.Instance {
      S.instID = V.instID v
      , S.instType = "t2.micro"
      , S.instStatus = 1
      , S.ami = "0000"
      , S.state = 0
      , S.load = 0.00
      , S.securityGroupRef = V.securityGroupRef v
      })
  (const Nothing)
