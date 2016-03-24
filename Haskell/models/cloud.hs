{-# LANGUAGE FlexibleContexts,
TemplateHaskell,
TypeFamilies,
TypeOperators,
ViewPatterns #-}

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

import qualified AutoscalingBX as ABX
import qualified RedundancyBX as RBX hiding(SourceModel(..))
