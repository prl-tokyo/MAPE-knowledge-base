{-# LANGUAGE TemplateHaskell
, ViewPatterns #-}

module Utils (align) where

import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Arrow
import Data.Maybe

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
