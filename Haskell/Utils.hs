{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}

module Utils (align,emb) where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH 
import Generics.BiGUL.Lib

import Control.Arrow
import Data.Maybe

align :: (Show a, Show b) =>
         (a -> Bool)
      -> (a -> b -> Bool)
      -> BiGUL a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> BiGUL [a] [b]
align p match b create conceal = Case [
    $(normal [| \s v -> null (filter p s) && null v |]
             [| \s -> null (filter p s) |])
      ==> $(rearrV [| \[] -> () |])$ (skip ()),
    
    $(adaptive [| \s v -> null v |])
      ==> \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss),
                   
  -- view is necessarily nonempty in the cases below
    $(normal [| \ (s:ss) v -> not (p s) |] [| \(s:ss) -> not (p s) |])
      ==> 
        $(rearrS [| \(s:ss) -> ss |])$
           align p match b create conceal,
           
    $(normal [| \ss vs -> not (null ss) && p (head ss) && match (head ss) (head vs) |]
             [| \ss    -> not (null ss) && p (head ss) |])
      ==> 
        $(rearrV [| \(v:vs) -> (v, vs) |])$
          $(rearrS [| \(s:ss) -> (s, ss) |])$
            b `Prod` align p match b create conceal,
            
    $(adaptive [| \ss (v:_) -> isJust (findFirst (\s -> p s && match s v) ss) ||
                               let s = create v in p s && match s v |])
      ==> 
        \ss (v:_) -> maybe (create v:ss) (uncurry (:)) (findFirst (\s -> p s && match s v) ss)
  ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findFirst p [] = Nothing
    findFirst p (x:xs) | p x       = Just (x, xs)
    findFirst p (x:xs) | otherwise = fmap (id *** (x:)) (findFirst p xs)



