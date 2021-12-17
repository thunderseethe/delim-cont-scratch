{-# LANGUAGE LambdaCase #-}
module Interp.Arith where

import Data.Function
import Debug.Trace

-- Direct style
data Exp
  = Val Int
  | Add Exp Exp
  deriving (Show)

eval1 = fix (\rec v ->
  case v of
    Val v -> v
    Add l r -> rec l + rec r)

evaluate1 = eval1

-- CPS translation
eval2 = fix (\rec e k ->
  case e of
    Val v -> k v
    Add l r -> rec l (\m -> rec r (\n -> k (m + n))))

evaluate2 e = eval2 e id


-- Defunctionalized
data Cont3
  = ID3
  | Hold3 Exp Cont3
  | Sum3 Int Cont3
  deriving (Show)

eval3 e k =
  case e of
    Val v -> trace ("applying " ++ show k) $ apply3 k v
    Add l r -> eval3 l (Hold3 r k)

apply3 k i =
  case k of
    ID3 -> i
    Hold3 e k' -> eval3 e (Sum3 i k')
    Sum3 m k' -> apply3 k' (m + i)

evaluate3 e = eval3 e ID3

-- Corrolary: Use list instead of recursive ADT
data Cont4 = Hold4 Exp | Sum4 Int

eval4 e k =
  case e of
    Val v -> apply4 k v
    Add l r -> eval4 l (Hold4 r:k)

apply4 [] i = i
apply4 (k:ks) i =
  case k of
    Hold4 e -> eval4 e (Sum4 i:ks)
    Sum4 m -> apply4 ks (m + i)

evaluate4 e = eval4 e []

-- Value-based Abstract machine
data Ctx5
  = Hold5 Exp
  | Sum5 Int

eval5 ctx e =
  case e of
    Val v -> cont5 ctx v
    Add l r -> eval5 (Hold5 r:ctx) l

cont5 [] n = n
cont5 (c:ctx) n =
  case c of
    Hold5 r -> eval5 (Sum5 n:ctx) r
    Sum5 m -> cont5 ctx (n + m)

-- programming with delim conts
rev a xs = foldl (flip (:)) xs a

-- Accumulator style
findFirstPrefixA :: (a -> Bool) -> [a] -> [a]
findFirstPrefixA p xs = visit xs []
  where
    visit [] a = []
    visit (x:xs) a =
      if p x
       then rev (x:a) []
       else visit xs (x:a)

findAllPrefixA :: (a -> Bool) -> [a] -> [[a]]
findAllPrefixA p xs = visit xs []
  where
    visit [] a = []
    visit (x:xs) a = let
                      a' = x:a
                      f = if p x then (:) (rev a' []) else id
                      in f (visit xs a')

-- CPS passing style
findFirstPrefixC1 :: (a -> Bool) -> [a] -> [a]
findFirstPrefixC1 p xs = visit xs id
  where
    visit [] k = []
    visit (x:xs) k =
      let
        k' = k . (x :)
       in if p x then k' [] else visit xs k'

findAllPrefixesC1 p xs = visit xs id
  where
    visit [] k = []
    visit (x:xs) k =
      let
        k' = k . (x :)
        f = if p x then (k' [] :) else id
       in f (visit xs k')

-- Continuation passing counter part of delim cont direct style using shift and reset
findFirstPrefixC2 :: (a -> Bool) -> [a] -> [a]
findFirstPrefixC2 p xs = visit xs (&) id
  where
    visit [] k1 k2 = k2 []
    visit (x:xs) k1 k2 =
      let k1' = \vs k2' -> k1 (x:vs) k2'
       in if p x
             then k1' [] k2
             else visit xs k1' k2

findAllPrefixC2 :: (a -> Bool) -> [a] -> [[a]]
findAllPrefixC2 p xs = visit xs (&) id
  where
    visit [] k1 k2 = k2 []
    visit (x:xs) k1 k2 =
      let k1' = \vs k2' -> k1 (x:vs) k2'
       in if p x
             then k1' [] (\vs -> visit xs k1' (k2 . (:) vs))
             else visit xs k1' k2
