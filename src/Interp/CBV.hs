{-# LANGUAGE LambdaCase #-}
module Interp.CBV where

import Data.Composition
import Data.Function
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

type Env = HashMap String Value
type Cont = Value -> MetaCont -> Value
type MetaCont = Value -> Value
type Fun = Value -> Cont -> MetaCont -> Value

data Value
  = M Int
  | F Fun

data Exp 
  = Val Int
  | Var String
  | Abs String Exp
  | Let String Exp Exp
  | LetRec String Exp Exp
  | App Exp Exp
  | Succ Exp
  | Reset Exp
  | Shift String Exp

-- CPS interpreter of cbv lambda calc + shift/reset
eval2 :: Exp -> Env -> Cont -> MetaCont -> Value
eval2 e env k mk = case e of
  -- Values
  Val v -> k (M v) mk
  Var x -> k (env ! x) mk
  Abs arg body -> k (F $ \v k' mk' -> eval2 body (HashMap.insert arg v env) k' mk') mk
  -- Terms 
  Let x defn body -> eval2 defn env (\v mk' -> eval2 body (HashMap.insert x v env) k mk') mk
  LetRec x defn body -> undefined
  App l r -> 
    let
      k1 = \(F f) mk' -> eval2 r env (\v mk'' -> f v k mk'') mk'
     in eval2 l env k1 mk
  Succ t -> eval2 t env (\(M v) mk' -> k (M $ v + 1) mk') mk
  Reset t -> eval2 t env (&) (\v -> k v mk)
  Shift x t -> eval2 t (HashMap.insert x (F c) env) (&) mk
    where
      c v k' mk' = k' v (\v' -> k' v' mk')

evaluate2 t = eval2 t HashMap.empty (&) id

-- Lambdas to defuncitonalize
-- Abs k free{arg, body} \v k' mk' -> eval2 body (HashMap.insert arg v env) k' mk'
-- Let k free{x, body} \v mk' -> eval2 body (HashMap.insert x v env) k mk'
-- App k free{r, k} \(F f) mk' -> eval2 r env (\v mk'' -> f v k mk'') mk'
-- App k free{f, k} \v mk'' -> f v k mk''
-- Succ k free{k} (\(M v) mk' -> k (M $ v + 1) mk')
-- Reset mk free{k, mk} \v -> k v mk
-- Shift doesn't appear to have any but double check how to handle F values



data Fun3
  = Cls3 String Exp Env3
  | K3 [K3]

data Value3
  = M3 Int
  | F3 Fun3

k3 = F3 . K3
cls3 = F3 .:. Cls3


type Env3 = HashMap String Value3

data K3 
  = K3App0 Exp Env3
  | K3App1 Fun3
  | K3Succ
  | K3Let String Exp Env3

apply3 :: Value3 -> [K3] -> [[K3]] -> Value3
apply3 v [] [] = v
apply3 v [] (k:mk) = apply3 v k mk
apply3 (M3 m) (K3Succ : k) mk = apply3 (M3 $ m + 1) k mk
apply3 v (K3Let x body env : k) mk = eval3 body (HashMap.insert x v env) k mk
apply3 (F3 f) (K3App0 r env : k) mk = eval3 r env (K3App1 f : k) mk
apply3 v (K3App1 f : k) mk =
  case f of
    Cls3 arg body env -> eval3 body (HashMap.insert arg v env) k mk
    K3 k' -> apply3 v k' mk
apply3 _ _ _ = error "Invalid state encountered"

eval3 :: Exp -> Env3 -> [K3] -> [[K3]] -> Value3
eval3 t env k mk = let
    updateEnv k v = HashMap.insert k v env
  in case t of
    Val m -> apply3 (M3 m) k mk
    Var x -> apply3 (env ! x) k mk
    Abs arg body -> apply3 (cls3 arg body env) k mk
    Let x defn body -> eval3 defn env (K3Let x body env : k) mk
    App l r -> eval3 l env (K3App0 r env:k) mk
    Succ t -> eval3 t env (K3Succ:k) mk
    Reset t -> eval3 t env [] (k:mk)
    Shift x t -> eval3 t (updateEnv x (k3 k)) [] mk

evaluate3 t = eval3 t HashMap.empty [] []
