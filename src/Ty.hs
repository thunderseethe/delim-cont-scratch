{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Ty where

import Data.Text
import Data.Functor.Foldable.TH

data BaseTy 
  = BaseI64
  | BaseUnit
  deriving (Show, Eq)

data Ty
  = Base BaseTy
  | TyVar Text
  | Fun Ty Ty
  deriving (Show, Eq)

makeBaseFunctor ''Ty

infixr 9 ->>
  
(->>) :: Ty -> Ty -> Ty
x ->> y = Fun x y
