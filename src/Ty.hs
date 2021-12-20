{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Ty where

import Data.Text
import Data.Functor.Foldable.TH

data BaseTy = BaseInt
  deriving (Show, Eq)

data Ty
  = Base BaseTy
  | TyVar Text
  | Fun Ty Ty
  deriving (Show, Eq)

makeBaseFunctor ''Ty

infixr 9 ->>

x ->> y = Fun x y
