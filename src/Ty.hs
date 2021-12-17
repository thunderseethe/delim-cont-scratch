{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Ty where

import Data.Functor.Foldable.TH

data BaseTy = BaseInt
  deriving (Show, Eq)

data Ty
  = Base BaseTy
  | Fun Ty Ty
  deriving (Show, Eq)

makeBaseFunctor ''Ty