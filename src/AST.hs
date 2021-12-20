{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module AST where

import Ty
import Data.Functor.Foldable.TH
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text hiding (foldl', foldr)
import Data.Functor.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.List as Prelude


data Term a
  = Var a
  | Num Int
  | Abs a (Term a)
  | App (Term a) (Term a)
  | Let a (Term a) (Term a)
  | Annotate (Term a) Ty
  deriving (Show, Eq)

makeBaseFunctor ''Term

infixl 9 <@>

(<@>) :: Term a -> Term a -> Term a
x <@> y = App x y

data FnDefn a = FnDefn
  { fnName :: Text
  , args :: [Text]
  , sig :: Ty
  -- Todo figure out if this is the best way to store this
  , constraints :: [(Text, [Text])]
  , body :: Term a
  } deriving (Show, Eq)

data EffDefn = EffDefn
  { effName :: Text
  , sigArgs :: [Text]
  , sigs :: [(Text, Ty)]
  } deriving (Show, Eq)

data Program a = Program 
  { funs :: [FnDefn a]
  , effs :: [EffDefn]
  } deriving (Show, Eq)

