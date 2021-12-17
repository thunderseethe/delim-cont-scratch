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

data Defn a = Defn
  { name :: Text,
    args :: [Text],
    body :: Term a
  } deriving (Show)

newtype Program a = Program (NonEmpty (Defn a))
    deriving (Show)

{-lowerParse :: ParseProgram -> Program Text
lowerParse (ParseProgram defns) = Program $ fmap toDefn defns
  where
    toDefn (ParseDefn name args body) = Defn name args (desugar body)
 
    desugar :: ParseTerm -> Term Text
    desugar = \case
        PVar name -> Var name
        PNum i -> Num i
        PLet defns body -> Let (desugar <$> defns) (desugar body)
        PApp (hd :| terms) -> foldl' App (desugar hd) $ desugar <$> terms
        PAbs (hd :| args) body -> foldr Abs (desugar body) (hd:args)-}

