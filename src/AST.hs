{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module AST where

import Parser (ParseProgram(..), ParseDefn(..), ParseTerm(..))
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
  | Let (HashMap a (Term a)) (Term a)
  deriving (Show)

makeBaseFunctor ''Term

data Defn a = Defn
  { name :: Text,
    args :: [Text],
    body :: Term a
  } deriving (Show)

newtype Program a = Program (NonEmpty (Defn a))
    deriving (Show)

lowerParse :: ParseProgram -> Program Text
lowerParse (ParseProgram defns) = Program $ fmap toDefn defns
  where
    toDefn (ParseDefn name args body) = Defn name args (desugar body)
 
    desugar :: ParseTerm -> Term Text
    desugar = \case
        PVar name -> Var name
        PNum i -> Num i
        PLet defns body -> Let (desugar <$> defns) (desugar body)
        PApp (hd :| terms) -> foldl' App (desugar hd) $ desugar <$> terms
        PAbs (hd :| args) body -> foldr Abs (desugar body) (hd:args)

