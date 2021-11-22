{-# LANGUAGE LambdaCase #-}
module Sema where

import AST
import Control.Monad
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import Prelude hiding (lookup)

data Ty = TInt | TFun Ty Ty
    deriving (Eq, Show)

type Ctx a = HashMap a Ty

infer :: (Eq a, Hashable a) => Ctx a -> Term a -> Maybe Ty
infer ctx = \case
  Var var -> HashMap.lookup var ctx
  Num n -> Just TInt
  Abs arg body -> undefined
  App te te' -> undefined
  Let hm te -> undefined


check :: (Eq a, Hashable a) => Ctx a -> Term a -> Ty -> Maybe Ty
check ctx (Num _) TInt = Just TInt
check ctx (Num _) _ = Nothing
check ctx (Var var) goal = HashMap.lookup var ctx >>= (\ty -> if ty == goal then Just ty else Nothing)
check ctx (Abs arg body) TInt = Nothing
check ctx (Abs arg body) (TFun argTy bodyTy) = do
    aty <- check ctx (Var arg) argTy
    bty <- check (HashMap.insert arg aty ctx) body bodyTy
    return (TFun aty bty)
check ctx (Let defns body) goal = 
    let
        ctx' = HashMap.foldr (\term ctx' -> _) ctx defns
     in _
check ctx (App fun arg) goal = _
