{-# LANGUAGE TupleSections #-}
module Sema where

import AST
import Ty
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap, (!?))
import Data.Hashable
import Prelude hiding (lookup)

type Ctx a = HashMap a Ty

infer :: (Eq a, Hashable a) => Ctx a -> Term a -> Maybe Ty
infer ctx t =
  case t of
    Var var -> ctx !? var
    Num _ -> Just (Base BaseI64)
    Unit -> Just (Base BaseUnit)
    Abs arg body -> do
      argTy <- ctx !? arg
      bodyTy <- infer ctx body
      return (argTy ->> bodyTy)
    App _ _ -> Nothing -- I don't think we can handle this?
    Annotate term ty -> check ctx term ty
    Let x local body -> do
      localTy <- infer ctx local
      infer (HashMap.insert x localTy ctx) body
    Handle body handlers -> do
      handlers' <- traverse (\(x, t) -> (x,) <$> infer ctx t) handlers
      let ctx' = foldr (\(x, t) acc -> HashMap.insert x t acc) ctx handlers'
      infer ctx' body

check :: (Eq a, Hashable a) => Ctx a -> Term a -> Ty -> Maybe Ty
check ctx (Var var) ty =
  do
    varTy <- ctx !? var
    if varTy == ty then Just varTy else Nothing
check _ (Num _) ty =
  if ty == Base BaseI64 
    then Just (Base BaseI64) 
    else Nothing
check _ Unit ty =
  if ty == Base BaseUnit
    then Just (Base BaseUnit)
    else Nothing
check ctx (Abs arg body) (Fun argTy bodyTy) =
  do
    True <- (== argTy) <$> ctx !? arg
    _ <- check (HashMap.insert arg argTy ctx) body bodyTy
    Just (Fun argTy bodyTy)
-- Check a lambda against anything but a function type fails
check _ (Abs _ _) _ = Nothing
check ctx (App fun arg) ty =
  do
    argTy <- infer ctx arg
    ty <$ check ctx fun (Fun argTy ty)
check ctx (Let x local body) ty =
  do
    localTy <- infer ctx local
    check (HashMap.insert x localTy ctx) body ty
check ctx (Handle body handlers) ty =
  do 
    handlers' <- traverse (\(x, t) -> (x,) <$> infer ctx t) handlers
    let ctx' = foldr (\(x, t) acc -> HashMap.insert x t acc) ctx handlers'
    check ctx' body ty
check _ (Annotate _ ty) ty' = 
  if ty == ty' 
    then Just ty 
    else Nothing
