module Sema where

import AST
import Ty
import Control.Monad
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude hiding (lookup)

type Ctx a = HashMap a Ty

{-infer :: (Eq a, Hashable a) => Ctx a -> Term a -> Maybe Ty
infer ctx = \case
  Var var -> HashMap.lookup var ctx
  Num _ -> Just (Base BaseInt)
  Abs arg body -> undefined
  App te te' -> undefined
  Let v d te -> undefined-}


{-check :: (Eq a, Hashable a) => Ctx a -> Term a -> Ty -> Maybe Ty
check ctx (Num _) (Base BaseInt) = Just (Base BaseInt)
check ctx (Num _) _ = Nothing
check ctx (Var var) goal = HashMap.lookup var ctx >>= (\ty -> if ty == goal then Just ty else Nothing)
check ctx (Abs arg body) (Base BaseInt) = Nothing
check ctx (Abs arg body) (Fun argTy bodyTy) = do
    aty <- check ctx (Var arg) argTy
    bty <- check (HashMap.insert arg aty ctx) body bodyTy
    return (Fun aty bty)
check ctx (Let var defn body) goal = 
    let
        ctx' = HashMap.foldr (\term ctx' -> _) ctx defns
     in _
check ctx (App fun arg) goal = _-}
