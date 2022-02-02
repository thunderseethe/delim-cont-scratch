{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
module SemaTest (htf_thisModulesTests) where

import Test.Framework hiding (Fun)
import Data.Text (Text)
import AST
import Ty
import qualified Sema
import qualified Data.HashMap.Strict as Hashmap

emptyCtx = Hashmap.empty

infer :: Sema.Ctx Text -> Term Text -> Maybe Ty
infer = Sema.infer

test_inferVarPresent =
  assertEqual
    (Just $ Base BaseI64)
    (infer (Hashmap.singleton "test" (Base BaseI64)) (Var "test"))

test_ifnerVarAbsent =
  assertEqual
    Nothing
    (infer (Hashmap.singleton "test" (Base BaseI64)) (Var "other"))

test_inferInt =
  assertEqual
    (Just $ Base BaseI64)
    (infer emptyCtx (Num 3))

test_inferUnit =
  assertEqual
    (Just unitTy)
    (infer emptyCtx Unit)

{- The implications of this test aren't great.
   We must already have "x" in context as Unit to infer the Abs correctly.

  Perhaps having an infer rule for Abs is incorrect?
-}
test_inferAbsArgHappy =
  assertEqual
    (Just $ Fun unitTy unitTy)
    (infer (Hashmap.singleton "x" unitTy) (Abs "x" Unit))
