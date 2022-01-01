{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
module Main where

import AST
import Parser (expr, parseExpr, parseTy, parseFnDefn, replParse, parseEffDefn, parseProgram)
import Ty

import Data.Either
import Data.List.NonEmpty
import Data.Text
import Test.Framework hiding (Fun)
import Text.Heredoc
import Text.Trifecta (parseFromFile, rendered)
import qualified Text.Trifecta.Result as Result
import Control.Exception (handle)

forgetfulEither :: Result.Result p -> Either () p
forgetfulEither (Result.Success p) = Right p
forgetfulEither (Result.Failure _) = Left ()

unwrap :: Result.Result p -> p
unwrap (Result.Success r) = r
unwrap (Result.Failure err) = error $ show err

test_parseVar =
  assertEqual
    (Var "x")
    (unwrap $ parseExpr "x")

test_parseKeywordCantBeIdent =
  assertLeft
    (forgetfulEither $ parseExpr "{| let => whoops |}")

test_parseNum =
  assertEqual
    (Num 3)
    (unwrap $ parseExpr "3")

test_parseUnti =
  assertEqual
    Unit
    (unwrap $ parseExpr "{}")

test_parseApp =
  assertEqual
    (Var "f" <@> Num 1234)
    (unwrap $ parseExpr "f 1234")

test_parseAppLeftAssoc =
  assertEqual
    (Var "K" <@> Var "x" <@> Var "y")
    (unwrap $ parseExpr "K x y")

test_parseClosure1Arg =
  assertEqual
    (Abs "x" (Var "add" <@> Var "x" <@> Num 1))
    (unwrap $ parseExpr "{| x => add x 1 |}")

test_parseClosureMutliArgs =
  assertEqual
    (Abs "x" $ Abs "y" $ Abs "z" $ Var "x" <@> Var "z" <@> (Var "y" <@> Var "z"))
    (unwrap $ parseExpr "{| x y z => x z (y z) |}")

test_parseIndentedClosure =
  assertEqual
    (Abs "x" $ Var "f" <@> Var "x" <@> Var "x")
    (unwrap $
        parseExpr
          [str|{|
        |  x =>
        |  f x x
        ||}|])

test_parseClosureFailsWithEqIndentation =
  assertLeft
    (forgetfulEither $
        parseExpr
          [str|{|
          |f x =>
          |f x x
          ||}|])

test_parseLet =
  assertEqual
    (Let "x" (App (Var "f") (Num 3)) (Var "x"))
    (unwrap $
        parseExpr
          [str|let x = f 3
        |x|])

test_parseNestedLets =
  assertEqual
    (Let "x" (Let "y" (Var "f" <@> Num 4) (Var "add" <@> Var "y" <@> Num 1)) (Var "mult" <@> Var "x" <@> Num 2))
    (unwrap $
        parseExpr
          [str|let x = let y = f 4
        |        add y 1
        |mult x 2|])

test_parseIndentedLetDefn =
  assertEqual
    (Let "k" (Abs "x" (Abs "y" (Var "x"))) (Var "k" <@> Num 10))
    (unwrap $
        parseExpr
          [str|let k =
              |  {| x y => x |}
              |k 10|])

test_parseIndentedLetBody =
  assertEqual
    (Let "x" (Num 3) (Let "y" (Num 17) (Var "add" <@> Var "x" <@> Var "y")))
    (unwrap $
        parseExpr
          [str|let x = 3
              |let y = 17
              |add x y|])

test_parseIndentedNestedLetIsParseError =
  assertEqual
    (Left ())
    (forgetfulEither $
        parseExpr
          [str|let x = 3
              | let y = 17
              |add x y|])

test_parseIndentedClosuresInLet = 
  assertEqual
    (Let "f" (Abs "x" (Var "foldr" <@> Var "alt" <@> Var "x")) (Abs "y" (Abs "z" (Var "f" <@> (Var "y" <@> Var "z")))))
    (unwrap $ parseExpr
      [str|let f =
          |  {| x => foldr alt x |}
          |{| y z => f (y z) |}|])

test_parseHandleExpr =
  assertEqual
    (Handle (Var "add" <@> (Var "get" <@> Var "unit") <@> Num 4) [("get", Abs "k" $ Abs "resume" (Num 4))])
    (unwrap $ parseExpr
      [str|add (get unit) 4
          |handle
          |  get k resume = 4
          |])

test_parseBaseI64Ty =
  assertEqual
    (Base BaseI64)
    (unwrap $ parseTy "I64")

test_parseTyVar =
  assertEqual
    (TyVar "a")
    (unwrap $ parseTy "a")

test_parseTyVarLonger =
  assertEqual
    (TyVar "_a_'longer_ty_'var_")
    (unwrap $ parseTy "_a_'longer_ty_'var_")

test_parseTyVarFailsWithUppercaseLetter =
  assertEqual
    (Left ())
    (forgetfulEither $ parseTy "a_bWhoops")

test_parseSingleFunTy =
  assertEqual
    (Fun (Base BaseI64) (Base BaseI64))
    (unwrap $ parseTy "I64 -> I64")

test_parseFunTyAssocsRight =
  assertEqual
    (Fun (Base BaseI64) (Fun (Base BaseI64) (Fun (Base BaseI64) (Base BaseI64))))
    (unwrap $ parseTy "I64 -> I64 -> I64 -> I64")

test_unfinishedFunctionTypeIsParseError =
  assertEqual
    (Left ())
    (forgetfulEither $ parseTy "I64 ->")

test_parseFnConstraintlessFnDef =
  assertEqual
    (FnDefn "the_s_combinator" [] (Base BaseI64) [] (Abs "x" (Abs "y" (Abs "z" (Var "x" <@> Var "z" <@> (Var "y" <@> Var "z"))))))
    (unwrap $ parseFnDefn "fn the_s_combinator : I64 = {| x y z => x z (y z) |}")

sCombTy = (TyVar "a" ->> TyVar "b" ->> TyVar "c") ->> (TyVar "a" ->> TyVar "b") ->> TyVar "a" ->> TyVar "c"
sCombAST = Abs "x" $ Abs "y" $ Abs "z" $ Var "x" <@> Var "z" <@> (Var "y" <@> Var "z")

test_parseFnConstrainedFnDefn =
  assertEqual
    (FnDefn "hash_something" [] 
      sCombTy
      [("a", ["Show", "Eq"]), ("b", ["Hash"])] 
      sCombAST)
    (unwrap $ parseFnDefn "fn hash_something : (a -> b -> c) -> (a -> b) -> a -> c where a : Show, Eq b: Hash = {| x y z => x z (y z) |}")

test_parseIndentedFnDefn =
  assertEqual
    (FnDefn "the_s_combinator" [] 
      sCombTy
      [("a", ["Eq", "Show"]), ("b", ["Hash"])] 
      sCombAST)
    (unwrap $ parseFnDefn
      [str|fn the_s_combinator
          |  : (a -> b -> c)
          |    -> (a -> b)
          |    -> a -> c
          |where
          |  a : Eq, Show
          |  b : Hash
          |= {| x y z => x z (y z) |}
          |])

test_parseUnindentedFnFails =
  assertEqual
    (Left ())
    (forgetfulEither $ parseFnDefn
      [str|  fn the_s_combinator 
          |: (a -> b -> c)
          |-> (a -> b)
          |-> a -> c
          |where
          |a : Eq, Show
          |b : Hash
          |= {| x y z => x z (y z) |}
          |])

test_parseIndentedEffDefn =
  assertEqual
    (EffDefn "State" ["s"] [("get", TyVar "s" ->> TyVar "s"), ("put", TyVar "s" ->> TyVar "s")])
    (unwrap $ parseEffDefn
      [str|eff State s where
          |  get : s -> s
          |  put : s -> s
          |])

test_parseEffWithEmptyMethodsFail =
  assertEqual
    (Left ())
    (forgetfulEither $ parseEffDefn
      [str|eff TestMe a b c where
          |])

test_parseEffWithEqIndentFails =
  assertEqual
    (Left ())
    (forgetfulEither $ parseEffDefn
      [str|eff Function a b where
          |call : a -> b
          |compose : (a -> b) -> (c -> d)
          |tell : b
          |])

test_parseProgram =
  assertEqual
    (Program 
      { funs = [FnDefn "the_s_combinator" [] sCombTy [("a", ["Eq", "Show"]), ("b", ["Hash"])] sCombAST]
      , effs = [EffDefn "State" ["s"] [("get", TyVar "s" ->> TyVar "s"), ("put", TyVar "s" ->> TyVar "s")]]
      })
    (unwrap $ parseProgram
      [str|fn the_s_combinator
          |  : (a -> b -> c)
          |    -> (a -> b)
          |    -> a -> c
          |where
          |  a : Eq, Show
          |  b : Hash
          |= {| x y z => x z (y z) |}
          |
          |eff State s where
          |  get : s -> s
          |  put : s -> s
          |])

main = htfMain htf_thisModulesTests

example = replParse expr "let x = let y = 4 in y in {| y => x |}"
