{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import AST
import Parsing (parseExpr, replParse, expr)

import Data.List.NonEmpty
import Data.Text
import qualified Text.Trifecta.Result as Result
import Test.Framework
import Text.Trifecta (rendered, parseFromFile)
import Text.Heredoc
import Data.Either

forgetfulEither :: Result.Result p -> Either () ()
forgetfulEither (Result.Success _) = Right ()
forgetfulEither (Result.Failure _) = Left ()

unwrap :: Result.Result p -> p
unwrap (Result.Success r) = r
unwrap (Result.Failure err) = error $ show err

test_parseVar = assertEqual
  (Var "x")
  (unwrap $ parseExpr "x") 

test_parseKeywordCantBeIdent = assertLeft
  (forgetfulEither $ parseExpr "{ let => whoops }")

test_parseNum = assertEqual
  (Num 3)
  (unwrap $ parseExpr "3")

test_parseApp = assertEqual
  (Var "f" <@> Num 1234)
  (unwrap $ parseExpr "f 1234")

test_parseAppLeftAssoc = assertEqual
  (Var "K" <@> Var "x" <@> Var "y")
  (unwrap $ parseExpr "K x y")

test_parseClosure1Arg = assertEqual
  (Abs "x" (Var "add" <@> Var "x" <@> Num 1))
  (unwrap $ parseExpr "{ x => add x 1 }")

test_parseClosureMutliArgs = assertEqual
  (Abs "x" $ Abs "y" $ Abs "z" $ Var "x" <@> Var "z" <@> (Var "y" <@> Var "z"))
  (unwrap $ parseExpr "{ x y z => x z (y z) }")

test_parseIndentedClosure = assertEqual
  (Abs "x" $ Var "f" <@> Var "x" <@> Var "x")
  (unwrap $ parseExpr 
    [str|{
        |  x =>
        |  f x x
        |}|])

test_parseClosureFailsWithEqIndentation :: IO ()
test_parseClosureFailsWithEqIndentation = assertLeft
    (forgetfulEither $ parseExpr 
      [str|{
          |f x =>
          |f x x
          |}|])

test_parseClosureFailsWithNonAbsoluteIndentation = assertLeft
  (forgetfulEither $ parseExpr
    [str|{
        | f x =>
        |  f x x
        |}|])

main = htfMain htf_thisModulesTests

example = replParse expr 
  [str|{
      |  f x =>
      |  f x x
      |}|]
