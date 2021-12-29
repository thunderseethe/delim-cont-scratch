{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Interp.Direct where

import AST
import CC.Func
import Control.Arrow
import Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat, unpack)
import qualified Data.Text as Text
import Prompt (Prompt (..))

lllet x defn body = LLApp (LLAbs x body) defn

compile :: Program Text -> Comp
compile (Program funs effs) = Comp main fns
 where
  sigLst = fst <$> (effs >>= sigs)
  effMap = HashMap.fromList $ snd $ mapAccumR f 0 sigLst
   where
    f n sig = (n + 1, (sig, Prompt n))

  main :: LLTerm
  main =
    let mainBody = fromMaybe (error "Expected 'main' function to be defined") (fns !? "main")
     in foldr (\sig t -> lllet (Text.concat ["-", sig, "_prompt"]) NewPrompt t) mainBody sigLst
  fns = HashMap.fromList $ compileFn <$> funs

  compileFn :: FnDefn Text -> forall ans. (Text, LLTerm)
  compileFn (FnDefn name args _ _ body) = (name, compileTerm body)

  compileTerm :: Term Text -> LLTerm
  compileTerm term =
    case term of
      Var x -> LLVar x
      Num n -> LLNum n
      Annotate body _ -> compileTerm body
      Let arg param body -> compileTerm (Abs arg body <@> param)
      Abs arg body -> LLAbs arg (compileTerm body)
      App left right -> LLApp (compileTerm left) (compileTerm right)
      Handle body handlers ->
        -- TODO: how to compile handles since they need to be embedded
        foldr
          ( \(sig, handle) t ->
              let sigVar = Text.concat ["-", sig, "_prompt"]
                  defn = WithSubCont (LLVar sigVar) (compileTerm handle)
                  body = PushPrompt (LLVar sigVar) t
               in lllet sig defn body
          )
          (compileTerm body)
          handlers

{- A term where effects have been compiled into delimited continuations explicitly. The rest is a standard CBV lambda calc -}
data LLTerm
  = LLVar Text
  | LLNum Int
  | -- | LLPrompt (Prompt ans (LLTerm ans))
    -- Delim Control Stuff
    NewPrompt
  | PushPrompt LLTerm LLTerm
  | WithSubCont LLTerm LLTerm
  | PushSubCont LLTerm LLTerm
  | -- CBV lambda calc stuff
    LLAbs Text LLTerm
  | LLApp LLTerm LLTerm

-- To be decided
data Comp = Comp
  { main :: LLTerm
  , topLevel :: HashMap Text LLTerm
  }

evaluate :: Comp -> CC ans LLTerm
evaluate (Comp main topLevel) = eval (InterTerm . return <$> topLevel) (topLevel ! "main")

-- Intermediate values that can appear during execution
data InterTerm ans a =
    ITerm LLTerm
  | IPrompt (Prompt ans a)
  | ICont () -- Todo

eval :: HashMap Text (InterTerm ans a) -> LLTerm -> CC ans LLTerm
eval ctx t =
  case t of
    LLVar x -> runTerm (ctx ! x)
    LLNum n -> return $ LLNum n
    -- LLPrompt p -> return $ LLPrompt p
    NewPrompt -> LLPrompt <$> newPrompt
    PushPrompt prompt e -> do
      -- LLPrompt p <- eval ctx prompt
      pushPrompt p (eval ctx e)

{-case t of
  Var x -> runTerm . fromMaybe (error $ "variable '" ++ unpack x ++ "' not in scope") $ ctx !? x
  Num n -> {- A number is inert -} return $ LLNum n
  Abs arg body -> {- A lambda is inert -} return $ LLAbs arg body
  Let x defn body ->
    let t = eval ctx defn
     in eval (HashMap.insert x (InterTerm t) ctx) body
  Annotate body _ -> eval ctx body
  Handle body handlers -> eval ctx body
  App l r -> do
    --Abs arg body <- eval ctx l
    let param = eval ctx r
    eval (HashMap.insert arg (InterTerm param) ctx) body-}
