{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Interp.Direct where

import AST hiding (body, effs, funs, sig)
import CC.Frame
import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace)
import Prompt
import Ty (BaseTy (BaseInt), Ty (Base, Fun))

lllet :: Text -> LLTerm -> LLTerm -> LLTerm
lllet x defn body = LLApp (LLVal $ VAbs x body) defn

compile :: Program Text -> Comp
compile (Program funs effs) = Comp main sigLst funs'
 where
  sigLst = fst <$> (effs >>= sigs)

  main :: LLTerm
  main = fromMaybe (error "Expected 'main' function to be defined") (fns !? "main")
  funs' = (\t -> case t of LLVal v -> v; _ -> error "Expected top level funs to be a value") <$> HashMap.delete "main" fns
  fns = HashMap.fromList $ compileFn <$> funs

  compileFn :: FnDefn Text -> (Text, LLTerm)
  compileFn (FnDefn name _ _ _ body) =
    ( name
    , compileTerm body
    )

  compileTerm :: Term Text -> LLTerm
  compileTerm term =
    case term of
      Var x -> LLVar x
      Num n -> LLVal (VNum n)
      Annotate body _ -> compileTerm body
      Let arg param body -> compileTerm (Abs arg body <@> param)
      Abs arg body -> LLVal $ VAbs arg (compileTerm body)
      App left right -> LLApp (compileTerm left) (compileTerm right)
      Handle body handlers ->
        -- TODO: how to compile handles since they need to be embedded
        foldr
          ( \(sig, handle) t ->
              let sigVar = Text.concat ["-", sig, "_prompt"]
                  defn = pushUnderAbs (WithSubCont (LLVar sigVar)) (compileTerm handle)
                  body' = PushPrompt (LLVar sigVar) t
               in lllet sig defn body'
          )
          (compileTerm body)
          handlers

pushUnderAbs :: (LLTerm -> LLTerm) -> LLTerm -> LLTerm
pushUnderAbs f t =
  case t of
    LLVal (VAbs arg body) -> LLVal (VAbs arg (pushUnderAbs f body))
    term -> f term

{- We separate values as a separate datatype as they can be stored in an environment. Since we're CBV arbitrary terms cannot be stored in env -}
data Val
  = VNum Int
  | VAbs Text LLTerm
  deriving (Show)

{- A term where effects have been compiled into delimited continuations explicitly. The rest is a standard CBV lambda calc -}
data LLTerm
  = LLVal Val
  | LLVar Text
  | LLApp LLTerm LLTerm
  | PushPrompt LLTerm LLTerm
  | WithSubCont LLTerm LLTerm
  deriving (Show)

showsTerm :: LLTerm -> ShowS
showsTerm = go (9 :: Int)
 where
  go prec t =
    case t of
      LLVal (VNum n) -> shows n
      LLVal (VAbs arg body) -> ('(' :) . ('\\' :) . (Text.unpack arg ++) . (". " ++) . go prec body . (')' :)
      LLVar var -> (Text.unpack var ++)
      LLApp e1 e2@(LLApp _ _) -> go prec e1 . (' ' :) . ('(' :) . go (prec + 1) e2 . (')' :)
      LLApp e1 e2 -> go prec e1 . (' ' :) . go (prec + 1) e2
      PushPrompt e1 e2 -> ("pushPrompt " ++) . go prec e1 . (" (" ++) . go prec e2 . (')' :)
      WithSubCont e1 e2 -> ("withSubCont " ++) . go prec e1 . (" (" ++) . go prec e2 . (')' :)

data Comp = Comp
  { _main :: LLTerm
  , _sigList :: [Text]
  , _topLevel :: HashMap Text Val
  }

type Ctx ans = HashMap Text (RuntimeVal ans)

data RuntimeVal ans
  = RunInt Int
  | RunPrompt (Prompt ans (RuntimeVal ans))
  | RunFun (RuntimeVal ans -> CC ans (RuntimeVal ans))

translate :: Ctx ans -> LLTerm -> CC ans (RuntimeVal ans)
translate ctx t =
  case t of
    LLVar x -> return $ fromMaybe (error ("Undefined variable " ++ Text.unpack x)) $ ctx !? x
    LLVal (VNum n) -> return (RunInt n)
    LLVal (VAbs arg body) -> return $ RunFun $ \x -> translate (HashMap.insert arg x ctx) body
    LLApp e1 e2 -> do
      f <- unwrapFun <$> translate ctx e1
      a <- translate ctx e2
      f a
    PushPrompt e1 e2 -> do
      p <- unwrapPrompt <$> translate ctx e1
      pushPrompt p (translate ctx e2)
    WithSubCont e1 e2 -> do
      p <- unwrapPrompt <$> translate ctx e1
      withSubCont
        p
        ( \sk ->
            let ctx' = HashMap.insert "resume" (RunFun $ \v -> pushSubCont sk (pure v)) ctx
             in translate ctx' e2
        )

unwrapPrompt :: RuntimeVal ans -> Prompt ans (RuntimeVal ans)
unwrapPrompt (RunPrompt p) = p
unwrapPrompt _ = error "Expected runtime value to be a prompt"

unwrapFun :: RuntimeVal ans -> RuntimeVal ans -> CC ans (RuntimeVal ans)
unwrapFun (RunFun f) = f
unwrapFun _ = error "Expected runtime value to be a function"

evaluate :: Program Text -> Text
evaluate prog = runCC $ do
  ctx' <- foldM acc builtins sigLst
  let ctx = HashMap.union ctx' (valToRuntime ctx <$> topLevel)
  val <- trace (showsTerm main "") $ translate ctx main
  return $ case val of
    RunPrompt (Prompt p) -> Text.concat ["Prompt #", Text.pack $ show p]
    RunInt n -> Text.pack (show n)
    RunFun _ -> "<function>"
 where
  (Comp main sigLst topLevel) = compile prog
  builtins =
    HashMap.fromList
      [ ("add", binIntOp (+))
      , ("sub", binIntOp (-))
      , ("mul", binIntOp (*))
      , ("div", binIntOp div)
      ]
  acc c sig =
    do
      let key = Text.concat ["-", sig, "_prompt"]
      val <- RunPrompt <$> newPrompt
      return $ HashMap.insert key val c

binIntOp :: (Int -> Int -> Int) -> RuntimeVal ans
binIntOp op = RunFun (\(RunInt m) -> pure $ RunFun $ \(RunInt n) -> pure $ RunInt $ op m n)

valToRuntime :: Ctx ans -> Val -> RuntimeVal ans
valToRuntime ctx v =
  case v of
    VNum n -> RunInt n
    VAbs arg body -> RunFun (\x -> translate (HashMap.insert arg x ctx) body)

testProgram :: Program Text
testProgram = Program [main] [get]
 where
  main = FnDefn "main" [] (Base BaseInt) [] $ Handle (Let "x" (Var "get" <@> Num 0) (Var "add" <@> Var "x" <@> Var "x")) [("get", Abs "n" (Var "resume" <@> (Var "add" <@> Var "n" <@> Num 1)))]

  get = EffDefn "Const" [] [("get", Fun (Base BaseInt) (Base BaseInt))]
