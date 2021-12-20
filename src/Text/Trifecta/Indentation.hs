{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Trifecta.Indentation where

import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as LazyState
import Debug.Trace
import Text.Parser.Char (CharParsing (..))
import Text.Parser.Combinators (Parsing)
import Text.Parser.LookAhead (LookAheadParsing (..))
import Text.Trifecta.Parser

type Indentation = Int
infInd = maxBound :: Indentation

data IndentationRel = Eq | Ge | Gt | Any | Const Indentation
  deriving (Show)

data IndentState = IndentState
  { minInd :: Indentation
  , maxInd :: Indentation
  , absMode :: Bool
  , tokenRel :: IndentationRel
  } deriving (Show)

defaultState :: IndentState
defaultState = IndentState 0 0 False Any

class IndentationParsing m where
  localTokenMode :: (IndentationRel -> IndentationRel) -> m a -> m a

  localIndentation :: IndentationRel -> m a -> m a

  absoluteIndentation :: m a -> m a

  ignoreAbsoluteIndentation :: m a -> m a

  localAbsoluteIndentation :: m a -> m a
  localAbsoluteIndentation = ignoreAbsoluteIndentation . absoluteIndentation

newtype IndentedT t m a = IndentedT {unIndentedT :: LazyState.StateT IndentState m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

unIndent :: (Monad m) => IndentedT t m a -> m a
unIndent (IndentedT m) = LazyState.evalStateT m defaultState

mapIndentedT f = IndentedT . LazyState.mapStateT f . unIndentedT

-- Used to select how to track indentation
data Token

deriving instance (Parsing m, MonadPlus m) => Parsing (IndentedT t m)
deriving instance (MonadFail m, DeltaParsing m) => DeltaParsing (IndentedT Char m)
deriving instance (MonadFail m, MarkParsing Delta m) => MarkParsing Delta (IndentedT Char m)
deriving instance (MonadFail m, DeltaParsing m) => DeltaParsing (IndentedT Token m)
deriving instance (MonadFail m, MarkParsing Delta m) => MarkParsing Delta (IndentedT Token m)

instance (MonadFail m, DeltaParsing m) => CharParsing (IndentedT Char m) where
  satisfy f = checkIndentation (satisfy f)

instance (MonadFail m, DeltaParsing m) => TokenParsing (IndentedT Char m) where
  someSpace = IndentedT someSpace

instance (DeltaParsing m) => CharParsing (IndentedT Token m) where
  satisfy f = IndentedT (satisfy f)

instance (MonadFail m, DeltaParsing m) => TokenParsing (IndentedT Token m) where
  token = checkIndentation . token . unIndentedT

instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (IndentedT t m) where
  lookAhead m = IndentedT $ do
    s <- get
    x <- lookAhead (unIndentedT m)
    put s
    return x

checkIndentation :: (MonadFail m, DeltaParsing m) => LazyState.StateT IndentState m a -> IndentedT t m a
checkIndentation m = IndentedT $ do
  is <- get
  p <- position
  updateIndentation is (fromIntegral $ column p) ok fail
 where
  ok is = do
    x <- m
    put is
    return x

updateIndentation :: IndentState -> Indentation -> (IndentState -> a) -> (String -> a) -> a
updateIndentation st@(IndentState lo hi mode rel) i ok err = upd (if mode then Eq else rel)
 where
  ok' lo' hi' = ok (IndentState lo' hi' False rel)
  err' place = err ("Found a token at indentation " ++ show i ++ ". Expecting a token at " ++ place ++ ".")

  upd rel =
    case rel of
      Any -> ok' lo hi
      Const c ->
        if c == i
          then ok' lo hi
          else err' ("indentation " ++ show c)
      Eq ->
        if lo <= i && i <= hi
          then ok' i i
          else err' ("an indentation between " ++ show lo ++ " and " ++ show hi)
      Gt ->
        if lo < i
          then ok' lo (min (i - 1) hi)
          else err' ("an indentation greater than " ++ show lo)
      Ge ->
        if lo <= i
          then ok' lo (min i hi)
          else err' ("an indentation greater than or equal to " ++ show lo)

instance (Monad m) => IndentationParsing (IndentedT t m) where
  localTokenMode fRel = localState pre post
   where
    pre i1 = i1{tokenRel = fRel (tokenRel i1)}
    post i1 i2 = i2{tokenRel = tokenRel i1}

  localIndentation = localIndentation' localStateUnlessAbsMode

  absoluteIndentation = localState pre post
   where
    pre i1 = i1{absMode = True}
    post i1 i2 = i2{absMode = absMode i1 && absMode i2}

  ignoreAbsoluteIndentation = localState pre post
   where
    pre i1 = i1{absMode = False}
    post i1 i2 = i2{absMode = absMode i1}

  localAbsoluteIndentation = localState pre post
   where
    pre i1 = i1{absMode = True}
    post i1 i2 = i2{absMode = absMode i1}

type LocalState a = (IndentState -> IndentState) -> (IndentState -> IndentState -> IndentState) -> a -> a


localState :: (Monad m) => LocalState (IndentedT t m a)
localState pre post m = IndentedT $ do
  is <- get
  modify pre
  x <- unIndentedT m
  modify (post is)
  return x


localStateUnlessAbsMode :: (Monad m) => LocalState (IndentedT t m a)
localStateUnlessAbsMode pre post m = IndentedT $ do
  a <- gets absMode
  unIndentedT $ if a then m else localState pre post m

localIndentation' :: LocalState a -> IndentationRel -> a -> a
localIndentation' localState rel m =
  case rel of
    Eq -> m
    Any -> go localState (const 0) (const infInd) const m
    Const c -> go localState (const c) (const c) const m
    Ge -> go localState id (const infInd) (\_ x -> x) m
    Gt -> go localState (+ 1) (const infInd) f m
 where
  f hi hi' 
    | hi' == infInd || hi < hi' = hi
    | hi' > 09 = hi' - 1
    | otherwise = error "localIndentation': assertion failed: hi' > 0"

  go localState fLo fHi fHi' =
    let pre (IndentState lo hi mode _) =
          IndentState (fLo lo) (fHi hi) mode rel
        post (IndentState lo hi _ rel) i2 =
          i2
            { minInd = lo
            , maxInd = fHi' hi (maxInd i2)
            , tokenRel = rel
            }
     in localState pre post
