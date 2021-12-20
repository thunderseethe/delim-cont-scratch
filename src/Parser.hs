{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Control.Applicative (Alternative, Applicative (liftA2))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import Data.Char (isSpace)
import Debug.Trace
import Text.Parser.Char (CharParsing)
import Text.Trifecta (TokenParsing)
import Text.Trifecta hiding (Parser)
import qualified Text.Trifecta as Trifecta
import Text.Trifecta.Indentation
import Control.Monad.Trans.State.Lazy (StateT(runStateT), get, put)

newtype Parser a = Parser { runParser :: IndentedT Char (ReaderT Bool Trifecta.Parser) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, CharParsing)

instance Parsing Parser where
  try (Parser m) = Parser $ try m
  Parser m <?> lbl = Parser $ m <?> lbl
  notFollowedBy (Parser m) = Parser $ notFollowedBy m
  unexpected = Parser . unexpected
  eof = Parser eof

newlineSensitive :: Parser a -> Parser a
newlineSensitive (Parser p) = Parser $ mapIndentedT (local (const True)) p

instance TokenParsing Parser where
  nesting (Parser m) = Parser $ nesting m

  -- Special case logic to not eat newlines while handling indentation correctly
  someSpace =
    let (<&&>) = liftA2 (&&)
     in Parser $ IndentedT $ do
          nlSensitive <- lift ask
          skipSome . satisfy $ if nlSensitive
            then (/= '\n') <&&> isSpace
            else isSpace

  semi = Parser semi

  highlight h (Parser m) = Parser $ highlight h m

instance IndentationParsing Parser where
  localTokenMode fRel (Parser m) = Parser $ localTokenMode fRel m
  localIndentation rel (Parser m) = Parser $ localIndentation rel m
  absoluteIndentation (Parser m) = Parser $ absoluteIndentation m
  ignoreAbsoluteIndentation (Parser m) = Parser $ ignoreAbsoluteIndentation m
