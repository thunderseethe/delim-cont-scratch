{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import AST
import Control.Applicative
import Control.Monad.Trans.Class
import Data.HashSet (fromList)
import Data.List.NonEmpty (NonEmpty ((:|)), some1)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Par1 (unPar1))
import Parser (Parser, runParser, newlineSensitive)
import qualified Text.Parser.Token as Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta hiding (Parser, ident, reserve, runParser, symbol)
import qualified Text.Trifecta as Trifecta
import Text.Trifecta.Indentation
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Prettyprinter.Render.Terminal
import Prettyprinter (viaShow, layoutSmart, defaultLayoutOptions, hardline)

identStyle :: IdentifierStyle Parser
identStyle =
  IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = fromList ["let", "eff"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

commentStyle :: CommentStyle
commentStyle =
  CommentStyle
    { _commentStart = "-[-"
    , _commentEnd = "-]-"
    , _commentLine = "--"
    , _commentNesting = True
    }

ident :: Parser Text
ident = Token.ident identStyle

reserve :: Text -> Parser ()
reserve = Token.reserveText identStyle

ignoreLine :: TokenParsing f => f a -> f a
ignoreLine p = token (p <* skipOptional newline)

ignoreLines :: CharParsing f => f a -> f a
ignoreLines p = p <* many newline

symbol :: Text -> Parser Text
symbol = textSymbol

closure :: Parser (Term Text)
closure = between (symbolic '{') (localIndentation Any $ symbolic '}') body
 where 
   body = do
    args <- absoluteIndentation (some ident <* symbol "=>")
    e <- absoluteIndentation expr
    return $ foldr Abs e args

plet :: Parser (Term Text)
plet = do
  reserve "let"
  localIndentation Gt $ do
    name <- ident
    symbolic '='
    defn <- expr
    symbolic ';' <|> newline
    Let name defn <$> expr

term :: Parser (Term Text)
term =
  ((Num . fromIntegral <$> integer) <?> "number literal")
    <|> (closure <?> "closure")
    <|> ((Var <$> ident) <?> "identifier")
    <|> between (symbolic '(') (symbolic ')') (localIndentation Any expr)

expr :: Parser (Term Text)
expr = app <$> some1 term
 where
  app = \case
    term :| [] -> term
    terms -> foldl1 App terms

unParse :: Parser a -> Trifecta.Parser a
unParse = flip runReaderT False . unIndent . runParser

exprParser = unParse expr

parseExpr :: Text -> Result (Term Text)
parseExpr = parseText (unParse expr) mempty

replParse p txt =
  case parseText (unParse p) mempty txt of
    Success e -> putDoc (viaShow e <> hardline)
    Failure (ErrInfo doc _) -> putDoc (doc <> hardline)
