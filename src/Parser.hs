{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (ParseTerm(..), ParseDefn(..), ParseProgram(..), parseProgram, printProgram, parseFile, printFile) where

import Control.Applicative.Combinators.NonEmpty (some)
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Char
import Data.HashMap.Strict
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Printf as Text

type Parser = Parsec Void Text

data ParseTerm
  = PVar Text
  | PNum Int
  | PAbs (NonEmpty Text) ParseTerm
  | PApp (NonEmpty ParseTerm)
  | PLet (HashMap Text ParseTerm) ParseTerm
  deriving (Show)

data ParseDefn = ParseDefn
  { name :: Text
  , args :: [Text]
  , body :: ParseTerm
  } deriving (Show)

newtype ParseProgram = ParseProgram (NonEmpty ParseDefn)
  deriving (Show)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword = lexeme . string

integer :: Parser Int
integer = lexeme L.decimal

reserved :: HashSet Text
reserved = HashSet.fromList ["let", "in"]

ident :: Parser Text
ident = lexeme $ do
  i <- satisfy (\c -> isAlpha c || c == '_')
  is <- many (alphaNumChar <|> char '-' <|> char '_')
  let id = pack (i : is)
  when (HashSet.member id reserved) (fail "Identifier cannot be a reserved word")
  return id

closure :: Parser ParseTerm
closure =
  PAbs
    <$> (symbol "\\" *> some ident)
    <*> (symbol "." *> expr)

plet :: Parser ParseTerm
plet =
  PLet
    <$> (keyword "let" *> defns)
    <*> (keyword "in" *> expr)
  where
    defns :: Parser (HashMap Text ParseTerm)
    defns = Data.HashMap.Strict.fromList . Data.List.NonEmpty.toList <$> some1 ((,) <$> (ident <* symbol "=") <*> expr)

term :: Parser ParseTerm
term =
  (PVar <$> ident)
    <|> (PNum <$> integer)
    <|> closure
    <|> between (symbol "(") (symbol ")") expr

expr :: Parser ParseTerm
expr = app <$> some term
  where
    -- If we only have one term it's not an app, otherwise make it an app
    app = \case
      term :| [] -> term
      terms -> PApp terms

defn :: Parser ParseDefn
defn =
  ParseDefn
    <$> ident
    <*> many ident
    <*> (symbol "=" *> expr)

program :: Parser ParseProgram
program = ParseProgram <$> some defn

parseProgram :: Text -> Either (ParseErrorBundle Text Void) ParseProgram
parseProgram = runParser program ""

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) ParseProgram)
parseFile filename = do
  contents <- TIO.readFile filename
  return $ runParser program filename contents

printFile :: FilePath -> IO ()
printFile filename = do
  t <- parseFile filename
  case t of
    Left error -> putStr $ errorBundlePretty error
    Right prog -> print prog

printProgram :: Text -> IO ()
printProgram source = case parseProgram source of
  Left error -> putStrLn $ errorBundlePretty error
  Right prog -> print prog
