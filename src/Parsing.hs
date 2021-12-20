{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import AST
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Functor.Foldable.TH (baseRulesType)
import Data.HashSet (fromList)
import Data.List.NonEmpty (NonEmpty ((:|)), some1)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace
import GHC.Generics (Par1 (unPar1))
import Parser (Parser, newlineSensitive, runParser)
import Prettyprinter (defaultLayoutOptions, hardline, layoutSmart, viaShow)
import Prettyprinter.Render.Terminal
import qualified Text.Parser.Token as Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta hiding (Parser, ident, reserve, runParser, symbol)
import qualified Text.Trifecta as Trifecta
import Text.Trifecta.Indentation
import Ty

identStyle :: IdentifierStyle Parser
identStyle =
  IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = fromList ["let", "eff", "fn", "in", "where"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

tyStyle :: IdentifierStyle Parser
tyStyle =
  IdentifierStyle
    { _styleName = "type name"
    , _styleStart = upper
    , _styleLetter = alphaNum
    , _styleReserved = fromList ["Int"]
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

symbol :: Text -> Parser Text
symbol = textSymbol

baseTy :: Parser BaseTy
baseTy = BaseInt <$ reserve "Int"

atomicTy :: Parser Ty
atomicTy =
  (Base <$> baseTy)
    <|> (TyVar <$> ident)
    <|> parens (localIndentation Any ty)

ty :: Parser Ty
ty = foldr1 Fun <$> funTys
 where
  funTys = (:) <$> atomicTy <*> many (indent $ symbol "->" *> ty)
  indent = localIndentation Ge . absoluteIndentation

closure :: Parser (Term Text)
closure =
  between (symbolic '{') (localIndentation Any $ symbolic '}') $
    localIndentation Gt body
 where
  body = do
    args <- some ident
    symbol "=>"
    e <- expr
    return $ foldr Abs e args

plet :: Parser (Term Text)
plet = do
  reserve "let"
  name <- ident
  ty <- optional (symbolic ':' *> ty)
  symbolic '='
  defn <- localIndentation Gt (absoluteIndentation expr)
  Let name (maybe defn (Annotate defn) ty) <$> localIndentation Eq (absoluteIndentation expr)

term :: Parser (Term Text)
term =
  choice
    [ Num . fromIntegral <$> integer <?> "number literal"
    , closure <?> "closure"
    , plet <?> "let"
    , Var <$> ident <?> "identifier"
    , between (symbolic '(') (symbolic ')') (localIndentation Any expr)
    ]

expr :: Parser (Term Text)
expr = app <$> some1 term
 where
  app = \case
    term :| [] -> term
    terms -> foldl1 App terms

fnDefn :: Parser (FnDefn Text)
fnDefn = localTokenMode (const Gt) $ do
  localTokenMode (const Ge) $ reserve "fn"
  name <- ident
  sig <- symbolic ':' *> localIndentation Gt (absoluteIndentation ty)
  constraints <- optional (pwhere *> some constraint)
  eq
  body <- localIndentation Gt (absoluteIndentation expr)
  return $ FnDefn name [] sig (fromMaybe [] constraints) body
 where
  eq = localTokenMode (const Ge) (symbolic '=')
  pwhere = localTokenMode (const Ge) (reserve "where")
  constraint = do
    var <- ident
    symbolic ':'
    classes <- sepBy1 ident (symbolic ',')
    return (var, classes)

effDefn :: Parser EffDefn
effDefn = do
  reserve "eff"
  name <- ident
  args <- some ident
  reserve "where"
  methods <- localIndentation Gt (some $ absoluteIndentation signature)
  return $ EffDefn name args methods
 where
  signature = do
    name <- ident
    symbolic ':'
    sig <- ty
    return (name, sig)

program :: Parser (Program Text)
program = foldr build (Program [] []) <$> some1 (choice [Left <$> effDefn, Right <$> fnDefn])
 where
  build (Left eff) (Program fns effs) = Program fns (eff : effs)
  build (Right fn) (Program fns effs) = Program (fn : fns) effs

unParse :: Parser a -> Trifecta.Parser a
unParse = flip runReaderT False . unIndent . runParser

parseExpr :: Text -> Result (Term Text)
parseExpr = parseText (unParse (skipMany space *> expr)) mempty

parseTy :: Text -> Result Ty
parseTy = parseText (unParse (skipMany space *> ty)) mempty

parseFnDefn :: Text -> Result (FnDefn Text)
parseFnDefn = parseText (unParse (skipMany space *> fnDefn)) mempty

parseEffDefn :: Text -> Result EffDefn
parseEffDefn = parseText (unParse (skipMany space *> effDefn)) mempty

parseProgram :: Text -> Result (Program Text)
parseProgram = parseText (unParse (skipMany space *> program)) mempty

replParse p txt =
  case parseText (unParse p) mempty txt of
    Success e -> putDoc (viaShow e <> hardline)
    Failure (ErrInfo doc _) -> putDoc (doc <> hardline)
