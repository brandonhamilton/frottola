module Language.Frottola.Parser where

import Control.Applicative
import qualified Data.HashSet as HS
import Data.Text
import Language.Frottola.Syntax
import Text.Parsix
import Text.Parser.Expression
import Text.Parser.Token.Style
import qualified Text.Parser.Token.Highlight as Highlight

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> oneOf "_"
  , _styleLetter = alphaNum <|> oneOf "-'"
  , _styleReserved = HS.fromList ["def", "extern"]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

opsTable :: (Monad m, TokenParsing m) => [[Operator m Expr]]
opsTable = 
  [ [binary "*" Multiply AssocLeft, binary "/" Divide AssocLeft ]
  , [binary "+" Add AssocLeft, binary "-" Subtract AssocLeft ]
  ]
  where
    binary sym op = Infix (BinOp op <$ reserve emptyOps sym <?> "binary operation")

identifier :: (Monad m, TokenParsing m) => m Text
identifier = ident style

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = reserveText style

expr :: (Monad m, TokenParsing m) => m Expr
expr = buildExpressionParser opsTable factor <?> "expression"

floating :: TokenParsing m => m Expr
floating = Float . either fromIntegral id <$> integerOrDouble <?> "float"

variable :: (Monad m, TokenParsing m) => m Expr
variable = Var <$> identifier <?> "variable"

function :: (Monad m, TokenParsing m) => m Expr
function = reserved "def" >> Function <$> identifier <*> parens (many variable) <*> expr <?> "function"

extern :: (Monad m, TokenParsing m) => m Expr
extern = reserved "extern" >> Extern <$> identifier <*> parens (many variable) <?> "extern"

call :: (Monad m, TokenParsing m) => m Expr
call = Call <$> identifier <*> parens (commaSep expr) <?> "call"

factor :: (Monad m, TokenParsing m) => m Expr
factor = choice [ try floating, try extern, try function, try call, variable, parens expr ] <?> "factor"

defn :: (Monad m, TokenParsing m) => m Expr
defn = choice [ try extern, try function, expr ] <?> "definition"

program :: (Monad m, TokenParsing m) => m Program
program = many (defn <* reserve emptyOps ";")

parseProgram :: Text -> Result Program
parseProgram p = parseText (token program <* eof) p "<interactive>"
