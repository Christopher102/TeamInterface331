{-
 -  HOPL/LET/Parser.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the grammatical specification for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.LET.Lang.Lexer
import HOPL.LET.Lang.Syntax (Exp (..), Pgm (..))
import Text.Parsec (ParseError, choice, eof, parse, try)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

parseToplevel :: String -> Either ParseError Pgm
parseToplevel = parse (contents toplevel) "<stdin>"

toplevel :: Parser Pgm
toplevel = program

parseExp :: String -> Either ParseError Exp
parseExp = parse (contents expression) "<stdin>"

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the LET language -}

program :: Parser Pgm
program = Pgm <$> expression

expression :: Parser Exp
expression =
  (choice . map try)
    [ LetExp <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      IfExp <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      DiffExp <$> (reserved "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsZeroExp <$> (reserved "zero?" >> parens expression),
      ConstExp <$> integer,
      VarExp <$> identifier
    ]
