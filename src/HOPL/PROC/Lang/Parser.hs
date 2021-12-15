{-
 -  HOPL/PROC/Parser.hs
 -
 -  Reference implementation of the toy language PROC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for PROC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.PROC.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.PROC.Lang.Lexer
import HOPL.PROC.Lang.Syntax (Exp (..), Pgm (..))
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

{- Grammar for the PROC language -}

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
      CallExp <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      ProcExp <$> (reserved "proc" >> parens identifier)
        <*> expression,
      DiffExp <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsZeroExp <$> (reserved "zero?" >> parens expression),
      ConstExp <$> integer,
      VarExp <$> identifier
    ]
