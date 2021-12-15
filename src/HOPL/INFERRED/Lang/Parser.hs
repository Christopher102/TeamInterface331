{-
 -  HOPL/INFERRED/Parser.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for INFERRED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.INFERRED.Lang.Lexer
import HOPL.INFERRED.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.INFERRED.Type
import Text.Parsec (ParseError, choice, eof, many1, parse, sepBy, try)
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
    [ -- Variable declarations
      LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> optionalType)
        <*> identifier
        <*> (symbol "(" >> identifier)
        <*> (symbol ":" >> optionalType <* symbol ")")
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      -- Control expressions
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      -- Function definition
      ProcExp
        <$> (reserved "proc" >> symbol "(" >> identifier)
        <*> (symbol ":" >> optionalType)
        <*> (symbol ")" >> expression),
      -- Function call
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      -- Arithmetic operators
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arithmetic/numeric predicates
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier
    ]

optionalType :: Parser OptionalType
optionalType =
  (choice . map try)
    [ NoType
        <$ reservedOp "?",
      AType <$> typeAnnotation
    ]

typeAnnotation :: Parser Type
typeAnnotation =
  (choice . map try)
    [ IntType
        <$ reserved "int",
      BoolType
        <$ reserved "bool",
      ProcType
        <$> (symbol "(" >> typeAnnotation)
        <*> (reservedOp "->" >> typeAnnotation <* symbol ")")
    ]
