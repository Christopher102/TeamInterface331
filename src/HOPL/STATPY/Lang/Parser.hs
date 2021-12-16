{-
 -  HOPL/STATPY/LANG/Parser.hs

 -  This module provides the grammatical specification for STATPY.
 -
 -  Author: Brandon Alker, Christopher Fioti, and Nicholas Petrilli 
 -}
module HOPL.STATPY.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.STATPY.Lang.Lexer
import HOPL.STATPY.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.STATPY.Type
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
  [
    -- Control expressions
    IfExp
      <$> (reserved "if" >> expression <* symbol ":")
      <*> expression
      <*> (reserved "else" >> expression),
    -- Function definition
    DefExp
      <$> typeAnnotation
      <*> (reserved "def" >> identifier)
      <*> (symbol ":" >> expression),
    -- Function call
    CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
    -- Arithmetic operators
    DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
    -- Arithmetic/numeric predicates
    AddExp
      <$> (expression <* reservedOp "+")
      <*> expression,
    --Mult Exp
    MultExp
      <$> (expression <* reservedOp "*")
      <*> expression,
    DivExp
      <$> (expression <* reservedOp "/")
      <*> expression,
    ExpoExp
      <$> (expression <* reservedOp "**") 
      <*> expression,
    ModExp
      <$> (expression <* reservedOp "%")
      <*> expression,
    SqrtExp
      <$> (reserved "sqrt" >> symbol "(" >> expression <* symbol ")"),
    GreaterExp
      <$> (expression <* reservedOp ">")
      <*> expression,
    LessExp
      <$> (expression <* reservedOp "<")
      <*> expression,
    GreatEqExp
      <$> (expression <* reservedOp ">=")
      <*> expression,
    LessEqExp
      <$> (expression <* reservedOp "<=")
      <*> expression,
    EqualExp
      <$> (expression <* reservedOp "==")
      <*> expression,
    NotEqualExp
      <$> (expression <* reservedOp "!=")
      <*> expression,
    NotExp
      <$> (reserved "not" >>  expression),
    IsZeroExp
      <$> (reserved "zero?" >> symbol "(" >> expression <* symbol ")"),
    EmptyListExp
      <$> (typeAnnotation <* symbol "[" <* symbol "]"),
    ListExp
        <$> (typeAnnotation <* reserved "list" >> symbol "[" >> sepBy expression (symbol ",") <* symbol "]"),
    -- Integer literal
    ConstExp
      <$> integer,
    -- Variable reference
    VarExp
      <$> identifier
  ]

typeAnnotation :: Parser Type
typeAnnotation =
  (choice . map try)
    [ IntType
        <$ reserved "int",
      BoolType
        <$ reserved "bool",
      StringType
        <$ reserved "str",
      CharType
        <$ reserved "char",
      ListType
        <$ reserved "list",
      DefType
        <$> (symbol "(" >> typeAnnotation)
        <*> (reservedOp "->" >> typeAnnotation <* symbol ")")
    ]