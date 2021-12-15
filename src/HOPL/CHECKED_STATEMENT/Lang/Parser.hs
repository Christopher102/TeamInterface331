{-
 -  HOPL/CHECKED/Parser.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED_STATEMENT.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.CHECKED_STATEMENT.Lang.Lexer
import HOPL.CHECKED_STATEMENT.Lang.Syntax (Exp (..), Pgm (..), Stmt (..))
import HOPL.CHECKED_STATEMENT.Type
import Text.Parsec (ParseError, choice, eof, many, parse, sepBy, try)
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
program = Pgm <$> statement

statement :: Parser Stmt
statement =
  (choice . map try)
    [ AssignStmt
        <$> identifier
        <*> (reservedOp "=" >> expression),
      PrintStmt
        <$> (reserved "print" >> expression),
      MultiStmt
        <$> (symbol "{" >> sepBy statement (symbol ";") <* symbol "}"),
      IfStmt
        <$> (reserved "if" >> expression)
        <*> statement
        <*> statement,
      WhileStmt
        <$> (reserved "while" >> expression)
        <*> statement,
      uncurry BlockStmt
        <$> (reserved "var" >> (unzip <$> sepBy declaration (symbol ",")))
        <*> (symbol ";" >> statement)
    ]

expression :: Parser Exp
expression =
  (choice . map try)
    [ LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> typeAnnotation)
        <*> identifier
        <*> (symbol "(" >> identifier)
        <*> (symbol ":" >> typeAnnotation <* symbol ")")
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      UnpairExp
        <$> (reserved "unpair" >> identifier)
        <*> identifier
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      uncurry ProcExp
        <$> (reserved "proc" >> parens declaration)
        <*> expression,
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      ProdExp
        <$> (reservedOp "*" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      SumExp
        <$> (reservedOp "+" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      NotExp
        <$> (reserved "not" >> parens expression),
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      BeginExp
        <$> (reserved "begin" >> sepBy expression (symbol ";") <* reserved "end"),
      AssignExp
        <$> (reserved "set" >> identifier)
        <*> (reservedOp "=" >> expression),
      NewPairExp
        <$> (reserved "newpair" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      SetLeftExp
        <$> (reserved "setleft" >> expression)
        <*> (reservedOp "=" >> expression),
      SetRightExp
        <$> (reserved "setright" >> expression)
        <*> (reservedOp "=" >> expression),
      StrExp
        <$> stringLiteral,
      ConstExp
        <$> integer,
      VarExp
        <$> identifier
    ]

declaration :: Parser (String, Type)
declaration =
  (choice . map try)
    [(,) <$> identifier <*> (symbol ":" >> typeAnnotation)]

typeAnnotation :: Parser Type
typeAnnotation =
  (choice . map try)
    [ IntType
        <$ reserved "int",
      BoolType
        <$ reserved "bool",
      StrType
        <$ reserved "str",
      ProcType
        <$> (symbol "(" >> typeAnnotation)
        <*> (reservedOp "->" >> typeAnnotation <* symbol ")"),
      PairType
        <$> (reserved "pairof" >> symbol "(" >> typeAnnotation)
        <*> (reservedOp "*" >> typeAnnotation <* symbol ")")
    ]
