{-
 -  HOPL/SIMPLE_STATEMENT/Lang/Parser.hs
 -
 -  Reference implementation of the toy language HOPL.SIMPLE_STATEMENT based
 -  on an exercise from the EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for the language.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.SIMPLE_STATEMENT.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.SIMPLE_STATEMENT.Lang.Lexer
import HOPL.SIMPLE_STATEMENT.Lang.Syntax (Exp (..), Pgm (..), Stmt (..))
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

{- Grammar for the SIMPLE_STATEMENT language -}

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
      BlockStmt
        <$> (reserved "var" >> sepBy identifier (symbol ","))
        <*> (symbol ";" >> statement)
    ]

expression :: Parser Exp
expression =
  (choice . map try)
    [ LetExp
        <$> (reserved "let" >> identifier)
        <*> (reservedOp "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> identifier)
        <*> parens identifier
        <*> (reservedOp "=" >> expression)
        <*> (reserved "in" >> expression),
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      CallExp
        <$> (symbol "(" >> expression)
        <*> (many expression <* symbol ")"),
      ProcExp
        <$> (reserved "proc" >> symbol "(" >> sepBy identifier (symbol ","))
        <*> (symbol ")" >> expression),
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
      LeftExp
        <$> (reserved "left" >> parens expression),
      RightExp
        <$> (reserved "right" >> parens expression),
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
