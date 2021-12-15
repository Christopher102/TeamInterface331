{-
 -  HOPL/LETREC/Parser.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for LETREC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.LETREC.Lang.Lexer
import HOPL.LETREC.Lang.Syntax (BinaryOp (..), Exp (..), Pgm (..), UnaryOp (..))
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
      UnpackExp
        <$> (reserved "unpack" >> many1 identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> identifier)
        <*> parens identifier
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      -- Control expressions
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      -- Function definition
      ProcExp
        <$> (reserved "proc" >> parens identifier)
        <*> expression,
      -- Function call
      uncurry CallExp
        <$> parens (pairOf expression),
      -- List constructors
      EmptyExp
        <$ reserved "emptylist",
      ListExp
        <$> (reserved "list" >> parens (sepBy expression (symbol ","))),
      -- Unary operations
      UnaryExp
        <$> unaryOperator <*> parens expression,
      uncurry . BinaryExp
        <$> binaryOperator
        <*> parens (sepPairOf expression ","),
      -- Boolean literals
      TrueExp
        <$ reserved "true",
      FalseExp
        <$ reserved "false",
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier
    ]

unaryOperator :: Parser UnaryOp
unaryOperator =
  (choice . map try)
    [ IsZero <$ reserved "zero?",
      IsNeg <$ reserved "neg?",
      IsPos <$ reserved "pos?",
      Not <$ reserved "not",
      Minus <$ reserved "minus",
      IsNull <$ reserved "null?",
      Car <$ reserved "car",
      Cdr <$ reserved "cdr"
    ]

binaryOperator :: Parser BinaryOp
binaryOperator =
  (choice . map try)
    [ Diff <$ reservedOp "-",
      Plus <$ reservedOp "+",
      Times <$ reservedOp "*",
      Divides <$ reservedOp "/",
      Mod <$ reservedOp "%",
      Equal <$ reservedOp "==",
      NotEqual <$ reservedOp "!=",
      Less <$ reservedOp "<",
      LessEqual <$ reservedOp "<=",
      Greater <$ reservedOp ">",
      GreaterEqual <$ reservedOp ">=",
      And <$ reservedOp "&&",
      Or <$ reservedOp "||",
      Cons <$ reserved "cons"
    ]

pairOf :: Parser a -> Parser (a, a)
pairOf p = (,) <$> p <*> p

sepPairOf :: Parser a -> String -> Parser (a, a)
sepPairOf p sep = (,) <$> p <*> (symbol sep >> p)
