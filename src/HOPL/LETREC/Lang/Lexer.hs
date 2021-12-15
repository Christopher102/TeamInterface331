{-
 -  HOPL/LETREC/Lexer.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for LETREC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

letrecLexer :: Tok.TokenParser ()
letrecLexer =
  Tok.makeTokenParser $ letrecDef

letrecDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames =
        [ "=",
          "-",
          "+",
          "*",
          "/",
          "%",
          "==",
          "!=",
          "<",
          ">",
          "<=",
          ">=",
          "&&",
          "||"
        ],
      Tok.reservedNames =
        [ "let",
          "in",
          "if",
          "then",
          "else",
          "zero?",
          "proc",
          "letrec",
          "minus",
          "pos?",
          "neg?",
          "not",
          "true",
          "false",
          "emptylist",
          "null?",
          "cons",
          "car",
          "cdr",
          "list",
          "unpack"
        ]
    }

integer :: Parser Integer
integer = Tok.integer letrecLexer

symbol :: String -> Parser String
symbol = Tok.symbol letrecLexer

parens :: Parser a -> Parser a
parens = Tok.parens letrecLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep letrecLexer

identifier :: Parser String
identifier = Tok.identifier letrecLexer

reserved :: String -> Parser ()
reserved = Tok.reserved letrecLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp letrecLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace letrecLexer
