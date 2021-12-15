{-
 -  HOPL/CHECKED/Lexer.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

checkedLexer :: Tok.TokenParser ()
checkedLexer =
  Tok.makeTokenParser $ checkedDef

checkedDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames =
        [ "=",
          "-",
          "->"
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
          "int",
          "bool"
        ]
    }

integer :: Parser Integer
integer = Tok.integer checkedLexer

symbol :: String -> Parser String
symbol = Tok.symbol checkedLexer

parens :: Parser a -> Parser a
parens = Tok.parens checkedLexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets checkedLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep checkedLexer

identifier :: Parser String
identifier = Tok.identifier checkedLexer

reserved :: String -> Parser ()
reserved = Tok.reserved checkedLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp checkedLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace checkedLexer
