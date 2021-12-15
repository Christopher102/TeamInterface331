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
module HOPL.CHECKED_STATEMENT.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

checkedStmtLexer :: Tok.TokenParser ()
checkedStmtLexer =
  Tok.makeTokenParser $ checkedStmtDef

checkedStmtDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames =
        [ "=",
          "-",
          "+",
          "*",
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
          "set",
          "begin",
          "end",
          "newpair",
          "unpair",
          "setleft",
          "setright",
          "print",
          "while",
          "var",
          "not",
          "int",
          "bool",
          "pairof"
        ]
    }

integer :: Parser Integer
integer = Tok.integer checkedStmtLexer

symbol :: String -> Parser String
symbol = Tok.symbol checkedStmtLexer

parens :: Parser a -> Parser a
parens = Tok.parens checkedStmtLexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets checkedStmtLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep checkedStmtLexer

identifier :: Parser String
identifier = Tok.identifier checkedStmtLexer

reserved :: String -> Parser ()
reserved = Tok.reserved checkedStmtLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp checkedStmtLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace checkedStmtLexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral checkedStmtLexer
