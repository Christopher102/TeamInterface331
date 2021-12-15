{-
 -  HOPL/SIMPLE_STATEMENT/Lang/Lexer.hs
 -
 -  Reference implementation of the toy language HOPL.SIMPLE_STATEMENT based
 -  on an exercise from the EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for the language.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.SIMPLE_STATEMENT.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

simpleStatementLexer :: Tok.TokenParser ()
simpleStatementLexer =
  Tok.makeTokenParser $ simpleStatementDef

simpleStatementDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-", "+", "*"],
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
          "left",
          "right",
          "setleft",
          "setright",
          "print",
          "while",
          "var",
          "not"
        ]
    }

integer :: Parser Integer
integer = Tok.integer simpleStatementLexer

symbol :: String -> Parser String
symbol = Tok.symbol simpleStatementLexer

parens :: Parser a -> Parser a
parens = Tok.parens simpleStatementLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep simpleStatementLexer

identifier :: Parser String
identifier = Tok.identifier simpleStatementLexer

reserved :: String -> Parser ()
reserved = Tok.reserved simpleStatementLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp simpleStatementLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace simpleStatementLexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral simpleStatementLexer
