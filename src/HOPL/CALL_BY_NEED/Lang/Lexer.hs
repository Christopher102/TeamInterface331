{-
 -  HOPL/CALL_BY_NEED/Lang/Lexer.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for CALL_BY_NEED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_NEED.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

callByNeedLexer :: Tok.TokenParser ()
callByNeedLexer =
  Tok.makeTokenParser $ callByNeedDef

callByNeedDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-"],
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
          "setright"
        ]
    }

integer :: Parser Integer
integer = Tok.integer callByNeedLexer

symbol :: String -> Parser String
symbol = Tok.symbol callByNeedLexer

parens :: Parser a -> Parser a
parens = Tok.parens callByNeedLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep callByNeedLexer

identifier :: Parser String
identifier = Tok.identifier callByNeedLexer

reserved :: String -> Parser ()
reserved = Tok.reserved callByNeedLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp callByNeedLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace callByNeedLexer
