{-
 -  HOPL/IMPLICIT_REFS/Lexer.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for IMPLICIT_REFS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.IMPLICIT_REFS.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

implicitRefsLexer :: Tok.TokenParser ()
implicitRefsLexer =
  Tok.makeTokenParser $ implicitRefsDef

implicitRefsDef =
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
          "end"
        ]
    }

integer :: Parser Integer
integer = Tok.integer implicitRefsLexer

symbol :: String -> Parser String
symbol = Tok.symbol implicitRefsLexer

parens :: Parser a -> Parser a
parens = Tok.parens implicitRefsLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep implicitRefsLexer

identifier :: Parser String
identifier = Tok.identifier implicitRefsLexer

reserved :: String -> Parser ()
reserved = Tok.reserved implicitRefsLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp implicitRefsLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace implicitRefsLexer
