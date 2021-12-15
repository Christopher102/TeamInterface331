{-
 -  HOPL/CALL_BY_REFERENCE/Lang/Lexer.hs
 -
 -  Reference implementation of the toy language CALL_BY_REFERENCE from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for CALL_BY_REFERENCE.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_REFERENCE.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

callByReferenceLexer :: Tok.TokenParser ()
callByReferenceLexer =
  Tok.makeTokenParser $ callByReferenceDef

callByReferenceDef =
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
integer = Tok.integer callByReferenceLexer

symbol :: String -> Parser String
symbol = Tok.symbol callByReferenceLexer

parens :: Parser a -> Parser a
parens = Tok.parens callByReferenceLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep callByReferenceLexer

identifier :: Parser String
identifier = Tok.identifier callByReferenceLexer

reserved :: String -> Parser ()
reserved = Tok.reserved callByReferenceLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp callByReferenceLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace callByReferenceLexer
