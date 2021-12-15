{-
 -  HOPL/LET/Lexer.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the lexical specification for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

letLexer :: Tok.TokenParser ()
letLexer =
  Tok.makeTokenParser $ letDef

letDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-"],
      Tok.reservedNames = ["let", "in", "if", "then", "else", "zero?"]
    }

integer :: Parser Integer
integer = Tok.integer letLexer

symbol :: String -> Parser String
symbol = Tok.symbol letLexer

parens :: Parser a -> Parser a
parens = Tok.parens letLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep letLexer

identifier :: Parser String
identifier = Tok.identifier letLexer

reserved :: String -> Parser ()
reserved = Tok.reserved letLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp letLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace letLexer
