{-
 -  HOPL/PROC/Lexer.hs
 -
 -  Reference implementation of the toy language PROC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for PROC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.PROC.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

procLexer :: Tok.TokenParser ()
procLexer =
  Tok.makeTokenParser $ procDef

procDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-"],
      Tok.reservedNames = ["let", "in", "if", "then", "else", "zero?", "proc"]
    }

integer :: Parser Integer
integer = Tok.integer procLexer

symbol :: String -> Parser String
symbol = Tok.symbol procLexer

parens :: Parser a -> Parser a
parens = Tok.parens procLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep procLexer

identifier :: Parser String
identifier = Tok.identifier procLexer

reserved :: String -> Parser ()
reserved = Tok.reserved procLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp procLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace procLexer
