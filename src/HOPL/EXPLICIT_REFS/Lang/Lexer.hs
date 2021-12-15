{-
 -  HOPL/EXPLICIT_REFS/Lexer.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for EXPLICIT_REFS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.EXPLICIT_REFS.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

explicitRefsLexer :: Tok.TokenParser ()
explicitRefsLexer =
  Tok.makeTokenParser $ explicitRefsDef

explicitRefsDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-"],
      Tok.reservedNames = [
        "let", "in", "if", "then", "else", "zero?", "proc", "letrec", "newref", "deref", "setref", "begin", "end"
        ]
    }

integer :: Parser Integer
integer = Tok.integer explicitRefsLexer

symbol :: String -> Parser String
symbol = Tok.symbol explicitRefsLexer

parens :: Parser a -> Parser a
parens = Tok.parens explicitRefsLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep explicitRefsLexer

identifier :: Parser String
identifier = Tok.identifier explicitRefsLexer

reserved :: String -> Parser ()
reserved = Tok.reserved explicitRefsLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp explicitRefsLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace explicitRefsLexer
