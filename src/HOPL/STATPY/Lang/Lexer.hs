{-
 -  Lang/Lexer.hs
 -
 -  Implementation for Statpy Language
 -
 -  This module provides the lexical specification for StatPy
 -
 -  Authors: Brandon Alker, Nick Petrilli, and Christopher Fioti
 -}
module HOPL.STATPY.Lang.Lexer where

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
    { Tok.commentLine = "//",
      Tok.commentStart = "/*",
      Tok.commentEnd = "*/",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames =
        [ "=",
          "-",
          "+", 
          "-", 
          "*", 
          "/", 
          "**", 
          "%", 
          "sqrt", 
          ">", 
          "<", 
          "<=", 
          ">=", 
          "==", 
          "!=", 
          "not" 
        ],
      Tok.reservedNames =
        [ "int",
          "bool",
          "string",
          "def",
          "char",
          "list",
          "emptyList",
          "null",
          "if",
          "else",
          "for",
          "while",
          "isZero",
          "head",
          "tail",
          "import",
          "for"
        ]
    }

integer :: Parser Integer
integer = Tok.integer checkedLexer

symbol :: String -> Parser String
symbol = Tok.symbol checkedLexer

parens :: Parser a -> Parser a
parens = Tok.parens checkedLexer

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