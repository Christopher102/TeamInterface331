{-
 -  HOPL/STATPY/LANG/Lexer.hs
 -
 -  Implementation for STATPY Language
 -
 -  This module provides the lexical specification for STATPY
 -
 -  Authors: Brandon Alker, Nick Petrilli, and Christopher Fioti
 -}
module HOPL.STATPY.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

statpyLexer :: Tok.TokenParser ()
statpyLexer =
  Tok.makeTokenParser $ statpyDef

statpyDef =
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
          "^", 
          "%", 
          "sqrt", 
          ">", 
          "<", 
          "<=", 
          ">=", 
          "==", 
          "!=", 
          "not",
          ":"
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
          "for",
          "sqrt"
        ]
    }

integer :: Parser Integer
integer = Tok.integer statpyLexer

symbol :: String -> Parser String
symbol = Tok.symbol statpyLexer

parens :: Parser a -> Parser a
parens = Tok.parens statpyLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep statpyLexer

identifier :: Parser String
identifier = Tok.identifier statpyLexer

reserved :: String -> Parser ()
reserved = Tok.reserved statpyLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp statpyLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace statpyLexer