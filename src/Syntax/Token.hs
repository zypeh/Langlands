{-# LANGUAGE PatternSynonyms #-}

module Syntax.Token where

import Data.ByteString (ByteString)

tokLineComment :: ByteString
tokLineComment = "--"

tokDocComment :: ByteString
tokDocComment = "--|"

tokBar :: ByteString
tokBar = "|"

tokLet :: ByteString
tokLet = "let"

tokIn :: ByteString
tokIn = "in"

tokComma :: ByteString
tokComma = ","

tokSemiColon :: ByteString
tokSemiColon = ";"

tokColon :: ByteString
tokColon = ":"

tokAutoAssign :: ByteString
tokAutoAssign = ":="

tokLParen, tokRParen :: ByteString
tokLParen = "("
tokRParen = ")"

tokLBracket, tokRBracket :: ByteString
tokLBracket = "["
tokRBracket = "]"

tokLBrace, tokRBrace :: ByteString
tokLBrace = "{"
tokRBrace = "}"
