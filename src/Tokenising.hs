{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tokenising (testParser, pSrc) where

-- import Data.ByteString (ByteString)
import Data.Char (isLetter)
import FlatParse.Stateful
import FlatParse.Stateful qualified as FlatParse

type Tokeniser = FlatParse.Parser ()

-- | Parse a line comment, end with a newline
lineComment :: Tokeniser ()
lineComment =
  optioned
    anyWord8
    ( \case
        10 {- '\n' -} -> put 0 >> ws
        _ -> modify (+ 1) >> lineComment
    )
    (pure ())

-- | Consume a whitespace char.
ws :: Tokeniser ()
ws =
  $( switch
       [|
         case _ of
           " " -> modify (+ 1) >> ws
           "\n" -> put 0 >> ws
           "\t" -> modify (+ 1) >> ws
           "\r" -> modify (+ 1) >> ws
           "--" -> lineComment
           _ -> pure ()
         |]
   )

token :: Tokeniser a -> Tokeniser a
token p = p <* ws
{-# INLINE token #-}

scanIdent :: Tokeniser ()
scanIdent = identStartChar >> manyIdentChars

-- | Read a starting character of an identifier.
identStartChar :: Tokeniser Char
identStartChar =
  fusedSatisfy
    isLatinLetter
    (\c -> isGreekLetter c || isLetter c)
    isLetter
    isLetter

inlineIdentChar :: Tokeniser Char
inlineIdentChar =
  fusedSatisfy
    (\c -> isLatinLetter c || FlatParse.isDigit c)
    (\c -> isGreekLetter c || isLetter c)
    isLetter
    isLetter
{-# INLINE inlineIdentChar #-}

manyIdentChars :: Tokeniser ()
manyIdentChars = many_ inlineIdentChar

-- ident :: Parser ByteString
ident :: Tokeniser Span
ident = spanOf $ spanned (identStartChar *> manyIdentChars) (\_ identSpan -> fails (isKeyword identSpan))

isKeyword :: Span -> Tokeniser ()
isKeyword identSpan = inSpan identSpan $ do
  $( FlatParse.switch
       [|
         case _ of
           "lam" -> pure ()
           "let" -> pure ()
           "in" -> pure ()
           "if" -> pure ()
           "then" -> pure ()
           "else" -> pure ()
         |]
   )
  eof

pSrc :: Tokeniser [Span]
pSrc = many (ws *> ident)

testParser :: Show a => Tokeniser a -> String -> IO ()
testParser p str = case packUTF8 str of
  b -> case runParser p 0 0 b of
    Err _e -> putStrLn "what"
    OK a _ _ -> print a
    Fail -> putStrLn "parse error!"
