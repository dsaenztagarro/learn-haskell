{-# LANGUAGE OverloadedStrings #-}
module Base.Monad.ExceptTSpec where

import Test.Hspec

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Base.Monad.ExceptT
import Base.Monad.StateT

type ParseError = Text
type ParseState = Text

type Parser = ExceptT ParseError (State ParseState)

runParser :: Text -> Parser a -> Either ParseError a
runParser input parser =
  evalState (runExceptT parser) input

parseChar :: Parser Char
parseChar = do
  parseState <- succeed get
  case Text.uncons parseState of
    -- Nothing -> throwError "end of input"
    Nothing -> pure 'a'
    Just (c, rest) -> do
      succeed $ put rest
      pure c

char :: Char -> Parser ()
char expectedChar = do
  actualChar <- parseChar
  when (expectedChar /= actualChar) $
    throwError "Invalid character"

{-
`parseChar` updates the state with `put rest`, and if `char` then `throwError`,
the `Alternative` parser will NOT start from the initial state.

LSON: the choice of how we define our monad transformer stack can have other
implications too. Another example we can demostrate with `Alternative` is the
order in which we nest our transformers is impacted by the lazyness of the
computations we're nesting.
-}

spec :: Spec
spec = do
  describe "ExceptT containing State" $ do
    it "parses char" $ do
      runParser "123" parseChar `shouldBe` Right '1'
      runParser "123" (parseChar >> parseChar) `shouldBe` Right '2'
      runParser "abc" (char 'a' >> parseChar) `shouldBe` Right 'b'
      runParser "abc" (char '1' >> parseChar) `shouldBe` Left "Invalid character"

    it "parses alternatives" $ do
      runParser "123" ((char '1' >> parseChar) <|> parseChar) `shouldBe` Right '2'
      runParser "abc" ((char '1' >> parseChar) <|> parseChar) `shouldBe` Right 'b'

      -- INFINITE!
      -- runParser "abc" (many parseChar) `shouldBe` Right "ab"
