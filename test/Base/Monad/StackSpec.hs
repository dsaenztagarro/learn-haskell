{-# LANGUAGE OverloadedStrings #-}
module Base.Monad.StackSpec where

import Test.Hspec

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Base.Monad.ExceptT
import Base.Monad.StateT

type ParseError = Text
type ParseState = Text

-------------------------------------
-- Nesting State Inside of ExcepT
-------------------------------------

type Parser' = ExceptT ParseError (State ParseState)

runParser' :: Text -> Parser' a -> Either ParseError a
runParser' input parser =
  evalState (runExceptT parser) input

parseChar' :: Parser' Char
parseChar' = do
  parseState <- succeed get
  case Text.uncons parseState of
    Nothing -> throwError "end of input"
    Just (c, rest) -> do
      succeed $ put rest
      pure c

char' :: Char -> Parser' ()
char' expectedChar = do
  actualChar <- parseChar'
  when (expectedChar /= actualChar) $
    throwError "Invalid character"

-------------------------------------
-- Nesting Except Inside of StateT
-------------------------------------

type Parser = StateT ParseState (Except ParseError)

runParser :: Text -> Parser a -> Either ParseError a
runParser input parser =
  runExcept $ evalStateT parser input

parseChar :: Parser Char
parseChar = do
  parseState <- get
  case Text.uncons parseState of
    Nothing -> liftStateT $ throwError "end of input"
    Just (c, rest) -> do
      put rest
      pure c

char :: Char -> Parser ()
char expectedChar = do
  actualChar <- parseChar
  when (expectedChar /= actualChar) $
    liftStateT $ throwError "Invalid character"

spec :: Spec
spec = do
  describe "State inside of ExceptT" $ do
    it "parses alternatives" $ do
      runParser' "123" parseChar' `shouldBe` Right '1'
      runParser' "123" (parseChar' >> parseChar') `shouldBe` Right '2'
      runParser' "abc" ((char' 'a' >> parseChar') <|> parseChar') `shouldBe` Right 'b'
      -- runParser' "abc" ((char' '1' >> parseChar') <|> parseChar') `shouldBe` Left "Invalid character"

  describe "Except inside of StateT" $ do
    it "parses alternatives" $ do
      runParser "123" ((char '1' >> parseChar) <|> parseChar) `shouldBe` Right '2'
      runParser "123" ((char 'a' >> parseChar) <|> parseChar) `shouldBe` Right '1'
