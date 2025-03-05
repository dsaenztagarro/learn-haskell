{-# LANGUAGE OverloadedStrings #-}
module Libs.Mtl.Control.Monad.StateTSpec where

import Test.Hspec

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Libs.Mtl.Control.Monad.ExceptT
import Libs.Mtl.Control.Monad.StateT

type ParseError = Text
type ParseState = Text

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
  describe "StateT containting Except" $ do
    it "parses alternatives" $ do
      runParser "123" ((char '1' >> parseChar) <|> parseChar) `shouldBe` Right '2'
      runParser "123" ((char 'a' >> parseChar) <|> parseChar) `shouldBe` Right '1'

    it "parses many" $ do
      runParser "123" (many parseChar) `shouldBe` Right "123"
