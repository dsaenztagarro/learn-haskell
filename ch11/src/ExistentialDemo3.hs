{-# LANGUAGE ExistentialQuantification #-}
module ExistentialDemo3 where

data CanBeShown = forall a. Show a => CanBeShown a

showWhatCanBeShown :: CanBeShown -> String
showWhatCanBeShown (CanBeShown value) = show value

instance Show CanBeShown where
  show (CanBeShown a) = show a

-- print [CanBeShown "hello", CanBeShown 12, CanBeShown True]
-- ["hello", 12, True]
