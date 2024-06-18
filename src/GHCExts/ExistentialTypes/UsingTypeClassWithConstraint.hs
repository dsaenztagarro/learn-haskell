{-# LANGUAGE ExistentialQuantification #-}

module GHCExts.ExistentialTypes.UsingTypeClassWithConstraint where

data CanBeShown = forall a. Show a => CanBeShown a

showWhatCanBeShown :: CanBeShown -> String
showWhatCanBeShown (CanBeShown value) = show value

instance Show CanBeShown where
  show (CanBeShown a) = show a
