-- Allow type signatures which appear that they would result in an unusable binding.
{-# LANGUAGE AllowAmbiguousTypes #-}
module GHCExt.AllowAmbiguousTypes where

f :: Show a => b -> c
f = undefined
