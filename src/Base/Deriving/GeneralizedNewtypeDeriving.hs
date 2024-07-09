{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base.Deriving.GeneralizedNewtypeDeriving where

newtype USD = USD { getMillis :: Integer } deriving (Eq,Ord,Show)
