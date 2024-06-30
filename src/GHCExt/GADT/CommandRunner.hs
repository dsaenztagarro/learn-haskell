-- Promotes data constructors to type level constructors
{-# LANGUAGE DataKinds #-}
-- Allows defining generalized algebraic data types
{-# LANGUAGE GADTs #-}
-- Enable type-level programming features
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Provide fine-grained control over type variables and allow explicit type application
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Provide more flexibility on defining type classes and instances
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module GHCExt.GADT.CommandRunner where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHCExt.GADT.ShellCmd

{- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html
 
  See 6.4.16.2. Ordering of specified variables

  - If an identifier’s type has a `forall`, then the order of type variables as 
    written in the forall is retained.

  - If any of the variables depend on other variables (that is, if some of the 
    variables are kind variables), the variables are reordered so that kind 
    variables come before type variables, preserving the left-to-right order as 
    much as possible. That is, GHC performs a stable topological sort on the 
    variables.

    h :: Proxy (a :: (j, k)) -> Proxy (b :: Proxy a) -> ()
    -- as if h :: forall j k a b. ...
    
    In this example, `a` depends on `j` and `k`, and `b` depends on `a`. Even 
    though `a` appears lexically before `j` and `k`, `j` and `k` are quantified 
    first, because `a` depends on `j` and `k`. Note further that `j` and `k` 
    are not reordered with respect to each other, even though doing so would 
    not violate dependency conditions.

  - Class methods’ type arguments include the class type variables, followed by 
    any variables an individual method is polymorphic in. So, 

    `class Monad m where return :: a -> m a` 

    means that return’s type arguments are `m, a`.

  - With the `RankNTypes` extension...
  
  -----------------------------------------------------------------------------
  
  While type application is commonly associated with specifying type class 
  instances, its use in this context is purely about providing explicit type 
  arguments to a polymorphic function or constructor. 

  In the case of AddCommand, the type application provides the specific Symbol 
  to be used for the name parameter, ensuring that it adheres to the KnownSymbol 
  constraint.


  ghci> :m +GHCExt.GADT.CommandRunner
  ghci> :m +GHCExt.GADT.ShellCmd
  ghci> :set -XTypeApplications
  ghci> :set -XDataKinds
  ghci> :set -fprint-explicit-foralls

  ghci> :t AddCommand
  AddCommand
    :: forall (name :: ghc-prim:GHC.Types.Symbol) a1 b1
              (names :: [ghc-prim:GHC.Types.Symbol]) (commands :: [*]).
       GHC.TypeLits.KnownSymbol name =>
       ShellCmd a1 b1
       -> CommandSet names commands
       -> CommandSet (name : names) (ShellCmd a1 b1 : commands)

  ghci> :t AddCommand @"ls" listDirectory EmptyCommandSet
  AddCommand @"ls" listDirectory EmptyCommandSet
    :: CommandSet '["ls"] '[ShellCmd FilePath [FilePath]]
-}

data CommandSet :: [Symbol] -> [Type] -> Type where
  EmptyCommandSet :: CommandSet '[] '[]
  AddCommand ::
    KnownSymbol name =>
    ShellCmd a b ->
    CommandSet names commands ->
    CommandSet (name:names) (ShellCmd a b : commands)

commands =
  AddCommand @"ls" listDirectory $ 
  addLiteral @"free" "free -h" $
  addLiteral @"uptime" "uptime" $
  AddCommand @"cat" printFile $
  addLiteral @"uname" "uname -a" $ EmptyCommandSet
  where
    addLiteral :: 
      forall name {names} {commands}. KnownSymbol name =>
      String ->
      CommandSet names commands ->
      CommandSet (name : names) (ShellCmd () String : commands)
    addLiteral command = AddCommand (literal command)

    literal :: String -> ShellCmd () String
    literal shellCommand =
      RunCommand (ProgName "bash") args outputFunc
      where
        args = const $ ProgArgs ["-c", shellCommand]
        outputFunc = const id

class CommandByName
  (name :: Symbol) commands shellIn shellOut |
    commands name -> shellIn shellOut
  where
  lookupProcessByName ::
    proxy name ->
    commands ->
    ShellCmd shellIn shellOut

instance 
  (matches ~ HeadMatches name names
  , CommandByName' matches name (CommandSet names types) shellIn shellOut
  ) => CommandByName name (CommandSet names types) shellIn shellOut
  where 
  lookupProcessByName _ = 
    lookupProcessByName' (Proxy @matches) (Proxy @name)

type family HeadMatches (name :: Symbol) (names :: [Symbol]) :: Bool where
  HeadMatches name (name : _) = True
  HeadMatches name _ = False

-- Allows to call any command in our command list, but requires the user to know
-- whether a particular command is at the front of the list of commands or not.
class CommandByName'
  (matches :: Bool) (name :: Symbol) commands shellIn shellOut |
  commands name -> shellIn shellOut
  where
  lookupProcessByName' :: 
    Proxy matches ->
    Proxy name ->
    commands ->
    ShellCmd shellIn shellOut

instance 
  CommandByName' True name (CommandSet (name:names) (ShellCmd a b : types)) a b
  where
  lookupProcessByName' _ _ (AddCommand cmd _) = cmd

instance
  (nextMatches ~ HeadMatches name names
  , CommandByName' 
    nextMatches name (CommandSet names types) shellIn shellOut
  ) => CommandByName' False name
        (CommandSet (badName : names) (t : types)) shellIn shellOut
  where
  lookupProcessByName' _ nameProxy (AddCommand _ rest) = 
    lookupProcessByName' (Proxy @nextMatches) nameProxy rest
