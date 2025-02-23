{-# LANGUAGE UndecidableInstances #-}
module Exts.UndecidableInstances where

{-
https://en.wikipedia.org/wiki/Undecidable_problem

An undecidable problem is a decision problem for which it is proved to be
impossible to construct an algorithm that always leads to a correct yes-or-no
answer.

The UndecidableInstances extension lifts restrictions that ensure the type
checker can actually complete type checking type class instances.

These checks are helpful more often than not, and prevent you from writing
code that might cause the compiler to hang during type checking.

You should only enable the extension when you actually need to relax these
checks.

In this file, without this extension, we would get the following error:
-}


-- TODO: add example
