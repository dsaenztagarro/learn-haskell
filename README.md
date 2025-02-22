This project contains examples from [Effective Haskell][1] book.

- Chapter13: Monad Transformers

[1]: https://www.pragprog.com/titles/rshaskell/effective-haskell/

### Learning path

- `InferredTypes`

#### Types and Kinds
- `LanguageExtensions.ExistentialTypes.UsingRecord`
- `LanguageExtensions.ExistentialTypes.UsingTypeClass`
- `LanguageExtensions.ExistentialTypes.UsingTypeClassWithExistentialConstraint`
- `LanguageExtensions.FunctionalDependencies` (ShellCommand)
- `LanguageExtensions.Kinds.AssociatedTypeFamily` (ShellCommand)
- `LanguageExtensions.Kinds.AssociatedDataFamily` (ShellCommand)
- `LanguageExtensions.Kinds.OpenTypeFamily`
- `LanguageExtensions.Kinds.ClosedTypeFamily`
- `LanguageExtensions.Kinds.TypeLevelListOperations.WithClosedTypeFamilies`
- `LanguageExtensions.Kinds.TypeLevelListOperations.WithOpenTypeFamilies`
- `LanguageExtensions.GADT.HeterogeneusList` (ExistentialTypes related)
- `LanguageExtensions.GADT.TypeClassRefactor` (ShellCommand)


#### YouTube Videos to add to LanguageExtensions

Universal and Existential Quantification in Haskell – Stepan Prudnikov
https://www.youtube.com/watch?v=ohp2uRM9n0o

## Specs

```bash
cabal install hspec-discover
cabal build
cabal test
cabal test --test-show-details=direct
cabal test --test-options="--color --match=FilePackParser"
cabal repl
```

### Debugging

#### Adding traces with TypeApplications

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Debug.Trace
import Data.Typeable

-- requires the constraint `Typeable a`
-- method :: forall a. (Typeable a, ...) =>
let mytrace = trace ("extractValue: " <> show (typeRep (Proxy @a)))
```

## Parallel and Concurrent Programming (PCP)

```
git clone git@github.com:haskell/ThreadScope.git
```
