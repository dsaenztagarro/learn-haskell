This project contains examples from [Effective Haskell][1] book.

Modules:
- `Apps`: Applications
- `Exts`: Language extensions
- `Std`: Standard Library
- `Libs`: Simple implementation of libraries

#### Types and Kinds
- `Exts.ExistentialTypes.UsingRecord`
- `Exts.ExistentialTypes.UsingTypeClass`
- `Exts.ExistentialTypes.UsingTypeClassWithExistentialConstraint`
- `Exts.FunctionalDependencies` (ShellCommand)
- `Exts.Kinds.AssociatedTypeFamily` (ShellCommand)
- `Exts.Kinds.AssociatedDataFamily` (ShellCommand)
- `Exts.Kinds.OpenTypeFamily`
- `Exts.Kinds.ClosedTypeFamily`
- `Exts.Kinds.TypeLevelListOperations.WithClosedTypeFamilies`
- `Exts.Kinds.TypeLevelListOperations.WithOpenTypeFamilies`
- `Exts.GADT.HeterogeneusList` (ExistentialTypes related)
- `Exts.GADT.TypeClassRefactor` (ShellCommand)


#### YouTube Videos to add to Exts

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

[1]: https://www.pragprog.com/titles/rshaskell/effective-haskell/
