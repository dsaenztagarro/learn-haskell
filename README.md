This project contains examples from [Effective Haskell][1] book.

- Chapter13: Monad Transformers

[1]: https://www.pragprog.com/titles/rshaskell/effective-haskell/

### Learning path

- `InferredTypes`

#### Types and Kinds
- `GHCExt.ExistentialTypes.UsingRecord`
- `GHCExt.ExistentialTypes.UsingTypeClass`
- `GHCExt.ExistentialTypes.UsingTypeClassWithExistentialConstraint`
- `GHCExt.FunctionalDependencies` (ShellCommand)
- `GHCExt.Kinds.AssociatedTypeFamily` (ShellCommand)
- `GHCExt.Kinds.AssociatedDataFamily` (ShellCommand)
- `GHCExt.Kinds.OpenTypeFamily`
- `GHCExt.Kinds.ClosedTypeFamily`
- `GHCExt.Kinds.TypeLevelListOperations.WithClosedTypeFamilies`
- `GHCExt.Kinds.TypeLevelListOperations.WithOpenTypeFamilies`
- `GHCExt.GADT.HeterogeneusList` (ExistentialTypes related)
- `GHCExt.GADT.TypeClassRefactor` (ShellCommand)


#### YouTube Videos to add to GHCExt

Universal and Existential Quantification in Haskell – Stepan Prudnikov
https://www.youtube.com/watch?v=ohp2uRM9n0o

## Specs

```
cabal test --test-show-details=direct

cabal test --test-options="--color --match=FilePackParser"
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
