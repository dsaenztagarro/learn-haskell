# *Effective Haskell* → Modules in this repo

Reverse index: book chapter → modules. Inverse of the `Source:` line in
each module's Haddock header. Citation tag scheme is documented in
[`README.md`](README.md).

To regenerate the body from source:

```bash
grep -RHn "^-- Source.*EH:" src/ --include='*.hs' | sort -t: -k3
```

## By chapter

| Tag         | Topic / book section                       | Modules                                                                                                |
| ----------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------ |
| `EH:ch?`    | Functor                                    | `Std.Data.Functor`                                                                                     |
| `EH:ch?`    | Functor / Applicative                      | `Std.Data.Function`, `Std.Data.Functor.Identity`, `Std.Data.Either`                                    |
| `EH:ch?`    | Functor / Applicative / Monad              | `Std.Data.Maybe`, `Std.Data.List`                                                                      |
| `EH:ch?`    | Applicative                                | `Std.Control.Applicative`                                                                              |
| `EH:ch?`    | Monad                                      | `Std.Control.Monad`                                                                                    |
| `EH:p457`   | Alternative (page 457)                     | `Std.Control.Alternative`                                                                              |
| `EH:ch?`    | Semigroup / Monoid                         | `Std.Data.Semigroup`, `Std.Data.Monoid`                                                                |
| `EH:ch?`    | Kinds                                      | `Base.HigherKindedType`                                                                                |
| `EH:ch?`    | Deriving strategies                        | `Exts.Deriving.GeneralizedNewtypeDeriving`, `Exts.Deriving.DeriveAnyClass`, `Exts.Deriving.DerivingVia`, `Exts.Deriving.DerivingStrategies` |
| `EH:ch?`    | Existentials                               | `Exts.Types.ExistentialQuantification.WithTypeClass`, `Exts.Types.ExistentialQuantification.WithTypeClassConstraint` |
| `EH:ch?`    | GADTs                                      | `Exts.GADT.HeterogeneousList`, `Exts.GADT.ShellCmd`, `Exts.GADT.CommandRunner`                         |
| `EH:ch12`   | Type-level programming                     | `Exts.Kinds.ClosedTypeFamily`, `Exts.TypeSignatures.InferredTypes`, `Exts.GADT.CommandRunner`                         |
| `EH:ch12`   | Type families                              | `Exts.Kinds.OpenTypeFamily`, `Exts.Kinds.AssociatedTypeFamily.ShellCmd`, `Exts.Kinds.AssociatedDataFamily.ShellCmd`, `Exts.Kinds.TypeLevelListOperations.WithOpenTypeFamilies`, `Exts.Kinds.TypeLevelListOperations.WithClosedTypeFamilies`, `Exts.FunctionalDependency.ShellCmd` |
| `EH:ch?`    | Functional dependencies                    | `Exts.FunctionalDependency.ShellCmd` (also tagged `EH:ch12`)                                           |
| `EH:ch?`    | mtl / monad transformers                   | `Libs.Mtl.Control.Monad.State`, `StateT`, `MonadState`, `MonadTrans`, `ExceptT`, `MonadError`, `Apps.Mtl.StateMonadWithTypeAlias`, `Apps.Mtl.StateMonadWithNewtype` |
| `EH:p464`   | MonadFail (page 464)                       | `Libs.Mtl.Control.Monad.MonadFail`                                                                     |
| `EH:ch?`    | FilePack (Building applications)           | `Apps.FilePack.Encoder1`, `Encoder2`, `Encoder3`, `Apps.FilePack.Util`                                 |
| `EH:ch?`    | Parsers (Building applications)            | `Apps.FilePack.ApplicativeParser`, `Apps.FilePack.MonadicParser`                                       |
| `EH:ch?`    | HCat (Building applications)               | `Apps.Pager.HCat1`, `HCat2`, `HCat3`, `HCat4`                                                          |
| `EH:ch10`*  | Parallel and concurrent programming         | `Apps.PCP.*` (sibling project at `ch10/` until integrated)                                              |

\* `EH:ch10` is *not* yet placed inside `ch10/src/` because that project
is a separate cabal target. Tag the modules with `EH:ch10` as part of
the future fold-in (see `ch10/README.md`).

## Filling in `EH:ch?` placeholders

Run on next re-read of the book:

```bash
# Replace ? with the real chapter number, e.g. 7 for Functor:
rg -l "EH:ch\? functor\b" src/ docs/ | xargs sed -i '' 's/EH:ch? functor/EH:ch7 functor/g'
```

Then update the corresponding row in this table.
