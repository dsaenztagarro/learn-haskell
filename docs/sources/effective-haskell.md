# *Effective Haskell* → Modules in this repo

Reverse index: chapter from the book → modules in this repo. Inverse of the
`Source:` line in each module's Haddock header.

This file is **incrementally populated** as `src/` modules get tagged with a
`Source:` line during the per-folder sweeps (Slices 3–8 in
`/Users/dsaenz/.config/claude/plans/this-repository-contains-many-iterative-owl.md`).
A row is added the moment a module is tagged.

Quickly regenerate the body of this table from the source tree:

```bash
grep -RHn "^-- Source.*Effective Haskell" src/ \
  | sort -t: -k3
```

| Chapter | Topic                                            | Modules                                                    |
| ------: | ------------------------------------------------ | ---------------------------------------------------------- |
|     TBD | Functor                                          | `Std.Data.Functor`, `Std.Data.Functor.Identity`, `Std.Data.Function`, `Std.Data.Either`, `Std.Data.List`, `Std.Data.Maybe` |
|     TBD | Applicative                                      | `Std.Control.Applicative` (concrete instances in the modules above) |
|     TBD | Monad                                            | `Std.Control.Monad`, `Std.Data.Maybe`, `Std.Data.List`     |
|     TBD | Alternative (page 457)                           | `Std.Control.Alternative`                                  |
|     TBD | Semigroup / Monoid                               | `Std.Data.Semigroup`, `Std.Data.Monoid`                    |
|     TBD | Records & deriving                               | `Exts.Records.*`, `Exts.Deriving.*`                        |
|     TBD | Polymorphism (rank-N, scoped tyvars, ambiguity)  | `Exts.TypeSignatures.*`, `Exts.Types.RankNTypes`, `Exts.ArbitraryRankPolymorphism`, `Exts.InferredTypes` |
|     TBD | Existentials & GADTs                             | `Exts.Types.ExistentialQuantification.*`, `Exts.GADT.*`    |
|     TBD | Type families                                    | `Exts.Kinds.OpenTypeFamily`, `Exts.Kinds.ClosedTypeFamily`, `Exts.Kinds.AssociatedTypeFamily.ShellCmd`, `Exts.Kinds.AssociatedDataFamily.ShellCmd`, `Exts.Kinds.TypeLevelListOperations.*` |
|     TBD | Functional dependencies / UndecidableInstances   | `Exts.FunctionalDependency.ShellCmd`, `Exts.UndecidableInstances` |
|     TBD | Typed protocols (capstone)                       | `Exts.Kinds.TypedProtocols`, `Exts.GADT.CommandRunner`     |
|      10 | Parallel and concurrent programming              | `Apps.PCP.*` (formerly `ch10/`) — populated in Slice 7      |
|      12 | Type-level programming                           | `Exts.Kinds.*`, `Exts.FunctionalDependency.ShellCmd`       |

> Chapter numbers marked **TBD** are populated when the user revisits the
> book; the `Source:` lines in the modules currently say
> "Effective Haskell — &lt;topic&gt; chapter" with no numeric chapter.

> Tip: when you are reading a chapter and want the matching code, also look at
> [`youtube.md`](youtube.md) — several chapters have companion lectures that
> are tagged separately.
