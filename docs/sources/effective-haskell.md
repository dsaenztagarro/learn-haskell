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
|       — | _(rows are added as modules are tagged)_         | _(see commands above to regenerate)_                       |
|      10 | Parallel and concurrent programming              | `Apps.PCP.*` (formerly `ch10/`) — populated in Slice 7      |
|      12 | Type-level programming                           | `Exts.Kinds.*`, `Exts.FunctionalDependency.ShellCmd`       |

> Tip: when you are reading a chapter and want the matching code, also look at
> [`youtube.md`](youtube.md) — several chapters have companion lectures that
> are tagged separately.
