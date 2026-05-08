# `Exts/` — Stages 03–06: Language extensions

Concept-organized catalogue of GHC language extensions, demonstrated one
per module. Across **Stages 03–06** in
[`docs/ROADMAP.md`](../../docs/ROADMAP.md):

| Stage | Folder | Topic |
| ---: | --- | --- |
| 03 | `Records/`, `Deriving/` | Ergonomic data modelling. |
| 04 | `TypeSignatures/`, `TypeClasses/`, `Types/RankNTypes`, `InferredTypes`, `ArbitraryRankPolymorphism` | Polymorphism & type signatures. |
| 05 | `Types/Existential*`, `GADT/` | Existentials and GADTs. |
| 06 | `Kinds/`, `FunctionalDependency/`, `UndecidableInstances` | Type-level programming. |

## How to read this folder

Most modules are self-contained: a short Haddock header + a few
declarations + commented experiments. The reading order *across* this
folder is given in `docs/ROADMAP.md`. Reading order *within* a subfolder
is in that subfolder's `README.md` (where one exists) or in the order
listed in the table above.

## Notes

- `Exts/OverloadedStrings.hs` is **not** in the public cabal library
  (snippet only — read, don't import).
- `Exts/TypeSignatures/FlexibleContexts.hs` is an orphan file with a
  buggy module declaration (declares `Exts.TypeSignatures.ExplicitForAll`).
  The canonical FlexibleContexts demo is at
  `Exts/TypeClasses/FlexibleContexts.hs`.
- The shell-command example (`ShellCmd`) appears in four places to
  contrast extensions: under `FunctionalDependency`,
  `Kinds/AssociatedTypeFamily`, `Kinds/AssociatedDataFamily`, and
  `GADT/`. Reading them in that order is a fast tour of the design
  space.
