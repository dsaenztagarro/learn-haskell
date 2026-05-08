# `Std/` — Stage 01: Re-build the Prelude

This folder is a **`NoImplicitPrelude` sub-library** (`no-prelude-lib` in the
cabal file). Every module hand-rolls a piece of the standard hierarchy
(`Functor`, `Applicative`, `Monad`, `Monoid`, `Maybe`, `Either`, …) so you
can re-derive the abstractions from first principles after time away.

This is **Stage 01** in [`docs/ROADMAP.md`](../../docs/ROADMAP.md).

## Reading order within this folder

1. `Data.Function`  — entry point; the function arrow as a 'Functor'.
2. `Data.Functor`   — the 'Functor' class and its laws.
3. `Data.Functor.Identity` — trivial baseline.
4. `Control.Applicative` — adds `pure` and `<*>`.
5. `Control.Monad`  — adds `>>=`, `>>`, and `join`.
6. `Control.Alternative` — choice and identity (`<|>`, `empty`).
7. `Data.Semigroup` then `Data.Monoid`.
8. Concrete instances: `Data.Maybe`, `Data.Either`, `Data.List`.

## Notes

- Modules under `Data/` define data types; modules under `Control/` define
  effectful classes. This mirrors the layout of `base`.
- `Std.Data.Functor.Identity` is the only module *not* under
  `NoImplicitPrelude` — it lives in the main `learn-haskell` library so it
  can be reused by `Libs/Mtl/`.
- Tests for this layer live under `test/Std/`. The
  Effective Haskell chaining example from `Maybe.hs` is exercised in
  `test/Std/Data/MaybeSpec.hs`.
