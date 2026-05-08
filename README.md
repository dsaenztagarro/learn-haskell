# learn-haskell

A personal study repository for refreshing Haskell quickly after time away.
Material is collected from *Effective Haskell*, *Real World Haskell*, and
selected YouTube lectures, and is organized **conceptually** so you can see
"everything I know about kinds" or "everything I know about deriving" in one
place.

## How to refresh

Open [`docs/ROADMAP.md`](docs/ROADMAP.md) and start at the lowest stage that
still feels rusty. The roadmap walks Stage 01 → 09, from rebuilding the
Prelude up to parallel & concurrent programming, with an **exit checkpoint**
at the end of each stage.

Every module in `src/` carries a Haddock header with:

- **Stage** — which roadmap stage it belongs to.
- **Source** — the book chapter / video it came from.
- **Prereqs** — modules to read first.
- **Concept · Example · Exercise** — a 30-second self-orientation.

Run `cabal haddock all` and browse the generated docs for a fast index.

## How to find code for a specific book chapter

When you are re-reading *Effective Haskell* or *Real World Haskell* and want
the matching code in this repo, use the reverse-index:

- [`docs/sources/effective-haskell.md`](docs/sources/effective-haskell.md) —
  chapter → modules table.
- [`docs/sources/real-world-haskell.md`](docs/sources/real-world-haskell.md)
- [`docs/sources/youtube.md`](docs/sources/youtube.md)
- [`docs/glossary.md`](docs/glossary.md) — one-line definitions, each
  pointing to its canonical module here.

These files are the inverse of the `Source:` line in each module. To rebuild
them by hand: `grep -RH "^-- Source" src/`.

## Layout

```
src/
  Std/        — Stage 01: Prelude rebuilt from scratch (NoImplicitPrelude lib)
  Base/       — Stage 02: kinds, roles, proxies
  Exts/       — Stages 03–06: language extensions, by topic
    Records/        Deriving/        TypeSignatures/
    TypeClasses/    Types/           GADT/
    Kinds/          FunctionalDependency/
  Libs/Mtl/   — Stage 07: monad transformer library, from scratch
  Apps/       — Stage 08: small applications
    FilePack/  Mtl/  Pager/  PCP/  (PCP folded in from former ch10/)
  YouTube/    — supplementary lecture notes (referenced by stages above)
test/         — hspec specs mirroring src/ (auto-discovered)
docs/         — ROADMAP, source indexes, glossary, deep-dive PDF
```

## Build & test

```bash
cabal build --with-compiler=ghc-9.6.7

cabal install ormolu          # formatter
cabal install hspec-discover

cabal test
cabal test --test-show-details=direct
cabal test --test-options="--color --match=FilePackParser"

cabal repl
cabal haddock all
```

### Debugging tip — typed traces

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Debug.Trace
import Data.Typeable

-- requires the constraint `Typeable a`
-- method :: forall a. (Typeable a, ...) =>
let mytrace = trace ("extractValue: " <> show (typeRep (Proxy @a)))
```

## Resources

- [*Effective Haskell* (Pragmatic Bookshelf)][1]
- [Cardano Engineering Handbook — Haskell testing practices][2]
- [ThreadScope][3] — visualisation for Stage 09.

[1]: https://www.pragprog.com/titles/rshaskell/effective-haskell/
[2]: https://input-output-hk.github.io/cardano-engineering-handbook/practices/haskell/testing.html
[3]: https://github.com/haskell/ThreadScope
