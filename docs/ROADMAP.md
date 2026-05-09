# Learning Roadmap

A linear path through this repository, designed for a **cold restart** after time
away from Haskell. Each stage builds on the previous one.

If you are looking for code that matches a specific book chapter, do **not** use
this file — go to [`sources/effective-haskell.md`](sources/effective-haskell.md)
or [`sources/real-world-haskell.md`](sources/real-world-haskell.md) instead.
That mapping is the inverse of the `Source:` line in every module's Haddock
header. The `Source:` lines use a short tag scheme (`EH:ch12`, `GHC:roles`,
`YT:rae-kinds-1`, …) — full reference at [`sources/README.md`](sources/README.md).

---

## How to use this roadmap

1. Pick the lowest stage that still feels rusty — don't start higher than you
   need.
2. For each stage, read the modules **in the order listed**.
3. Open `cabal repl` and try the `>>>` examples in each module's Haddock
   header.
4. At the end of each stage, attempt the **exit checkpoint** without looking at
   the modules. If you can't, repeat the stage.

Concept dependency map:

```
   Stage 01 Std (rebuild Prelude)
        │
        ▼
   Stage 02 Base (kinds, roles, proxies)
        │
        ├─────────────┬──────────────┐
        ▼             ▼              ▼
   Stage 03      Stage 04        Stage 07
   Records &     Type sigs &     Mtl from
   Deriving      polymorphism    scratch
                      │
                      ▼
                 Stage 05
                 Existentials & GADTs
                      │
                      ▼
                 Stage 06
                 Type-level programming
                      │
                      ▼
                 Stage 08 Apps  ──► Stage 09 Parallel & Concurrent
```

---

## Stage 01 — Re-build the Prelude (`src/Std/*`)

**Why:** rebuilding `Functor`/`Applicative`/`Monad`/`Monoid` from scratch is
the fastest way to recover muscle memory for Haskell's core abstractions and
their laws.

**You should remember after:**
- The exact signatures of `fmap`, `<*>`, `>>=`, `<>`, `mempty`, `mconcat`.
- Why `Applicative` sits between `Functor` and `Monad`.
- The difference between `Alternative` and `Monoid`.

**Modules in order:**
1. `Std.Data.Function` — `id`, `const`, `(.)`, `flip`.
2. `Std.Data.Functor` — `fmap` and the functor laws.
3. `Std.Data.Functor.Identity` — the trivial functor; useful baseline.
4. `Std.Control.Applicative` — `pure`, `<*>`.
5. `Std.Control.Monad` — `return`, `>>=`, `>>`.
6. `Std.Control.Alternative` — `<|>`, `empty`.
7. `Std.Data.Semigroup`, `Std.Data.Monoid` — `<>`, `mempty`.
8. `Std.Data.Maybe`, `Std.Data.Either`, `Std.Data.List` — concrete instances of
   the above on familiar containers.

**Exit checkpoint:** without imports, write a `Maybe` instance of `Functor`,
`Applicative`, and `Monad`, plus a `Semigroup` and `Monoid` instance for
`(Maybe a)` when `a` is a `Semigroup`. Explain why `Nothing` is the identity.

---

## Stage 02 — Higher-kinded foundations (`src/Base/*`)

**Why:** before anything in `Exts/`, you need to be comfortable thinking at the
*kind* level (`*`, `* -> *`, `Constraint`). Roles and proxies show up
constantly downstream.

**Modules in order:**
1. `Base.HigherKindedType` — kinds and `Proxy`.
2. `Base.Roles` — nominal vs representational vs phantom roles, why `coerce`
   is sometimes blocked. Pairs with the PDF in `docs/`.

**Exit checkpoint:** explain when GHC will *not* let you `coerce` between two
newtypes that wrap the same underlying type, and how a role annotation fixes
or causes that.

---

## Stage 03 — Records & deriving (`src/Exts/Records/*`, `src/Exts/Deriving/*`)

**Why:** ergonomic data modelling. Cheap wins; mostly mechanical, but easy to
forget the trade-offs between strategies.

**Modules in order:**
1. `Exts.Records.RecordWildCards`
2. `Exts.Records.NamedFieldPuns`
3. `Exts.Deriving.GeneralizedNewtypeDeriving`
4. `Exts.Deriving.StandaloneDeriving`
5. `Exts.Deriving.DeriveAnyClass`
6. `Exts.Deriving.DerivingVia` — `DerivingVia`.
7. `Exts.Deriving.DerivingStrategies` — the umbrella; how to disambiguate.

**Exit checkpoint:** given a newtype `Age = Age Int`, derive `Num`, `Show`,
`Eq`, and a JSON instance using three different strategies. Justify each
choice.

---

## Stage 04 — Type signatures & polymorphism (`src/Exts/TypeSignatures/*`, `Exts/Types/RankNTypes`)

**Why:** prepares you for existentials and rank-N — the moment polymorphism
stops behaving like generics in other languages.

**Modules in order:**
1. `Exts.TypeSignatures.ExplicitForAll`
2. `Exts.TypeSignatures.ScopedTypeVariables`
3. `Exts.TypeSignatures.AllowAmbiguousTypes`
4. `Exts.TypeSignatures.InferredTypes`
5. `Exts.Types.RankNTypes` — both the applied form (logger example) and rank-1/2/3 grammar reference
6. `Exts.TypeClasses.FlexibleContexts`, `Exts.TypeClasses.FlexibleInstances`

**Exit checkpoint:** explain why `f :: (forall a. a -> a) -> (Int, Bool)`
typechecks but `f' :: forall a. (a -> a) -> (Int, Bool)` does not. Use one of
the modules above to verify.

---

## Stage 05 — Existentials & GADTs (`src/Exts/Types/Existential*`, `src/Exts/GADT/*`)

**Why:** "hide a type variable inside a value" is the gateway to most
expressive Haskell APIs (heterogeneous lists, plugin systems, command
runners).

**Modules in order:**
1. `Exts.Types.ExistentialQuantification` — the bare extension.
2. `Exts.Types.ExistentialQuantification.WithRecord`
3. `Exts.Types.ExistentialQuantification.WithTypeClass`
4. `Exts.Types.ExistentialQuantification.WithTypeClassConstraint`
5. `Exts.GADT.HeterogeneousList`
6. `Exts.GADT.ShellCmd`
7. `Exts.GADT.CommandRunner`

**Exit checkpoint:** rewrite an existential-via-typeclass-constraint as a GADT
and explain which one constrains pattern-matching better.

---

## Stage 06 — Type-level programming (`src/Exts/Kinds/*`, `src/Exts/FunctionalDependency/*`, `src/Exts/UndecidableInstances`)

**Why:** the hardest stage. Computations on *types* — needed to read
libraries like `servant`, `vinyl`, `singletons`.

**Modules in order:**
1. `Exts.Kinds.OpenTypeFamily`
2. `Exts.Kinds.ClosedTypeFamily`
3. `Exts.Kinds.TypeLevelListOperations.WithOpenTypeFamilies`
4. `Exts.Kinds.TypeLevelListOperations.WithClosedTypeFamilies`
5. `Exts.Kinds.AssociatedTypeFamily.ShellCmd`
6. `Exts.Kinds.AssociatedDataFamily.ShellCmd`
7. `Exts.FunctionalDependency.ShellCmd` — alternative to associated families.
8. `Exts.UndecidableInstances` — when you can't avoid it and why it's safer
   than it sounds.
9. `Exts.Kinds.TypedProtocols` — capstone: typed state machines.
10. `YouTube.Kinds`, `YouTube.Kinds2` — Tweag/RAE lecture notes; useful as a
    second pass.

**Exit checkpoint:** write a closed type family `Length :: [k] -> Nat` and a
GADT `Vec n a` indexed by it. Show that `append :: Vec n a -> Vec m a -> Vec (n + m) a` typechecks.

---

## Stage 07 — Monad transformers from scratch (`src/Libs/Mtl/*`)

**Why:** demystifies `mtl`. Reading a from-scratch `StateT`/`ExceptT` once is
worth ten tutorials.

**Modules in order:**
1. `Libs.Mtl.Control.Monad.State` — `State` as a newtype.
2. `Libs.Mtl.Control.Monad.MonadState` — the class.
3. `Libs.Mtl.Control.Monad.MonadTrans` — `lift`.
4. `Libs.Mtl.Control.Monad.StateT` — the transformer.
5. `Libs.Mtl.Control.Monad.MonadError`, `Libs.Mtl.Control.Monad.ExceptT`.
6. `Libs.Mtl.Control.Monad.MonadFail`, `Libs.Mtl.Control.Monad.MonadIO`.

**Exit checkpoint:** write `StateT s (Either e) a` by hand and explain the
order of effect interleaving you chose.

---

## Stage 08 — Applications (`src/Apps/*`)

**Why:** glue the abstractions together on real-ish programs.

**Modules in order:**
1. `Apps.FilePack.Encoder1` → `Encoder2` → `Encoder3` — iterative refactor of
   a binary serializer. Read in order; each fixes a flaw in the previous.
2. `Apps.FilePack.ApplicativeParser` then `Apps.FilePack.MonadicParser` —
   contrast the two parser styles.
3. `Apps.Mtl.StateMonadWithTypeAlias` then `Apps.Mtl.StateMonadWithNewtype` —
   the same program twice; pay attention to inference and error messages.
4. `Apps.Pager.HCat1` → `HCat4` — a `cat`/`less` clone, refined chapter by
   chapter.

**Exit checkpoint:** add a fifth `HCat5` that paginates a piped stdin (no file
arg) without breaking `HCat4`'s behaviour.

---

## Stage 09 — Parallel & Concurrent (`src/Apps/PCP/*`, formerly `ch10/`)

**Why:** GHC's runtime is one of the strongest reasons to choose Haskell. This
stage covers `par`/`pseq`, `async`, `STM`, and observability.

**Modules in order:**
1. `Apps.PCP.TraverseDirectoryTree.Base`
2. `Apps.PCP.TraverseDirectoryTree.WithoutResultRef`
3. `Apps.PCP.TraverseDirectoryTree.WithMetrics`
4. `Apps.PCP.Metrics`

(Until the integration in Slice 7 lands these still live in `ch10/src/Lib/...`.)

**Exit checkpoint:** instrument a parallel directory traversal with a
`TVar`-based counter and verify there are no lost updates under load.

---

## Once you finish

You have completed a refresh. Pick a real project (or revisit a chapter that
still felt shaky) — the `Source:` lines in each module's Haddock will get you
back to the relevant book section in one jump.
