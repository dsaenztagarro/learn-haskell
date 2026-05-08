# `ch10/` — Stage 09: Parallel & Concurrent Programming

A separate cabal project covering Effective Haskell Chapter 10
(parallel and concurrent programming): directory traversal with
async/STM, metrics, and observability.

This is **Stage 09** in [`../docs/ROADMAP.md`](../docs/ROADMAP.md).

## Why is this not folded into the main project?

It was originally intended to live as `src/Apps/PCP/*` in the main
`learn-haskell` cabal package. That integration was deferred because
the root `effective-haskell.cabal` pins `base ^>=4.17.2.1` (GHC 9.4)
while `ThreadScope` and some of the chapter's experiments are easier
on a slightly different toolchain. Keeping `ch10/` as a sibling lets
each project pick its own constraints.

## Reading order within this folder

1. `src/Lib/TraverseDirectoryTree/Base.hs`        — sequential baseline.
2. `src/Lib/TraverseDirectoryTree/WithoutResultRef.hs` — first parallel rewrite.
3. `src/Lib/TraverseDirectoryTree/WithMetrics.hs` — adds counters via STM.
4. `src/Lib/Metrics.hs`, `src/Lib/Metrics/Main.hs` — the observability piece.
5. `src/Lib/TraverseDirectoryTree/Main.hs`        — entry-point that wires it together.
6. `app/Main.hs`                                  — `chapter10` executable.

## Build & run

```bash
cd ch10
cabal build
cabal run chapter10 -- /path/to/some/dir
```

Pair with [ThreadScope](https://github.com/haskell/ThreadScope) for
visualisation:

```bash
cabal run chapter10 -- +RTS -lf -RTS /some/dir
threadscope chapter10.eventlog
```

## Where this should eventually go

If you ever harmonise toolchains across both projects, fold this in
as `src/Apps/PCP/*` (Parallel & Concurrent Programming) and remove
this directory. The plan in
`/Users/dsaenz/.config/claude/plans/this-repository-contains-many-iterative-owl.md`
captures the full integration steps.
