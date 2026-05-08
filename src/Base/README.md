# `Base/` — Stage 02: Higher-kinded foundations

Two modules that recover the kind-level mental model needed before any of
the `Exts/` material lands:

1. `HigherKindedType` — kinds (`*`, `* -> *`), explicit kind signatures,
   and using a higher-kinded variable in a generic function (`toCSV`).
2. `Roles` — `nominal` / `representational` / `phantom`. Why GHC sometimes
   refuses to let you `coerce` even though the runtime representation matches.
   Pairs with `docs/generative-type-abstraction-and-type-level-computation.pdf`.

This is **Stage 02** in [`docs/ROADMAP.md`](../../docs/ROADMAP.md).

## Reading order within this folder

1. `HigherKindedType` first — kinds before roles.
2. `Roles` second — needs `GeneralizedNewtypeDeriving` and `DerivingVia`,
   which become familiar in Stage 03.

## Tests

`test/Base/HigherKindedTypeSpec.hs` exercises `toCSV`. `Base.Roles` is
checked by `test/Base/RolesSpec.hs` — the value of those tests is largely
that the module *compiles*: the role annotations and the
`deriving Num via Set Int` line are the lesson.
