# `Exts/Deriving/` — Deriving strategies

The four strategies plus the standalone-deriving form. Reading order:

1. `GeneralizedNewtypeDeriving` — lift instances across a newtype boundary.
2. `StandaloneDeriving` — write the `deriving` clause separately, with explicit context.
3. `AnyclassDeriving` — derive an empty instance and use a class default.
4. `ViaCompatibleType` — `DerivingVia` through any representationally-compatible type.
5. `Strategies` — the umbrella; how to pick one explicitly with `deriving stock|newtype|anyclass|via`.

The order escalates in expressive power. `Base.Roles` (Stage 02) explains
when newtype-deriving is unsafe and how `representational`/`nominal`
roles guard it.
