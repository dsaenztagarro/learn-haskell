# `Exts/TypeSignatures/` — Type-signature ergonomics & semantics

Extensions and concepts that change how type variables behave in
signatures.

| Module                | Pragma / concept                                            |
| --------------------- | ----------------------------------------------------------- |
| `ExplicitForAll`      | `ExplicitForAll` — visible `forall` and the forall-or-nothing rule |
| `ScopedTypeVariables` | `ScopedTypeVariables` — bring a sig's tyvars into scope in the body |
| `AllowAmbiguousTypes` | `AllowAmbiguousTypes` — let unpinnable tyvars through, callers fix via `TypeApplications` |
| `InferredTypes`       | *concept*, not a pragma — specified vs. inferred tyvars (`{a}` brace syntax) |

Read in the order above. `InferredTypes` is filed here even though it
isn't a pragma because the brace syntax it explains is part of how
`ExplicitForAll`/`TypeApplications` interact.
