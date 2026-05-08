# `Exts/Kinds/` — Type-level programming

The hardest stage. Two main ideas: type families (functions on types)
and using kinds (`DataKinds`) to lift terms to types.

| Module | Idea |
| --- | --- |
| `OpenTypeFamily` | Type family whose equations can be added in any module. |
| `ClosedTypeFamily` | Type family whose equations are fixed and ordered (Peano arithmetic example). |
| `TypeLevelListOperations/WithOpenTypeFamilies` | List operations expressed with open families. |
| `TypeLevelListOperations/WithClosedTypeFamilies` | Same operations re-expressed with closed families — direct contrast. |
| `AssociatedTypeFamily/ShellCmd` | Type family declared inside a class. |
| `AssociatedDataFamily/ShellCmd` | Same idea but generates a fresh datatype per instance — avoids accidental coupling. |
| `TypedProtocols` | Capstone: typed state machine (ping-pong) using `DataKinds` + GADTs. |

Suggested reading: top-to-bottom in the table. The four `ShellCmd`
variants under `Kinds/`, `FunctionalDependency/` and `GADT/` are
deliberate parallels — comparing them in one sitting is the fastest way
to internalise which extension does what.
