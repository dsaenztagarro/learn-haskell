# `Exts/Types/` — Existentials & rank-N polymorphism

| Module | Extension(s) | Idea |
| --- | --- | --- |
| `RankNTypes` | `RankNTypes` | `forall` inside an argument arrow. |
| `ExistentialQuantification` | `ExistentialQuantification` | `forall` inside a constructor (the canonical existential). |
| `ExistentialQuantification/WithRecord` | + `RecordWildCards` | Pack the existential together with its operations. |
| `ExistentialQuantification/WithTypeClass` | (simulated) | Multi-param class as a *poor-man's* existential — illustrates why the real extension is preferable. |
| `ExistentialQuantification/WithTypeClassConstraint` | `ExistentialQuantification` + class constraint | The most idiomatic form: pack a value with a class dictionary. |

Suggested reading: `RankNTypes` → `ExistentialQuantification` → the three
`Existential*` variants in the order above.
