# `Exts/GADT/` — Generalized Algebraic Data Types

| Module | Idea |
| --- | --- |
| `HeterogeneousList` | Mixed-type list whose element types are tracked at the type level. |
| `ShellCmd` | A typed DSL for shell pipelines (input/output types prevent invalid composition). |
| `CommandRunner` | A capstone: type-level dictionary mapping `Symbol` (command name) to `ShellCmd`, with `TypeError` on misspellings. |

Read in this order — each module reuses the type-level toolbox of the
previous one. `CommandRunner` straddles Stage 05 and Stage 06 since it
combines GADTs with closed type families and functional dependencies.
