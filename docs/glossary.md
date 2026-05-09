# Glossary

One-line definitions for the vocabulary used throughout this repo. Each term
points to the module here that demonstrates it most concretely.

| Term                          | One-line definition                                                                            | Canonical module |
| ----------------------------- | ---------------------------------------------------------------------------------------------- | ---------------- |
| **Kind**                      | The "type of a type"; e.g. `Maybe :: * -> *`.                                                  | `Base.HigherKindedType` |
| **Role**                      | Per-type-parameter coercion permission: `nominal` / `representational` / `phantom`.            | `Base.Roles` |
| **Higher-kinded type**        | A type constructor that takes another type constructor (kind `(* -> *) -> *` or higher).       | `Base.HigherKindedType` |
| **Existential type**          | A type with a quantifier on the *right* of an arrow — "there exists some `a` such that...".    | `Exts.Types.ExistentialQuantification` |
| **Rank-N polymorphism**       | A `forall` nested inside an argument's type; the function receives a still-polymorphic value.  | `Exts.Types.RankNTypes` |
| **GADT**                      | A data type whose constructors can refine the result type — pattern-matching reveals types.    | `Exts.GADT.HeterogeneousList` |
| **Type family (open/closed)** | A function at the type level. Closed = fixed set of equations; open = instances can be added.  | `Exts.Kinds.ClosedTypeFamily`, `Exts.Kinds.OpenTypeFamily` |
| **Associated family**         | A type/data family declared inside a class, parameterised by the class's instance head.        | `Exts.Kinds.AssociatedTypeFamily.ShellCmd` |
| **Functional dependency**     | A class-level constraint that one parameter functionally determines another (`a -> b`).        | `Exts.FunctionalDependency.ShellCmd` |
| **`DerivingVia`**             | Derive an instance by routing through another type that already has it.                        | `Exts.Deriving.DerivingVia` |
| **`UndecidableInstances`**    | Switch off GHC's termination check for instance resolution.                                    | `Exts.UndecidableInstances` |
| **Monad transformer**         | A type constructor that adds an effect to an underlying monad, e.g. `StateT s m a`.            | `Libs.Mtl.Control.Monad.StateT` |
| **mtl-style class**           | A type-class capturing an effect (`MonadState`, `MonadError`) so callers stay polymorphic.     | `Libs.Mtl.Control.Monad.MonadState` |
| **Applicative parser**        | Parser combinators using `<*>` only — context-free; no choice based on prior parse.            | `Apps.FilePack.ApplicativeParser` |
| **Monadic parser**            | Parser combinators using `>>=` — later parses can depend on earlier results.                   | `Apps.FilePack.MonadicParser` |
