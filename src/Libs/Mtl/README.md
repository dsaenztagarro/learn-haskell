# `Libs/Mtl/` — Stage 07: Monad transformers from scratch

Hand-rolled equivalents of `transformers` and `mtl` so you can re-derive
how the magic of @get@/@put@/@throwError@ across a stack actually
works. Reading a from-scratch StateT/ExceptT once is worth ten
tutorials.

This is **Stage 07** in [`docs/ROADMAP.md`](../../../docs/ROADMAP.md).

## Reading order

1. `Control.Monad.State` — concrete `State s a = State { runState :: s -> (a, s) }`.
2. `Control.Monad.MonadState` — the mtl-style class with `m -> s` functional dependency.
3. `Control.Monad.MonadTrans` — `lift`; the contract every transformer obeys.
4. `Control.Monad.StateT` — `StateT s m a = s -> m (a, s)`.
5. `Control.Monad.ExceptT` — short-circuit-on-`Left` over an inner monad.
6. `Control.Monad.MonadError` — the mtl-style class for throw/catch.
7. `Control.Monad.MonadFail` — the class behind do-notation pattern-match failure.
8. `Control.Monad.MonadIO` — `liftIO` and how it composes through `lift`.

After this, the `Apps/Mtl/` modules in Stage 08 use the real `mtl` to
demonstrate the same patterns in a slightly bigger program.
