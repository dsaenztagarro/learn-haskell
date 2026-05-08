# `Apps/Mtl/` — Same program, two state shapes

Both modules implement the same small text-formatting state machine.
The only difference is whether the state lives behind a `type` alias
or a `newtype`. Read both and compare:

- error messages when you make a typo,
- when GHC needs a type annotation vs when it can infer,
- whether you can derive `MonadState` automatically.

`StateMonadWithNewtype` is closer to how production code is written.

These modules sit atop the from-scratch `Libs/Mtl/` library (Stage
07).
