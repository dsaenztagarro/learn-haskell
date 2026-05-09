# YouTube lectures → Modules in this repo

Reverse index: lecture → modules. Inverse of the `Source:` line in
each module's Haddock header. Citation tag scheme is documented in
[`README.md`](README.md).

To regenerate the body from source:

```bash
grep -RHn "^-- Source.*YT:" src/ --include='*.hs'
```

| Tag                          | Speaker / lecture                                                | URL                                          | Modules                                  |
| ---------------------------- | ---------------------------------------------------------------- | -------------------------------------------- | ---------------------------------------- |
| `YT:rae-kinds-1`             | Tweag (Richard Eisenberg) — *An introduction to Haskell's kinds* | https://www.youtube.com/watch?v=JleVecHAad4  | `YouTube.Kinds`                          |
| `YT:rae-kinds-2`             | Tweag (Richard Eisenberg) — *Getting a little fancy with Haskell's kinds* | https://www.youtube.com/watch?v=Qy_yxVkO8no | `YouTube.Kinds2`                         |
| `YT:prudnikov-quantification`| Stepan Prudnikov — *Universal and Existential Quantification*    | https://www.youtube.com/watch?v=ohp2uRM9n0o  | `Exts.Types.ExistentialQuantification`   |

Other vendor/library citations:

| Tag                     | Source                                                | Modules                       |
| ----------------------- | ----------------------------------------------------- | ----------------------------- |
| `Tweag:typed-protocols` | Tweag — *typed-protocols* library and lectures (Cardano network) | `Exts.Kinds.TypedProtocols` |
