# YouTube lectures → Modules in this repo

Reverse index: lecture / video → modules in this repo. Inverse of the
`Source:` line in each module's Haddock header.

Quickly regenerate the body of this table from the source tree:

```bash
grep -RHn "^-- Source.*YouTube" src/
```

| Speaker / series | Title                                                 | URL | Modules                       |
| ---------------- | ----------------------------------------------------- | --- | ----------------------------- |
| Stepan Prudnikov | Universal and Existential Quantification in Haskell   | https://www.youtube.com/watch?v=ohp2uRM9n0o | `Exts.Types.ExistentialQuantification.*` (to be tagged) |
| Tweag / RAE      | Kinds (lecture series)                                | _link to be added when tagging `YouTube.Kinds`_ | `YouTube.Kinds`, `YouTube.Kinds2` |

Rows beyond the seed entries are added as modules get tagged during the
per-folder sweeps.
