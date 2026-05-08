# *Real World Haskell* → Modules in this repo

Reverse index: chapter from the book → modules in this repo. Inverse of the
`Source:` line in each module's Haddock header.

This file is **incrementally populated** as `src/` modules get tagged during
the per-folder sweeps. A row is added the moment a module cites the book.

Quickly regenerate the body of this table from the source tree:

```bash
grep -RHn "^-- Source.*Real World Haskell" src/ \
  | sort -t: -k3
```

| Chapter | Topic                                       | Modules                              |
| ------: | ------------------------------------------- | ------------------------------------ |
|       — | _(rows are added as modules are tagged)_    | _(see commands above to regenerate)_ |
