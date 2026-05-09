# Source-citation tag scheme

Every module in `src/` carries a `Source:` line in its Haddock header.
The line uses **short, greppable tags** so that one keystroke in
Telescope (or `rg`, `grep`, GitHub search) lands on every reference
across code, the roadmap, and the per-source index files in this
folder.

## Tag forms

| Form              | Meaning                                                | Example                                    |
| ----------------- | ------------------------------------------------------ | ------------------------------------------ |
| `EH:chN`          | *Effective Haskell*, chapter N                          | `EH:ch12 type-level`                       |
| `EH:pN`           | *Effective Haskell*, page N (for sub-chapter pointers)  | `EH:p457 alternative`                      |
| `EH:ch?`          | *Effective Haskell*, chapter unknown — fill in on next read | `EH:ch? functor` (placeholder)         |
| `RWH:chN`         | *Real World Haskell*, chapter N                         | `RWH:ch6 io`                               |
| `GHC:slug`        | GHC users guide, extension slug (kebab-case)            | `GHC:type-families`, `GHC:roles`           |
| `YT:slug`         | YouTube lecture (speaker-topic in kebab-case)           | `YT:rae-kinds-1`, `YT:prudnikov-quantification` |
| `Tweag:slug`      | Tweag (or other vendor) library/post                    | `Tweag:typed-protocols`                    |
| `base:Module.Name`| Direct citation of a stdlib module                      | `base:Control.Monad.IO.Class`              |

## Rules

- The tag (`EH:ch12`, `GHC:roles`, …) is the **load-bearing** part — the
  trailing words after a space are a human-readable hint, not a search
  key.
- Every tag is **regex-friendly**:
  - all `EH` chapter cites: `EH:ch\d+`
  - all `EH` page cites:    `EH:p\d+`
  - all unknown EH chapters: `EH:ch\?`
  - any source for a module: `^-- Source` line.
- A module may cite **multiple sources on one line**, comma-separated:
  ```
  -- Source : EH:ch12 type-level, EH:ch? funcdeps
  -- Source : GHC:existential-quantification, YT:prudnikov-quantification
  ```
- Continuation lines (URLs, PDF refs) are indented under `--` with no
  `Source` prefix; they are not greppable as tags but stay readable in
  hover/Haddock:
  ```
  -- Source : GHC:roles
  --          https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html
  --          docs/generative-type-abstraction-and-type-level-computation.pdf
  ```

## How to find code from a book or lecture

| You are looking at…                                      | Search                                  |
| -------------------------------------------------------- | --------------------------------------- |
| A specific *Effective Haskell* chapter                   | `EH:ch12` (or `EH:ch\d+` for any)       |
| Anything in *Effective Haskell* (chapter or page)        | `EH:ch\|EH:p`                           |
| All *Real World Haskell* references                      | `RWH:`                                  |
| A specific GHC extension                                 | `GHC:type-families`                     |
| A specific YouTube lecture                               | `YT:rae-kinds-1`                        |

In Telescope: `:Telescope live_grep<CR>` then type the tag.

In `rg`:

```bash
rg "EH:ch12"                 # one chapter
rg "^-- Source.*EH:"         # all Effective Haskell cites, one per module
rg -l "GHC:undecidable-instances"   # files mentioning this extension
```

## Filling in `EH:ch?` placeholders

When you next re-read *Effective Haskell* with a chapter table of
contents to hand, replace `EH:ch?` with the real number — for example:

```bash
# After confirming "Functor" lives in chapter 7:
rg -l "EH:ch\? functor\b" src/ | xargs sed -i '' 's/EH:ch? functor/EH:ch7 functor/g'
```

The `?` is intentional: `EH:ch?` was used wherever this repo was tagged
without the book in hand, so a single grep brings up every line that
still needs a chapter number.

## Index files in this folder

- [`effective-haskell.md`](effective-haskell.md) — chapter ↦ modules
- [`real-world-haskell.md`](real-world-haskell.md) — chapter ↦ modules
- [`youtube.md`](youtube.md) — lecture ↦ modules

These are the inverse of the `Source:` line in each module. To
regenerate the body from scratch:

```bash
grep -RHn "^-- Source" src/ --include='*.hs' | sort -t: -k3
```
