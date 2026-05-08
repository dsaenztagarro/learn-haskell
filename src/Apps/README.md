# `Apps/` — Stage 08: Applications

Small applications that glue the abstractions from Stages 01–07 into
real-ish programs. **Stage 08** in [`docs/ROADMAP.md`](../../docs/ROADMAP.md).

## Subfolders, in suggested reading order

1. **`FilePack/`** — three iterations of a binary serializer plus two
   parsers (applicative vs monadic). Read `Encoder1` → `Encoder2` →
   `Encoder3`, then `ApplicativeParser` → `MonadicParser`. Each fixes
   a flaw in the previous.
2. **`Mtl/`** — the same small program written twice: with a state
   `type` alias, then with a `newtype`. Compare error messages and
   inference behaviour.
3. **`Pager/`** — a `cat`/`less` clone refined chapter-by-chapter.
   Read `HCat1` → `HCat2` → `HCat3` → `HCat4`. Each is built as its
   own executable (`apps-hcat1` … `apps-hcat4` in the cabal file), so
   you can run all four side by side and observe the UX progression.

For Stage 09 (parallel & concurrent programming), see the sibling
[`ch10/`](../../ch10) project — it remains a separate cabal project
because of base-version differences. The roadmap links there.
