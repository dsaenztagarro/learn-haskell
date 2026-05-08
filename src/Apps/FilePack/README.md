# `Apps/FilePack/` — A binary file-archive format

Three iterations of an encoder plus two parsers, all operating on the
same `FilePack` data shape.

| Module | Idea |
| --- | --- |
| `Encoder1` | Naive: pack via `show`/`read` and base64. |
| `Encoder2` | Replace with hand-written tag + length-prefixed payload. |
| `Encoder3` | Existential `Packable` lets one pack mix heterogeneous payloads. |
| `Util` | Shared `Encode`/`Decode` classes and bit primitives. |
| `ApplicativeParser` | Combinators using only `<*>`/`<|>` — context-free. |
| `MonadicParser` | Same shape as `ApplicativeParser` but with `Monad` for context-sensitive parses. |

Read top-to-bottom; the encoders escalate, then the parsers contrast.
Tests in `test/Apps/FilePack/*` round-trip each encoder.
