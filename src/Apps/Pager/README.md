# `Apps/Pager/` — `cat` to `less`, in four steps

Four executables (`apps-hcat1` … `apps-hcat4` in the cabal file). Each
adds one capability:

| Step | Adds |
| --- | --- |
| `HCat1` | Read a file from `argv` and print it. |
| `HCat2` | Pause every screenful, advance on ENTER. |
| `HCat3` | Detect terminal size at runtime so paging adapts. |
| `HCat4` | Raw terminal mode + single-key navigation (`q`, space, …). |

To exercise: `cabal run apps-hcatN -- some-file.txt` for N in 1..4.

These modules are not unit-tested — they need a TTY and real file I/O.
Run them by hand to verify behaviour.
