# `M2/Macaulay2/editors/vim/` — Vim integration

Templates and dictionaries that get expanded into the M2-specific Vim
configuration files installed alongside the binary. The expansion is driven by
[`../make-M2-symbols.m2`](../make-M2-symbols.m2), which pulls the live list of
Core symbols out of a running M2 and renders the editor templates.

| File | Role |
|---|---|
| `m2.vim.syntax.in` | Templated syntax-highlighting rules; symbol-table substitutions happen at build time |
| `m2.vim.syntax` | Generated syntax file (one expansion target) |
| `m2.vim.dict.in` | Templated word-list for autocompletion |
| `m2.vim.dict` | Generated word list |
| `m2.vim.plugin` | Static plugin loader sourced from `m2.vimrc` |
| `m2.vimrc` | Sample `vimrc` snippet for users wiring M2 mode into their config |
| `VimM2.scpt` | macOS AppleScript helper to launch a fresh Vim/M2 session |
| `README_linux` | Linux setup instructions |
| `README_macos` | macOS setup instructions |

## Regenerating

```sh
cmake --build M2/BUILD/build --target M2-emacs   # also regenerates Vim files
```

(The CMake target is shared across editor flavours.)

## Related

- [`../README.md`](../README.md) — editor overview.
- [`../make-M2-symbols.m2`](../make-M2-symbols.m2) — the script that produces
  the editor symbol lists.
