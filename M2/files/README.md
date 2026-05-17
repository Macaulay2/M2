# `M2/files/` — runtime auxiliary files

A small directory of static files bundled with the distribution.

| File | Role | Deep dive |
|---|---|---|
| `M2-suppressions.supp` | Valgrind suppression list for M2-specific false positives | [`file-files-content.md`](file-files-content.md) |
| `info-dir-template` | Template for the `dir` file installed alongside texinfo `*.info` outputs | [`file-files-content.md`](file-files-content.md) |

**Coverage:** every file in this directory has a dedicated deep-dive doc.

If you add new auxiliary runtime files (config, data, suppressions, …) that
need to ship with M2 but don't belong to a specific source dir, this is the
home for them.

[← back to repository TOC](../../README.md#under-m2)
