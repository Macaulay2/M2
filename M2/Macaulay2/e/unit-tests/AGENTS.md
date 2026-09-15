# Engine unit tests: instructions for agents

The conventions for writing tests in this directory are in **`STYLE.md`**.
Read it before adding or revising a test. What follows applies only to how you
work, not to how tests are written.

- Keep changes within the requested component. Treat production fixes and broad
  shared-helper refactors as separate work unless the task includes them.
- Build and run the affected tests after changing test code, in both build
  systems if you touched a source list. `STYLE.md` has the commands.
- Report which checks actually ran, and any limits on what you validated. Do not
  claim tests or coverage were rerun if they were not. For comment-only edits,
  checking the diff for unintended code changes is sufficient.
- Mention in the commit message your exact model (e.g. Claude Opus 5, GPT 5.6 Astra, Qwen3.8, etc.) -- do not comment in the test file regarding AI generation.
