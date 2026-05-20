/**
 * @file unit-tests/M2-cpp-replacement.cpp
 * @brief Single-symbol stub for `system_interrupted()` so the gtest binary links without the M2 interpreter.
 *
 * Hosts one line: `bool system_interrupted() { return false; }`.
 * The engine calls this hook from inner loops to honour Ctrl+C
 * arriving through the interpreter's `interrupts.d`; the
 * `M2-unit-tests` binary does not link the interpreter, so the
 * symbol would otherwise be undefined. The stub always reports
 * "not interrupted," which is the correct answer for an
 * automated test run: tests should never time out from a fake
 * Ctrl+C.
 *
 * The fact that a single one-line replacement is enough to
 * build the engine for testing demonstrates how cleanly the
 * engine / interpreter boundary is drawn --- everything else
 * the test binary needs is satisfied by the regular engine
 * libraries. Companion to `testMain.cpp`, `fromStream.cpp`, and
 * `util-polyring-creation.{cpp,hpp}` in the `file-test-harness`
 * family.
 *
 * @see testMain.cpp
 * @see fromStream.cpp
 * @see util-polyring-creation.hpp
 * @see interrupted.hpp
 */

bool system_interrupted() { return false; }
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
