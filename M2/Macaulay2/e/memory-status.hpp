/**
 * @file memory-status.hpp
 * @brief Placeholder memory-statistics hooks: `memorystat1` / `memorystat2` / `memorystat3`.
 *
 * Declares three unimplemented `int`-returning functions whose
 * definitions in `memory-status.cpp` return distinct sentinel
 * constants (`123`, `1234`, `12345`). The names are referenced
 * from the interpreter side by `rawMemoryUsageStatus` in
 * `d/interface.dd`, which returns a hash with the placeholder
 * keys `"foo"` / `"bar"` / `"foobar"` --- a deliberate signal
 * that no real engine-wide memory accounting has been wired in
 * yet. The file exists so the link line keeps working while the
 * real implementation is deferred.
 *
 * @see mem.hpp
 */

int memorystat1(void);
int memorystat2(void);
int memorystat3(void);
