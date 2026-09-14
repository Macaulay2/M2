# Benchmarking Macaulay2

Macaulay2 uses [Google Benchmark](https://github.com/google/benchmark) for
repeatable C++ microbenchmarks of engine code. The standalone `M2-benchmarks`
executable initializes the M2 engine before running the benchmarks; it is run
from the shell, not from the Macaulay2 interpreter.

Benchmarking is opt-in. It is not part of `make check` or `ctest`, because
performance measurements are sensitive to build flags, hardware, and machine
load and should not determine whether correctness tests pass.

## Install Google Benchmark

Install Google Benchmark separately before enabling benchmarks. Macaulay2 does
not download or build it. For Debian or Ubuntu:

```sh
sudo apt install libbenchmark-dev
```

On macOS with Homebrew:

```sh
brew install google-benchmark
```

If your system has no package, follow the upstream
[installation instructions](https://github.com/google/benchmark#installation)
and install it into a prefix of your choice. For CMake, pass
`-DCMAKE_PREFIX_PATH=/path/to/prefix` (or
`-Dbenchmark_DIR=/path/to/prefix/lib/cmake/benchmark`). For Autotools, pass
`CPPFLAGS="-I/path/to/prefix/include"` and
`LDFLAGS="-L/path/to/prefix/lib -Wl,-rpath,/path/to/prefix/lib"` when configuring;
use `lib64` instead of `lib` if appropriate. For Homebrew, the prefix is
`$(brew --prefix google-benchmark)`.

These instructions assume the usual Macaulay2 build dependencies and setup;
see [the build guide](../../../BUILD/README.md). Benchmarking is disabled by
default and does not require Google Benchmark unless explicitly enabled.

## Quick start with CMake

Use a release build for meaningful timings. From the repository root:

```sh
cmake -S M2 -B build -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_BENCHMARKS=ON
cmake --build build --target build-libraries
cmake --build build --target M2-benchmarks
./build/Macaulay2/e/M2-benchmarks
```

The `build-libraries` step installs missing M2 dependencies into
`build/usr-host` and reruns CMake. Google Benchmark must already be installed
when configuring with `BUILD_BENCHMARKS=ON`.

Useful runner options include:

```sh
# List benchmark names without running them.
./build/Macaulay2/e/M2-benchmarks --benchmark_list_tests

# Run only subset benchmarks.
./build/Macaulay2/e/M2-benchmarks '--benchmark_filter=BM_Subset.*'

# Repeat runs and report aggregate statistics.
./build/Macaulay2/e/M2-benchmarks \
  --benchmark_repetitions=10 \
  --benchmark_report_aggregates_only=true

# Save machine-readable results.
mkdir -p benchmark-results
./build/Macaulay2/e/M2-benchmarks \
  --benchmark_out=benchmark-results/results.json \
  --benchmark_out_format=json
```

See the upstream [Google Benchmark user
guide](https://google.github.io/benchmark/user_guide.html) for all runner
options, fixtures, counters, and timing controls.

## Quick start with Autotools

Add `--enable-benchmarks` when configuring an out-of-tree M2 build. For
example, from an empty build directory:

```sh
/path/to/repository/M2/configure --enable-download --enable-benchmarks --enable-optimize
make
make -C Macaulay2/e benchmarks
make -C Macaulay2/e run-benchmarks
```

Configuration checks for the installed `benchmark/benchmark.h` and
`-lbenchmark` and fails if they are unavailable. Pass runner flags through
`BENCHMARK_ARGS`:

```sh
make -C Macaulay2/e run-benchmarks \
  BENCHMARK_ARGS="--benchmark_filter=BM_ExponentVectorMultiply --benchmark_repetitions=5"
```

The executable is `Macaulay2/e/benchmarks/M2-benchmarks` within the build tree,
so it can also be run directly.

## Implementation

`benchmarkMain.cpp` deliberately replaces upstream's `BENCHMARK_MAIN()` macro.
It initializes M2 before handing command-line processing and execution to
Google Benchmark. This keeps garbage collection and engine globals consistent
with the GoogleTest executable.

As with `M2-unit-tests`, the benchmark executable also links
`unit-tests/M2-cpp-replacement.cpp`; that shim supplies the no-op
`system_interrupted()` implementation needed by standalone engine programs.

The initial suite contains examples for exponent-vector multiplication and
subset encoding/decoding. These exercise real engine code and demonstrate
argument sets, processed-item counters, and optimization barriers.

## Adding a benchmark

Create a `*Benchmark.cpp` file in `M2/Macaulay2/e/benchmarks`. A minimal
benchmark looks like this:

```cpp
#include <benchmark/benchmark.h>

static void BM_Operation(benchmark::State &state)
{
  // Construct inputs here; setup outside the loop is not timed.
  for (auto _ : state)
    {
      auto result = operationToMeasure();
      benchmark::DoNotOptimize(result);
    }
}

BENCHMARK(BM_Operation);
```

Register the new source in both build descriptions:

1. Add its basename to `BENCHMARK_CCFILES` in
   `M2/Macaulay2/e/benchmarks/Makefile.files`.
2. Add its filename to the `M2-benchmarks` source list in
   `M2/Macaulay2/e/CMakeLists.txt`.

Keep input construction outside the state loop unless allocation or setup is
the operation being measured. Use `benchmark::DoNotOptimize` for results and
`benchmark::ClobberMemory` when memory writes must remain observable. For
expensive per-iteration validation, pause timing or validate once after the
loop. Correctness belongs in the corresponding GoogleTest test first.

## Comparing results

The optional `compare.py` tool comes from a separate checkout of
[Google Benchmark](https://github.com/google/benchmark), and may require the
Python dependencies listed in its `tools/requirements.txt`.

Capture a baseline and a contender on the same machine with the same compiler,
build type, power settings, and background load:

```sh
mkdir -p benchmark-results
./build/Macaulay2/e/M2-benchmarks \
  --benchmark_out=benchmark-results/baseline.json
# Rebuild after the candidate change.
./build/Macaulay2/e/M2-benchmarks \
  --benchmark_out=benchmark-results/contender.json
python /path/to/google-benchmark/tools/compare.py benchmarks \
  benchmark-results/baseline.json benchmark-results/contender.json
```

Prefer several repetitions and look at aggregate statistics. A debug build is
useful for developing a benchmark, but its timings should not be used for
performance conclusions.
