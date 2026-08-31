#include <benchmark/benchmark.h>
#include <M2/gc-include.h>
#include <engine.h>

int main(int argc, char **argv)
{
  IM2_initialize();
  ::benchmark::Initialize(&argc, argv);
  if (::benchmark::ReportUnrecognizedArguments(argc, argv)) return 1;
  ::benchmark::RunSpecifiedBenchmarks();
  ::benchmark::Shutdown();
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/benchmarks run"
// indent-tabs-mode: nil
// End:
