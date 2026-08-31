#include <benchmark/benchmark.h>

#include <cstdint>
#include <vector>

#include "monomials/ExponentVector.hpp"

static void BM_ExponentVectorMultiply(benchmark::State &state)
{
  const auto size = static_cast<int>(state.range(0));
  std::vector<int> left(size);
  std::vector<int> right(size);
  std::vector<int> result(size);

  for (int i = 0; i < size; ++i)
    {
      left[i] = i % 7;
      right[i] = i % 5;
    }

  for (auto _ : state)
    {
      benchmark::DoNotOptimize(left.data());
      benchmark::DoNotOptimize(right.data());
      exponents::mult(size, left.data(), right.data(), result.data());
      benchmark::ClobberMemory();
    }

  state.SetItemsProcessed(state.iterations() *
                          static_cast<std::int64_t>(size));
}

BENCHMARK(BM_ExponentVectorMultiply)->RangeMultiplier(4)->Range(4, 1024);
