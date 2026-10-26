#include <benchmark/benchmark.h>

#include <cstddef>
#include <numeric>

#include "comb.hpp"

static void BM_SubsetEncode(benchmark::State &state)
{
  const auto elementCount = static_cast<std::size_t>(state.range(0));
  const auto subsetSize = static_cast<std::size_t>(state.range(1));
  Subsets subsets(elementCount, subsetSize);
  Subset subset(subsetSize);
  std::iota(subset.begin(), subset.end(), std::size_t {0});

  for (auto _ : state)
    benchmark::DoNotOptimize(subsets.encode(subset));

  state.SetItemsProcessed(state.iterations());
}

BENCHMARK(BM_SubsetEncode)->Args({12, 6})->Args({21, 7})->Args({30, 5});

static void BM_SubsetDecode(benchmark::State &state)
{
  const auto elementCount = static_cast<std::size_t>(state.range(0));
  const auto subsetSize = static_cast<std::size_t>(state.range(1));
  Subsets subsets(elementCount, subsetSize);
  Subset subset(subsetSize);
  std::iota(subset.begin(), subset.end(), elementCount - subsetSize);
  const auto encoded = subsets.encode(subset);

  for (auto _ : state)
    {
      subsets.decode(encoded, subset);
      benchmark::DoNotOptimize(subset.data());
      benchmark::ClobberMemory();
    }

  state.SetItemsProcessed(state.iterations());
}

BENCHMARK(BM_SubsetDecode)->Args({12, 6})->Args({21, 7})->Args({30, 5});
