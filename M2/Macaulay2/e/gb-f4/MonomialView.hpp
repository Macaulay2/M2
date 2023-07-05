#pragma once

#include "../MemoryBlock.hpp"
#include <vector>

namespace newf4 {

class MonomialView
{
 private:
  int32_t* mData;  // We do not own the data pointed to
 public:
  explicit MonomialView(int32_t* data) : mData(data) {}
  explicit MonomialView(std::vector<int32_t>& data) : mData(data.data()) {}
  MonomialView(std::vector<int32_t> data, MemoryBlock& block)
  {
    MonomialView m(data.data());
    auto rng = block.allocateArray<int32_t>(m.size());
    mData = rng.first;
    std::copy(m.begin(), m.end(), mData);
  }
  MonomialView(const MonomialView& m, MemoryBlock& block)
  {
    auto rng = block.allocateArray<int32_t>(m.size());
    mData = rng.first;
    std::copy(m.begin(), m.end(), mData);
  }

  size_t size() const { return mData[0]; }
  bool operator==(const MonomialView& monom) const
  {
    if (size() != monom.size()) return false;
    for (auto i = 1; i < size(); ++i)
      if (mData[i] != monom.mData[i]) return false;
    return true;
  }

  auto begin() const -> decltype(mData) { return mData; }
  auto end() const -> decltype(mData) { return mData + size(); }
  auto begin() -> decltype(mData) { return mData; }
  auto end() -> decltype(mData) { return mData + size(); }
};

} // end namespace newf4