// Copyright 2018  Michael E. Stillman

#ifndef _monomial_collection_hpp_
#define _monomial_collection_hpp_

#include "memtailor.h"
#include "PolynomialAlgebra.hpp"

#include <unordered_set>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <utility>

template<typename T1, typename T2>
std::ostream& operator << (std::ostream& o, const std::pair<T1,T2>& p)
{
  return o << "[" << p.first << "," << p.second << "]";
}

template<typename T>
inline void PRINT_ELEMENTS(const T& collection, const std::string& prefix)
{
  std::cout << prefix;
  for (const auto& elem : collection) {
    std::cout << elem << ' ';
  }
  std::cout << std::endl;
}

template<typename T>
void printHashTableState(const T& cont)
{
  // basic layout data
  std::cout << "size:            " << cont.size() << std::endl;
  std::cout << "buckets:         " << cont.bucket_count() << std::endl;
  std::cout << "load factor:     " << cont.load_factor() << std::endl;
  std::cout << "max load factor: " << cont.max_load_factor() << std::endl;

  // iterator category
  if (typeid(typename std::iterator_traits<typename T::iterator>::iterator_category)
      == typeid(std::bidirectional_iterator_tag))
    {
      std::cout << "chaining style:  doubly-linked" << std::endl;
    }
  else
    {
      std::cout << "chaining style:  singly-linked" << std::endl;
    }

  // elements per bucket
  std::cout << "data: " << std::endl;
  for (auto idx=0ul; idx != cont.bucket_count(); ++idx)
    {
      std::cout << " b[" << std::setw(2) << idx << "]: ";
      for (auto pos = cont.begin(idx); pos != cont.end(idx); ++pos)
        {
          std::cout << *pos << " ";
        }
      std::cout << std::endl;
    }
}

// Hash table for monomials (or ...)
// Functions:
//   insert(monomial,comp): returns index if (monomial,comp) is in table. Otherwise sets value to next size.
//   lookup(monomial,comp): returns index.
// A function elsewhere will loop through the monomials in a matrix, creating the hash table of these monomials.
//  actually: there might be translation from the monomials.
//  basically: (1) loop through monomials, add (transformed valued) one by one, when needed.
//  (2) loop through monomials, add monomial at row given by 'value' in hash table.
// Structure itself:
//  hash table (probably from stl)
//  memtailor memory area for monomials.
//  number of elements contained in here
// Memory management:
//  reserve space for a monomial (and/or copy it in).
//  jettison last one if not needed.

// stores elements of type T*: points to a contiguous list of integers, first one is the length.
// these are removed, when the hash table is removed.  Hash value is also stored.

class ModuleMonomDefaultConfig
{
public:
  using ElementType = ModuleMonom;

  ModuleMonomDefaultConfig(int nvars) : mNumVars(nvars) {}
  ModuleMonomDefaultConfig(const ModuleMonomDefaultConfig& C) : mNumVars(C.mNumVars) {}

  // ModuleMonom:
  // m[0]:
  // m[1]; hash value
  // m[2]: comnponent
  // m[3..] monomial itself.
  std::size_t computeHashValue(const ModuleMonom& m) const
  {
    std::cout << "compute hash" << std::endl;
    std::size_t result = 0;
    for (int i=2; i<3+m[3]; i++)
      result = 17*result + m[i];
    return result;
  }

  std::size_t hash(const ModuleMonom& m) const
  {
    if (m[1] == 0)
      m[1] = computeHashValue(m);
    return m[1];
  }

  bool keysEqual(const ModuleMonom& e1, const ModuleMonom& e2) const
  {
    std::cout << "equal" << std::endl;
    int top = 3 + e1[3];
    for (int i=1; i < top; ++i)
      if (e1[i] != e2[i]) return false;
    return true;
  }

  // result must be preallocated with m.size() + 3 ints.
  void copyToModuleMonom(const Monom& m, int comp, ModuleMonom result)
  {
    result[0] = 0;
    result[2] = comp;
    std::copy(m.begin(), m.end(), result+3);
    result[1] = computeHashValue(result); // ignores 0,1 locations.
  }

  int sizeOfCorrespondingModuleMonom(const Monom& m) const
  {
    return m.size() + 3;
  }

  void setIndex(ModuleMonom& m, int idx) const
  {
    m[0] = idx;
  }
  
  std::size_t operator() (const ModuleMonom& e) const { return hash(e); }

  bool operator() (const ModuleMonom& e1, const ModuleMonom& e2) const { return keysEqual(e1,e2); }

  void display(std::ostream& o, const ModuleMonom& m) const
  {
    o << "val=" << m[0] << " [";
    for (int i=3; i<3+m[3]; ++i)
      o << m[i] << " ";
    o << "comp=" << m[2] << std::endl;
  }
private:
  int mNumVars;
};


template<typename Configuration>
// Configuration must include:
//    types: Monom, ModuleMonom
//    hash function:  Configuration(ModuleMonom) --> std::size_t
//    equality check: Configuration(ModuleMonom, ModuleMonom) --> bool
//    copyToModuleElement: Configuration::copyToModuleElement(Monom, comp, val, ModuleMonom) --> void
//    value(ModuleMonom)
class IntsSet
{
public:
  using Conf = Configuration;
  using ModuleMonom = typename Conf::ModuleMonom;
  using Set = std::unordered_set<ModuleMonom, Conf, Conf>;

  IntsSet(Conf C) : mConf(C), mHash(100, C, C) {}

  Configuration configuration() const { return mConf; }
  const Set& set() const { return mHash; }
  const std::vector<ModuleMonom>& uniqueMonoms() const { return mElements; }
  
  // insert (m,comp) into the set. Returns true if it is inserted, i.e. it isn't already in the set.
  bool insert(Monom m, int comp)
  {
    size_t sz = mConf.sizeOfCorrespondingModuleMonom(m);
    std::pair<int*, int*> mon { mArena.allocArrayNoCon<int>(sz) };
    mConf.copyToModuleMonom(m, comp, mon.first);
    auto result = mHash.insert(mon.first);
    bool new_elem = result.second;
    if (new_elem)
      {
        mConf.setIndex(mon.first, mElements.size());
        mElements.push_back(mon.first);
      }
    else
      {
        mArena.freeTop(mon.first);
      }
    return new_elem;
  }

  std::pair<bool,int> find(Monom m, int comp)
  {
    // 1. allocate space for module monomial
    // 2. copy monomial,comp over to this
    // 3. call mHash routine
    // 4. if found, return (true,ptr found).
    //    if not: return (false,...)
    // in either case, before returning, pop arena stack.
  }

  // Resorts the monomials, changing their indices
  void sort();

  void display(std::ostream& o) const
  {
    for (auto i=begin(); i != end(); ++i)
      {
        o << "    ";
        mConf.display(o, *i);
      }
  }
  
  // hash table
  // monomial arena
  // list of pointers to these 'monomials' (so we can sort them?)
  //
  // data members:
  //  1. arena
  //  2. std::vector of int*'s into arena, unique monomials
  //  3. unordered_set
  // each monomial has a spot for its value.  When inserted into the hash table, this value is set.
  //
  // Conf:
  //   hash function
  //   equality function
  //   comparison function on monomials (needs to be a different class, I think)
  // Monomial ops:
  //   (monomial,comp) --> moduleMonomial (and back), put in space for int:
  //     --> [len, value, comp, v1, ..., vn] (len includes value)
  //     --> or [value, comp, len, deg, v1, ..., vr]  (hash, length, equality, compare) all work on these.
  //
  // ops:
  //   H[mon,comp]: first copy monomial to arena, insert it. If there, pop monom, otherwise set 'value' field
  //   lookup: first copy monomial,comp to arena, lookup, then pop copied monomial.
  //   sortMonomials: sorts mons, sets 'value' field in each monomial.
  //   value(H[mon]) = 0-th int.
private:
  Conf mConf;
  memt::Arena mArena; // memory area for monomials
  Set mHash; // will need to put in hash function, compare fcn from Conf.
  std::vector<ModuleMonom> mElements; // each monomial points into mArena.
public:
  auto begin() const -> decltype(mElements.begin()) { return mElements.begin(); }
  auto end() const -> decltype(mElements.end()) { return mElements.end(); }
};

using ModuleMonomialSet = IntsSet<ModuleMonomDefaultConfig>;

#if 0

template<typename Configuration>
class MonomialSet
{
public:
  using Conf = Configuration;
  using Monom = typename Conf::Monom;
  //using Key = typename Conf::Key;
  //using Value = typename Conf::Value;
  using Key = int*;
  using Value = int;
  using Map = std::unordered_map<Key,
                                 Value,
                                 Conf,
                                 Conf
                                 >;
  
  MonomialSet(Conf C) : mHash(10,C,C) {}
  //  MonomialSet(MonomialSet&& MS) : mArena(std::move(MS.mArena)), mHash(std::move(MS.mHash)) {}

  size_t size() const { return mHash.size(); }

  std::pair<int*, int*> allocateMonom(size_t max_size, int comp);

  void shrinkLastAllocation(int* new_end);

  // insert: returns the value associated with the given monomial, if it is
  // already in the structure.  Otherwise 'val' is associated with this monomial, and that is returned.
  int insert(Monom monomial, int comp)
  {
    std::pair<int*, int*> mon { mArena.allocArrayNoCon<int>(monomial.end()-monomial.begin()+1) };
    mon.first[0] = monomial[0] + 1;
    mon.first[1] = comp;
    std::copy(monomial.begin()+1, monomial.end(), mon.first+2);
    auto result = mHash.insert(std::make_pair<const Key&, Value>(mon.first, static_cast<Value>(mHash.size())));
    return result.second;
  }

  std::pair<bool,Value> find(Monom monomial, int comp);
  
private:
  memt::Arena mArena; // memory area for monomials
  std::unordered_map<int*,int,Conf,Conf> mHash; // will need to put in hash function, compare fcn from Conf.

  // use: mArena.allocArrayNoCon<int>(count) --> std::pair<int*, int*>  (a range of pointers).
};

class MonomialArea
{
public:
  MonomialArea();
  ~MonomialArea();

  int* reserve(size_t numInts);
  void popSomeInts(size_t numInts);
private:
  int mLastSize;
  int* mLastAlloc;
  memt::Arena mArena;
};

class MonomialAreaTest
{
public:
  MonomialAreaTest(){}

  size_t test1()
  {
    for (int i=1; i<20; i++)
      {
        std::pair<int*, int*> mon { mArena.allocArrayNoCon<int>(16) };
        std::cout << "(" << mon.first << "," << mon.second << ") sz = " << mArena.getMemoryUse() << ", " << mArena.getAllocatedMemoryUse() << std::endl;
        for (auto j = mon.first; j < mon.second; ++j)
          *j = 2*i;
        if (i % 5 == 0)
          {
            mArena.freeTop(mon.first + 4);
          }
      }
    return mArena.getMemoryUse();
  }
private:
  memt::Arena mArena;
};
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
