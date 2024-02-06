// Copyright 2018  Michael E. Stillman

// TODO Feb 2018:
//   rename IntsSet (to e.g. ModuleMonomSet, making it not a template
//   sort() function: should match Matrix sort.
//   try to have a function return a ModuleMonomSet.
//   Monom, ModuleMonom:
//    try to remove operator[], etc, so the type is pretty much opaque.
//    add in another type: VarPowerMonom (or call it SparseMonom, SparseModuleMonom)
//   use these other types from M2FreeAlgebra, commutative version.
//   improve the hash function.
//   remove dead code, e.g. starting at #if 0 below.
//   use this code for coefficients, monomials, even in the commutative variant.
//   get M2FreeAlgebra.m2 so 'make check' works in a reasonable amount of time.
//   add in leadCoeff, leadMonomial, leadTerm.  What other poly routines need to be added for M2FreeAlgebra?
//   consider looking at: https://github.com/skarupke/flat_hash_map
//     it is under boost license, it might be (not sure) well-written.

#ifndef _monomial_collection_hpp_
#define _monomial_collection_hpp_

#include "Polynomial.hpp"     // for ModuleMonom, monomToModuleMonom, Monom
#include "style.hpp"          // for EQ

#include <memtailor/Arena.h>  // for Arena
#include <algorithm>          // for sort
#include <iomanip>            // for operator<<, setw
#include <iostream>           // for operator<<, ostream, endl, cout, basic_...
#include <iterator>           // for bidirectional_iterator_tag, iterator_tr...
#include <typeinfo>           // for type_info
#include <unordered_set>      // for unordered_set
#include <utility>            // for pair
#include <vector>             // for vector

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

class ModuleMonomLessThan
{
public:
  bool operator()(const ModuleMonom& a, const ModuleMonom& b) const
  {
    auto cmp = ModuleMonom::compare(a,b);
    return cmp <= EQ;
  }
};
class ModuleMonomHash
{
public:
  std::size_t operator()(const ModuleMonom& m) const
  {
    return m.hash();
  }
};
class ModuleMonomEq
{
public:
  bool operator() (const ModuleMonom& a, const ModuleMonom& b) const
  {
    return a == b;
  }
};
class ModuleMonomDefaultConfigOrig
{
public:
  using ElementType = ModuleMonom;

  ModuleMonomDefaultConfigOrig(int nvars) : mNumVars(nvars) {}
  ModuleMonomDefaultConfigOrig(const ModuleMonomDefaultConfigOrig& C) : mNumVars(C.mNumVars) {}

  std::size_t hash(const ModuleMonom& m) const
  {
    return m.hash();
  }

  bool keysEqual(const ModuleMonom& e1, const ModuleMonom& e2) const
  {
    if (e1[0] != e2[0]) return false;
    for (int i=2; i < e1[0]; ++i)
      if (e1[i] != e2[i]) return false;
    return true;
  }

  std::size_t operator() (const ModuleMonom& e) const { return hash(e); }

  bool operator() (const ModuleMonom& e1, const ModuleMonom& e2) const { return keysEqual(e1,e2); }

  void display(std::ostream& o, const ModuleMonom& m) const
  {
    o << "val=" << m[1] << " [";
    for (int i=3; i<3+m[0]; ++i)
      o << m[i] << " ";
    o << "comp=" << m[2] << std::endl;
  }
private:
  int mNumVars;
};

class ModuleMonomDefaultConfig
{
public:
  ModuleMonomEq Eq;
  ModuleMonomHash Hash;

  ModuleMonomDefaultConfig(int nvars) : mNumVars(nvars) {}
  ModuleMonomDefaultConfig(const ModuleMonomDefaultConfig& C) : mNumVars(C.mNumVars) {}
#if 0  

  std::size_t hash(const ModuleMonom& m) const
  {
    return m.hash();
  }

  bool keysEqual(const ModuleMonom& e1, const ModuleMonom& e2) const
  {
    if (e1[0] != e2[0]) return false;
    for (int i=2; i < e1[0]; ++i)
      if (e1[i] != e2[i]) return false;
    return true;
  }

  std::size_t operator() (const ModuleMonom& e) const { return hash(e); }

  bool operator() (const ModuleMonom& e1, const ModuleMonom& e2) const { return keysEqual(e1,e2); }

  void display(std::ostream& o, const ModuleMonom& m) const
  {
    o << "val=" << m[1] << " [";
    for (int i=3; i<3+m[0]; ++i)
      o << m[i] << " ";
    o << "comp=" << m[2] << std::endl;
  }
#endif
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
  //using Set = std::unordered_set<ModuleMonom, Conf, Conf>;
  using Set = std::unordered_set<ModuleMonom, ModuleMonomHash, ModuleMonomEq>;

  IntsSet(Conf C) : mConf(C), mHash(100, C.Hash, C.Eq) {}

  Configuration configuration() const { return mConf; }
  const Set& set() const { return mHash; }
  const std::vector<ModuleMonom>& uniqueMonoms() const { return mElements; }
  std::size_t size() const { return mElements.size(); }
  
  // insert (m,comp) into the set. Returns true if it is inserted, i.e. it isn't already in the set.
  bool insert(Monom m, int comp)
  {
    size_t sz = ModuleMonom::sizeOfCorrespondingModuleMonom(m);
    std::pair<int*, int*> mon { mArena.allocArrayNoCon<int>(sz) };
    ModuleMonom a = monomToModuleMonom(m, comp, mon);
    auto result = mHash.insert(a);
    bool new_elem = result.second;
    if (new_elem)
      {
        a.setIndex(mElements.size());
        mElements.push_back(a);
      }
    else
      {
        mArena.freeTop(a + 0);
      }
    return new_elem;
  }

  // if (m,comp) is found (and is a monomial with index idx), return {idx, true}
  // if it is not found, return {-1, false}
  std::pair<int,bool> find(Monom m, int comp)
  {
    size_t sz = ModuleMonom::sizeOfCorrespondingModuleMonom(m);
    std::pair<int*, int*> mon { mArena.allocArrayNoCon<int>(sz) };
    ModuleMonom a = monomToModuleMonom(m, comp, mon);
    auto result = mHash.find(a); // returns iterator pointing to value, or mHash.end()
    bool found = (result != mHash.end());
    int idx = (found ? result->index() : -1);
    mArena.freeTop(mon.first);
    return {idx, found};
  }

  // Resorts the monomials, changing their indices
  void sort()
  {
    std::sort(mElements.begin(), mElements.end(), ModuleMonomLessThan());
    for (int i=0; i<mElements.size(); ++i)
      mElements[i].setIndex(i);
  }

  void display(std::ostream& o) const
  {
    // TODO: maybe we don't need this function.
    for (auto& m : mElements)
      o << "    " << m << std::endl;
  }

  void stats(std::ostream& o) const
  {
    // TODO:
    //  display some info about hash table size, collisions.
    //  display memory usage (TODO: maybe memory usage should be a separate function too).
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


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
