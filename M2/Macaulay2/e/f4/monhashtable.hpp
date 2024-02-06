// Copyright 2005-2021  Michael E. Stillman

#ifndef _monhashtable_h_
#define _monhashtable_h_

#include <memory>                                      // for unique_ptr
#include "f4/moninfo.hpp"                              // for MonomialInfo
#include "schreyer-resolution/res-moninfo.hpp"         // for ResMonoid
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_packed_mon...

class MonomialsWithComponent
{
 public:
  typedef packed_monomial value;
  long hash_value(value m) const { return m[0] + m[1]; }
  bool is_equal(value m, value n) const { return mMonoid.is_equal(m, n); }
  void show(value m) const { mMonoid.show(m); }
  MonomialsWithComponent(const MonomialInfo& MI) : mMonoid(MI) {}
 private:
  const MonomialInfo& mMonoid;
};

class MonomialsIgnoringComponent
{
 public:
  typedef packed_monomial value;
  long hash_value(value m) const { return m[0]; }
  bool is_equal(value m, value n) const
  {
    return mMonoid.monomial_part_is_equal(m, n);
  }
  void show(value m) const { mMonoid.show(m); }
  MonomialsIgnoringComponent(const MonomialInfo& MI) : mMonoid(MI) {}
 private:
  const MonomialInfo& mMonoid;
};

class ResMonomialsWithComponent
{
 public:
  typedef res_packed_monomial value;
  int hash_value(value m) const
  {
    return mMonoid.hash_value(m) + 34141 * mMonoid.get_component(m);
  }  // m[0] + m[1]
  bool is_equal(value m, value n) const { return mMonoid.is_equal(m, n); }
  void show(value m) const { mMonoid.show(m); }
  ResMonomialsWithComponent(const ResMonoid& MI) : mMonoid(MI) {}
 private:
  const ResMonoid& mMonoid;
};

class ResMonomialsIgnoringComponent
{
 public:
  typedef res_packed_monomial value;
  int hash_value(value m) const { return m[0]; }
  bool is_equal(value m, value n) const
  {
    return mMonoid.monomial_part_is_equal(m, n);
  }
  void show(value m) const { mMonoid.show(m); }
  ResMonomialsIgnoringComponent(const ResMonoid& MI) : mMonoid(MI) {}
 private:
  const ResMonoid& mMonoid;
};

// ValueType must implement the following:
// values should have computed hash values stored with them
//  typename ValueType::value
//  long ValueType::hash_value(value m)
//  bool ValueType::is_equal(value m, value n)
//  void ValueType::show(value m) -- prints to stderr

template <typename ValueType>
class MonomialHashTable
{
  typedef typename ValueType::value value;

 private:
  const ValueType* M;
  std::unique_ptr<value[]> hashtab;

  unsigned long size;
  unsigned int logsize;
  unsigned long hashmask;
  unsigned long threshold;

  unsigned long count;

  unsigned long nfind_or_insert;
  unsigned long nclashes;
  unsigned long max_run_length;
  unsigned long monequal_count;
  unsigned long monequal_fails;

  void insert(value m);
  void grow();  // Increase logsize by 1, remake hashtab.
  void initialize(int logsize0);

 public:
  MonomialHashTable(const ValueType* M0, int logsize = 24);
  // The hash table size will be a power of 2, and this
  // is the initial power.

  ~MonomialHashTable() {}

  void reset();
  // Clear out the hash table, resetting all values to 0, and
  // all counting values back to 0 (count,nclashes,max_run_length)
  // BUT: the size is kept the same.

  bool find_or_insert(value m, value& result);
  // If the pointer m is in the hashtable already, then return true,
  // set result to the already existing pointer.  If m is not yet in
  // the hashtable, insert it, set result to be m, and return false;

  void dump() const;
  // displays on stderr some info about the hash table
  // and number of values

  void show() const;
  // displays the hash table
  // and then each value is displayed, with hash value.
  // in form [loc, hash, value]
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
