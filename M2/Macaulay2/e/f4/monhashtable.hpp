// Copyright 2005-2021  Michael E. Stillman

#ifndef _monhashtable_h_
#define _monhashtable_h_

/**
 * @file f4/monhashtable.hpp
 * @brief `MonomialHashTable<ValueType>` --- open-addressing intern table for F4 and resolution monomials.
 *
 * Declares the template class `MonomialHashTable<ValueType>`,
 * a power-of-2 open-addressing hash table whose only public
 * operations are `find_or_insert`, `reset`, `dump`, and `show`
 * --- it interns `value` pointers (each `ValueType::value`
 * carries its own precomputed hash in the encoding) and grows
 * itself by doubling `logsize` and rehashing. Storage is a flat
 * `std::unique_ptr<value[]> hashtab` of `2^logsize` slots
 * (initial `logsize = 24`) plus a `hashmask` and a
 * `threshold`; the table also records `nclashes`,
 * `max_run_length`, `monequal_count`, `monequal_fails` for
 * dump/diagnostic output.
 *
 * `ValueType` must supply `typename value`, `hash_value(value)`,
 * `is_equal(value, value)`, and `show(value)`. Four such
 * traits ship in this header: `MonomialsWithComponent` (hashes
 * `m[0] + m[1]`, keeps free-module slots distinct so the table
 * can map Macaulay-matrix columns) and `MonomialsIgnoringComponent`
 * (hashes `m[0]`, strips the component for equality) for the
 * F4 `packed_monomial` encoding, and the
 * `ResMonomialsWithComponent` / `ResMonomialsIgnoringComponent`
 * `res_packed_monomial` analogues. The With-Component variant
 * on the resolution side folds in `34141 * get_component(m)`
 * so distinct free-module slots stay distinguishable. The
 * companion `monhashtable.cpp` also instantiates
 * `MonomialHashTable<MonomialInfo>` (with `MonomialInfo` as
 * its own trait), giving five live instantiations.
 *
 * @see moninfo.hpp
 * @see schreyer-resolution/res-moninfo.hpp
 */

#include <memory>                                      // for unique_ptr
#include "f4/moninfo.hpp"                              // for MonomialInfo
#include "schreyer-resolution/res-moninfo.hpp"         // for ResMonoid
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_packed_mon...

/**
 * @brief `MonHashTable` trait for `packed_monomial`s that include the
 * component coordinate in equality.
 *
 * @details `hash_value` mixes the leading two ints of the packed monomial
 * (the precomputed hash plus the component) so two monomials with
 * the same exponent vector but different components hash to
 * different buckets. `is_equal` defers to `MonomialInfo::is_equal`,
 * which compares both the monomial body and its component.
 */
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

/**
 * @brief `MonHashTable` trait for `packed_monomial`s that fold all components
 * together (used when only the underlying monomial matters).
 *
 * @details `hash_value` reads the leading int (the monomial-only hash)
 * without mixing in the component. `is_equal` delegates to
 * `MonomialInfo::monomial_part_is_equal`, which compares exponent
 * vectors and ignores the component slot.
 */
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

/**
 * @brief `MonHashTable` trait for the resolution engine's
 * `res_packed_monomial`s, with components included.
 *
 * @details Counterpart of `MonomialsWithComponent` for the F4 resolution
 * code: combines the per-monomial hash from `ResMonoid::hash_value`
 * with a `34141 * component` term so two monomials with the same
 * exponent vector but different components land in different
 * buckets. Equality goes through `ResMonoid::is_equal`.
 */
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

/**
 * @brief `MonHashTable` trait for `res_packed_monomial`s with the
 * component coordinate folded out.
 *
 * @details Resolution counterpart of `MonomialsIgnoringComponent`:
 * `hash_value` is just the leading int (the monomial-only hash) and
 * `is_equal` defers to `ResMonoid::monomial_part_is_equal`.
 */
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
