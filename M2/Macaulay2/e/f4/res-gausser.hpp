// Copyright 2014 Michael E. Stillman.

#ifndef _res_gausser_hpp_
#define _res_gausser_hpp_

#include "../ring.hpp"
#include "../aring-zzp-flint.hpp"
#include "../aring-glue.hpp"

class F4Mem;

class ResGausser
{
public:
  typedef M2::ARingZZpFlint RingType;
  typedef M2::ConcreteRing<RingType> InterfaceRingType;
  typedef RingType::ElementType ElementType;

  // This is the type of all indices into (and length of) dense and sparse vectors
  typedef int ComponentIndex;

  typedef ElementType* CoefficientArray; // A contiguous array of elements

  struct dense_row {
    ComponentIndex len; // coeffs is an array 0..len-1
    CoefficientArray coeffs;
  };

  struct Stats {
    long mNumCallsDenseRowCancel;
    long mNumCallsSubtractMultiple;
  };
private:
  const RingType& mRing;
  F4Mem *Mem;

  ResGausser(const RingType& K0, F4Mem* Mem0);
public:
  ~ResGausser() {}

  static ResGausser* create(const Ring* K, F4Mem* Mem0);

  const RingType& ring() const { return mRing; }

  CoefficientArray allocateCoefficientArray(ComponentIndex len) const;

  CoefficientArray from_ringelem_array(ComponentIndex len, ring_elem* elems) const;

  void to_ringelem_array(ComponentIndex len, CoefficientArray, ring_elem* result) const;

  CoefficientArray copy_CoefficientArray(ComponentIndex len, CoefficientArray F) const;

  void deallocate_CoefficientArray(CoefficientArray& F, ComponentIndex len) const;

  void dense_row_allocate(dense_row& r, ComponentIndex nelems) const;
  // create a row of 0's (over K).

  void dense_row_clear(dense_row& r, ComponentIndex first, ComponentIndex last) const;

  void dense_row_deallocate(dense_row& r) const;

  ComponentIndex dense_row_next_nonzero(dense_row& r, 
                                        ComponentIndex first, 
                                        ComponentIndex last) const;

  void dense_row_fill_from_sparse(dense_row& r,
                                  ComponentIndex len,
                                  CoefficientArray sparse,
                                  ComponentIndex* comps) const;
  // Fills 'r' from 'sparse' (and 'comps')

  void dense_row_cancel_sparse(dense_row& r,
                               ComponentIndex len,
                               CoefficientArray sparse,
                               ComponentIndex* comps) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // There should also be a version of this routine which records this value c
  // into a CoefficientArray.

  void dense_row_to_sparse_row(dense_row& r,
                               ComponentIndex& result_len,
                               CoefficientArray& result_sparse,
                               ComponentIndex*& result_comps,
                               ComponentIndex first,
                               ComponentIndex last) const;

  void sparse_row_make_monic(ComponentIndex len,
                             CoefficientArray sparse) const;

  const Stats& statistics() const { return mStats; }

private:
  mutable Stats mStats;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
