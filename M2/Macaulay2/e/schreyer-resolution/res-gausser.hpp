// Copyright 2014 Michael E. Stillman.

#ifndef _res__gausser_hpp_
#define _res__gausser_hpp_

#include "newdelete.hpp"  // for our_new_delete
#include "ringelem.hpp"   // for ring_elem

#include <iosfwd>         // for ostream
#include <type_traits>    // for swap
#include <vector>         // for vector

class Ring;

typedef int ComponentIndex;

class CoefficientVector
{
  // disallow copy...
  friend class ResGausser;
  friend class ResGausserZZp;
  friend class ResGausserQQ;
  friend class ResGausserQQHybrid;

 public:
  CoefficientVector() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(CoefficientVector& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};

class ResGausser : public our_new_delete
{
 protected:
  static long numAdditions;

 public:
  ResGausser(const Ring* K) : mOriginalRing(K) {}
  virtual ~ResGausser() {}
  static ResGausser* newResGausser(const Ring* K1);

  const Ring* get_ring() const { return mOriginalRing; }
  long getNumAdditions() const { return numAdditions; }
 public:
  virtual void pushBackOne(CoefficientVector& coeffs) const = 0;
  virtual void pushBackMinusOne(CoefficientVector& coeffs) const = 0;
  virtual void pushBackElement(CoefficientVector& coeffs,
                               const CoefficientVector& take_from_here,
                               size_t loc) const = 0;
  virtual void pushBackNegatedElement(CoefficientVector& coeffs,
                                      const CoefficientVector& take_from_here,
                                      size_t loc) const = 0;

  virtual bool isAllowedCoefficientRing(const Ring* K) const = 0;
  virtual ring_elem to_ring_elem(
      const Ring* K,
      const CoefficientVector& coeffs,
      size_t loc) const = 0;  // in res-f4-m2-interface.cpp
  virtual void from_ring_elem(CoefficientVector& result,
                              ring_elem a,
                              ring_elem b) const = 0;  // appends to result.
  virtual long to_modp_long(CoefficientVector& coeffs, size_t loc) const = 0;

  virtual size_t size(CoefficientVector r) const = 0;

  virtual CoefficientVector allocateCoefficientVector(
      ComponentIndex nelems) const = 0;
  // create a row of 0's (over K).

  virtual CoefficientVector allocateCoefficientVector() const = 0;
  // create an empty array

  virtual void clear(CoefficientVector r,
                     ComponentIndex first,
                     ComponentIndex last) const = 0;
  // set the elements in the range first..last to 0.

  virtual void deallocate(CoefficientVector r) const = 0;

  virtual ComponentIndex nextNonzero(CoefficientVector r,
                                     ComponentIndex first,
                                     ComponentIndex last) const = 0;
  // returns last+1 in the case when there are no non-zero elements left.

  virtual void fillFromSparse(CoefficientVector r,
                              ComponentIndex len,
                              CoefficientVector sparse,
                              ComponentIndex* comps) const = 0;
  // Fills 'r' from 'sparse' (and 'comps')

  virtual void sparseCancel(CoefficientVector r,
                            CoefficientVector sparse,
                            ComponentIndex* comps,
                            CoefficientVector result_loc) const = 0;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is recorded in result_c.

  virtual void sparseCancel(CoefficientVector r,
                            CoefficientVector sparse,
                            ComponentIndex* comps) const = 0;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is not recorded in this version.

  virtual std::ostream& out(std::ostream& o,
                            CoefficientVector r,
                            int loc) const = 0;
  virtual std::ostream& debugDisplay(std::ostream& o,
                                     CoefficientVector r) const = 0;
  virtual std::ostream& debugDisplayRow(std::ostream& o,
                                        int ncolumns,
                                        const std::vector<int>& comps,
                                        CoefficientVector coeffs) const = 0;

 public:
  mutable long n_dense_row_cancel;
  mutable long n_subtract_multiple;

 private:
  const Ring* mOriginalRing;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
