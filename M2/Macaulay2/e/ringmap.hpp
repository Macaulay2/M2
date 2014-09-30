// Copyright 1996  Michael E. Stillman
#ifndef _ringmap_hh_
#define _ringmap_hh_

#include "ring.hpp"

class RingElement;
class Matrix;

class RingMap : public EngineObject
{
  struct var : public our_new_delete {
    bool is_zero;               // Does this variable map to 0?

    bool coeff_is_one;
    bool monom_is_one;
    bool bigelem_is_one;

    ring_elem coeff;            // this variable maps to coeff*monom*bigelem
                                // where coeff is 1 if isone is true.
                                // and   monom is 1 if mon_isone is true,
                                // and   bigelem is 1 if bigone_isone is true.
                                // coeff is an element of type K.
    int *monom;                 // This is an exponent vector in R.
    ring_elem bigelem;
  };

  const Ring *R;                // This is the target ring.

  const PolynomialRing *P;      // P is either R (if it is a poly ring) or 0, ifnot
  const Ring *K;                // K is either coeffs of P, or R (if P==0).
  const Monoid *M;

  bool is_monomial;             // True, if each term maps to a term in the
                                // target ring.

  int nvars;                    // Number of variables in the source ring.
  var *_elem;                   // elem[i] is the structure representing the image of
                                // the i th variable.
  RingMap(const Matrix *m);
protected:
  virtual unsigned int computeHashValue() const;
public:
  ~RingMap();

  static const RingMap *make(const Matrix *m);

  const Ring *get_ring() const { return R; }

  bool is_equal(const RingMap *phi) const;

  const ring_elem elem(int i) const {
    ASSERT(i >= 0);
    ASSERT(i < nvars);
    return _elem[i].bigelem;
  }

  ring_elem eval_term(const Ring *coeff_ring, const ring_elem coeff,
                      const int *vp,
                      int first_var,
                      int nvars_in_source) const;

  RingElement /* or null */ *eval(const RingElement *r) const;
  Matrix /* or null */ *eval(const FreeModule *newrows, const Matrix *m) const;

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
