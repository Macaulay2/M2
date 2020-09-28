// Copyright 1995  Michael E. Stillman

#ifndef _FreeModule_hh_
#define _FreeModule_hh_

#include "ring.hpp"
#include "schorder.hpp"

class Matrix;
class GBMatrix;

// an include file under mingw32 defines a macro with the name FreeModule:
#undef FreeModule

class FreeModule : public EngineObject
{
  friend class Ring;
  friend class ResF4toM2Interface;
  FreeModule(const Ring *R, int n, bool has_schreyer_order);

 protected:
  VECTOR(int *) components;  // Degrees of each component
  SchreyerOrder *schreyer;  // NULL, if not a Schreyer order...

  const Ring *R;

 protected:
  virtual unsigned int computeHashValue() const;

  void initialize(const Ring *RR);

  virtual FreeModule *new_free() const;

 public:
  static FreeModule *make_schreyer(const Matrix *m);
  static FreeModule *make_schreyer(const GBMatrix *m);

  Matrix *get_induced_order() const;

  virtual ~FreeModule();

 public:
  void append(const int *d);
  void append_schreyer(const int *d,
                       const int *monom,
                       int compare_num);  // append to a Schreyer order.
  // WARNING: change_degree modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  void change_degree(int i, const int *deg);

 public:
  const Ring *get_ring() const { return R; }
  const Monoid *degree_monoid() const { return R->degree_monoid(); }
  const SchreyerOrder *get_schreyer_order() const { return schreyer; }
  const int *degree(int i) const { return components[i]; }
  int rank() const { return components.size(); }
  int primary_degree(int i) const;

  bool is_equal(const FreeModule *F) const;

  FreeModule *sub_space(int n) const;
  FreeModule *sub_space(M2_arrayint a) const;
  FreeModule *transpose() const;
  FreeModule *direct_sum(const FreeModule *G) const;
  FreeModule *shift(const int *d) const;
  FreeModule *tensor(const FreeModule *G) const;
  FreeModule *schur(const int *m) const;
  FreeModule *exterior(int p) const;
  FreeModule *symm(int p) const;

  M2_arrayintOrNull select_by_degrees(M2_arrayintOrNull lo,
                                      M2_arrayintOrNull hi) const;

  void direct_sum_to(const FreeModule *G);
  int lowest_primary_degree() const;
  int highest_primary_degree() const;

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
