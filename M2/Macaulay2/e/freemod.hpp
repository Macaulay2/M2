// Copyright 1995  Michael E. Stillman

#ifndef _FreeModule_hh_
#define _FreeModule_hh_

/**
 * @file freemod.hpp
 * @brief `FreeModule` --- finite-rank free module `R^n`, the type-level anchor for every `Matrix`.
 *
 * Declares `FreeModule`, an `EngineObject` that holds its base
 * `Ring *R`, the per-generator degree vectors in
 * `gc_vector<monomial> components` (the rank is just
 * `components.size()`), and an optional `SchreyerOrder
 * *schreyer` (NULL when no Schreyer structure is installed).
 * Every `Matrix` stores pointers to a source and target
 * `FreeModule`, so this class is what gives matrix shapes
 * their static guarantees --- degree compatibility and
 * base-ring agreement are checked through these pointers. Once
 * a `FreeModule` is interned its `EngineObject` hash freezes
 * it, but during construction it is built up imperatively: the
 * fresh-return transformations `sub_space`, `direct_sum`,
 * `tensor`, `exterior` and the static `make_schreyer` allocate
 * a new `FreeModule`, while `append`, `append_schreyer`,
 * `change_degree`, and the in-place `direct_sum_to` mutate the
 * receiver and are used to fill it in before it escapes.
 *
 * When a Schreyer order is installed, the comparison
 * `compare(e_i * m, e_j * m')` first compares
 * `m_{i} * m` against `m_{j} * m'` using the stored leading
 * monomials of the inducing generators and then tiebreaks by
 * index, which is what lets a syzygy module remember the order
 * structure of the resolution step it came from. The
 * standalone `#undef FreeModule` near the top of the header
 * defuses a stray MinGW system-header macro of the same name.
 *
 * @see ring.hpp
 * @see schorder.hpp
 * @see matrix.hpp
 */

#include "ring.hpp"
#include "schorder.hpp"

class Matrix;
class GBMatrix;

// an include file under mingw32 defines a macro with the name FreeModule:
#undef FreeModule

/**
 * @brief Engine-side free module `R^n` over a `Ring`.
 *
 * @details Holds the underlying `Ring*` plus a `components` vector with the
 * multi-degree of each generator; optionally carries a
 * `SchreyerOrder*` that pins the per-component tie-breaker data
 * used when the free module participates in a Schreyer
 * resolution. Constructed only through `Ring`-side factory paths
 * (the constructor is private) so the engine can guarantee the
 * ring / degree pair is consistent before any vectors live in it.
 */
class FreeModule : public EngineObject
{
  friend class Ring;
  friend class ResF4toM2Interface;
  FreeModule(const Ring *R, int n, bool has_schreyer_order);

 protected:
  gc_vector<monomial> components;  // Degrees of each component
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
  void append(const_monomial d);
  void append_schreyer(const_monomial d,
                       const_monomial base_monom,
                       int compare_num);  // append to a Schreyer order.
  // WARNING: change_degree modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  void change_degree(int i, const_monomial deg);

 public:
  const Ring *get_ring() const { return R; }
  const SchreyerOrder *get_schreyer_order() const { return schreyer; }
  const_monomial degree(int i) const { return components[i]; }
  int rank() const { return components.size(); }
  int primary_degree(int i) const;

  bool is_equal(const FreeModule *F) const;

  FreeModule *sub_space(int n) const;
  FreeModule *sub_space(M2_arrayint a) const;
  FreeModule *transpose() const;
  FreeModule *direct_sum(const FreeModule *G) const;
  FreeModule *shift(const_monomial d) const;
  FreeModule *tensor(const FreeModule *G) const;
  FreeModule *schur(const_monomial m) const;
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
