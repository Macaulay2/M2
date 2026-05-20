// (c) 1994  Michael E. Stillman
#ifndef _int_bag_hh_
#define _int_bag_hh_

/**
 * @file int-bag.hpp
 * @brief `int_bag` / `Bag` --- minimal `(payload, varpower monomial)` carrier shared by monomial-ideal code.
 *
 * Declares `int_bag` (aliased as `Bag` at the bottom --- most
 * engine code refers to it under the alias), a small GC-managed
 * class pairing a `gc_vector<int>` varpower monomial with a
 * payload `union` of either an `int` (`b_elem`, typically a
 * basis index) or a `void*` (`b_ptr`, typically a `gbvector*`
 * or `Nterm*`). Callers know which arm of the union applies at
 * each call site; there is no runtime tag. Five constructors
 * cover the cases --- default, `int b`, `void* b`, `(int b,
 * varpower)`, and `(void* b, varpower)`, plus copy constructors
 * from reference and pointer. The accessors expose both arms
 * (`basis_elem()` / `basis_ptr()`) plus the monomial via the
 * paired `monom()` / `vector()` overloads (returning the same
 * `gc_vector<int>&` either way --- `monom()` carries the
 * varpower-encoded monomial, the `vector()` alias is the same
 * underlying buffer).
 *
 * The class survives from an older era of engine code that
 * wanted a single concrete type for ferrying `(payload,
 * monomial)` pairs --- primarily through `MonomialIdeal` build
 * helpers (`Nmi_node` leaves carry `Bag*` payloads) and
 * monomial-ideal minimal-prime recursion. Newer engine code
 * prefers `std::pair` over varpower or the strongly-typed
 * indices from `gb-f4/`; removing `int_bag` would touch many
 * call sites and is not yet on the critical path.
 *
 * @see ExponentList.hpp
 * @see monideal.hpp
 */

#include "newdelete.hpp"

class int_bag : public our_new_delete
{
  union
  {
    int b_elem;
    void* b_ptr;
  } val;
  gc_vector<int> mon;  // varpower representation

 public:
  int_bag() : mon() { memset(&val, 0, sizeof(val)); }
  int_bag(int b) : mon() { val.b_elem = b; }
  int_bag(void* b) : mon() { val.b_ptr = b; }
  int_bag(int b, const gc_vector<int>& m) : mon(m) { val.b_elem = b; }
  int_bag(void* b, const gc_vector<int>& m) : mon(m) { val.b_ptr = b; }
  int_bag(const int_bag& gcb) : val(gcb.val), mon(gcb.mon) {}
  int_bag(const int_bag* gcb) : val(gcb->val), mon(gcb->mon) {}

  gc_vector<int>& monom() { return mon; }
  const gc_vector<int>& monom() const { return mon; }

  gc_vector<int>& vector() { return mon; }
  const gc_vector<int>& vector() const { return mon; }

  int basis_elem() const { return val.b_elem; }
  void* basis_ptr() const { return val.b_ptr; }
};

typedef int_bag Bag;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
