// (c) 1994-2021  Michael E. Stillman

#ifndef _f4monlookup_h_
#define _f4monlookup_h_

#include "f4/moninfo.hpp"            // for MonomialInfo (ptr only), const_p...
#include "f4/ntuple-monomial.hpp"    // for const_ntuple_monomial, ntuple_word
#include "f4/varpower-monomial.hpp"  // for const_varpower_monomial, varpowe...
#include "newdelete.hpp"             // for VECTOR, our_new_delete

class buffer;
class stash;

template <typename Key>
class F4MonomialLookupTableT : public our_new_delete
{
  struct mi_node : public our_new_delete  // monomial ideal internal node ///
  {
    varpower_word var;
    varpower_word exp;
    mi_node *left;
    mi_node *right;
    mi_node *header;
    enum { node, leaf } tag;
    union
    {
      mi_node *down;  // 'up' node, if this is a head of a list
      Key key;
    } val;

   public:
    mi_node *&down() { return val.down; }
    Key &key() { return val.key; }
    void insert_to_left(mi_node *q)
    {
      q->header = header;
      q->left = left;
      q->right = this;
      left = left->right = q;
    }
  };

  stash *mi_stash;
  VECTOR(mi_node *) mis;
  int count;

  int size_of_exp;  // in ints, size of exp0
  ntuple_word
      *exp0;  // Always set to be all zeros, except during searches, inserts?
 private:
  mi_node *new_mi_node(varpower_word v, varpower_word e, mi_node *d);
  mi_node *new_mi_node(varpower_word v, varpower_word e, Key k);
  void delete_mi_node(mi_node *p);

  void update_expvector(int topvar, const_varpower_monomial m);
  void reset_expvector(const_varpower_monomial m);

  bool find_one_divisor1(mi_node *mi,
                         const_ntuple_monomial exp,
                         Key &result_k) const;

  void find_all_divisors1(mi_node *mi,
                          const_ntuple_monomial exp,
                          VECTOR(Key) & result_k) const;

  void insert1(mi_node *&p, const_varpower_monomial m, Key k);

 public:
  F4MonomialLookupTableT(int nvars, stash *mi_stash = 0);
  ~F4MonomialLookupTableT();

  //  // Should we write these two routines?
  //  void insert_minimal_packed(const MonomialInfo *M,
  //                  const_packed_monomial m,
  //                  Key k);
  //        // It is assumed that 'm' is not already in the monomial ideal.
  //
  //  bool insert_packed(const MonomialInfo *M,
  //          const_packed_monomial m,
  //          Key &k);
  //        // If m is already divisible by an element, return false, and set k
  //        // to be the key of that element.
  //        // If m is not divisible, then insert (m,k), and return true.

  void insert_minimal_vp(long comp, const_varpower_monomial m, Key k);

  bool insert_vp(long comp, const_varpower_monomial m, Key &k);

  bool find_one_divisor_vp(long comp,
                           const_varpower_monomial m,
                           Key &result_k) const;

  bool find_one_divisor_packed(const MonomialInfo *M,
                               const_packed_monomial m,
                               Key &result_k) const;
  // Search.  Return whether a monomial which divides 'm' is
  // found.  If so, return true, set the key.

  void find_all_divisors_vp(long comp,
                            const_varpower_monomial m,
                            VECTOR(Key) & result_k) const;

  void find_all_divisors_packed(const MonomialInfo *M,
                                const_packed_monomial m,
                                VECTOR(Key) & result_k) const;
  // Search. Return a vector of all keys corresponding to
  // monomials which divide m.

  void text_out(buffer &o) const;

  int length() const { return count / 2; }
 private:
  mi_node *next(mi_node *p) const;
  mi_node *prev(mi_node *p) const;

  void do_node(mi_node *p, int indent, int disp) const;
  void do_tree(mi_node *p, int depth, int indent, int disp) const;
  int debug_check(mi_node *p, const mi_node *up) const;

  void debug_out(int disp = 1) const;
  void debug_check() const;
};

void minimalize_varpower_monomials(const VECTOR(varpower_monomial) & elems,
                                   VECTOR(int) & result_minimals,
                                   stash *mi_stash = 0);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
