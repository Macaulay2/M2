// (c) 1994-2004  Michael E. Stillman

#ifndef _montable_h_
#define _montable_h_

#include <vector>

#include "../style.hpp"
#include "varpower_monomial.hpp"
#include "ntuple_monomial.hpp"
#include "moninfo.hpp"

class buffer;
template <typename Key> class MonomialLookupTableT;

template <typename Key>
class mi_node : public our_new_delete // monomial ideal internal node ///
{
  friend class MonomialLookupTableT<Key>;
protected:
  int                 var;
  int                 exp;		
  mi_node           * left;
  mi_node           * right;
  mi_node           * header;
  enum { node, leaf } tag;
  union {
    mi_node         * down;	// 'up' node, if this is a head of a list
    Key             key;
  } val;
public:
  mi_node(int v, int e, mi_node *d) 
    : var(v), exp(e), left(0), right(0), header(0), tag(node)
  { val.down = d; }

  mi_node(int v, int e, Key k) 
    : var(v), exp(e), left(0), right(0), header(0), tag(leaf)
  { val.key = k; }

  ~mi_node();

  mi_node *&down   () { return val.down; }
  Key     &key() { return val.key; }

  void insert_to_left(mi_node *q)
    {
      q->header = header;
      q->left = left;
      q->right = this;
      left = left->right = q;
    }
};

template <typename Key>
class MonomialLookupTableT : public our_new_delete
{
 private:
  typedef mi_node<Key> mi_node;
  mi_node *mi;
  int count;

  int size_of_exp; // in ints
  ntuple_word *exp0; // Always set to be all zeros, except during searches, inserts?
private:
  void update_exponent_vector(const_varpower_monomial m);
  void reset_exponent_vector(const_varpower_monomial m);

  mi_node *next(mi_node *p) const;
  mi_node *prev(mi_node *p) const;

  void insert1(mi_node *&p, const_varpower_monomial m, Key k);
  void remove1(mi_node *p);
  void do_node(mi_node *p, int indent, int disp) const;
  void do_tree(mi_node *p, int depth, int indent, int disp) const;
  int debug_check(mi_node *p, mi_node *up) const;
public:
  MonomialLookupTableT();
  virtual ~MonomialLookupTableT() { deleteitem(mi); }

  // Informational
  int length() const { return count; }
  int topvar() const { return (mi == 0 ? -1 : mi->var); }
  void text_out(buffer &o) const;

  // Insertion of new monomials.  
  void insert_minimal(const_varpower_monomial m, Key k) { insert1(mi,m,k); }
        // Insert baggage 'b'.  It is assumed
        // that 'b' is not already in the monomial ideal.

  int insert(const_varpower_monomial m, Key k);

  int remove(Key &result_k);
        // Deletion.  Remove the lexicographically largest element, placing
        // it into b.  Return 1 if an element is removed.

  int search_expvector(const_ntuple_monomial m, Key &result_k) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is assumed to be an
	// exponent vector of length larger than the top variable occuring
	// in 'this'
  int search(const_varpower_monomial m, Key &result_k) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is a varpower monomial.

  void find_all_divisors(ntuple_monomial exp, 
                         VECTOR(Key) &b) const;
  // Search. Return a list of all elements which divide 'exp'.

  void debug_out(int disp=1) const;
  void debug_check() const;

  class iterator;
  friend class iterator;

  class iterator {
    mi_node *p;
    const MonomialLookupTableT *T;
  public:
    iterator(const MonomialLookupTableT *T0) : p(T0->mi), T(T0) {}
    iterator(const MonomialLookupTableT *T0, mi_node *p0) : p(p0), T(T0) {}
    void operator++() { T->next(p); }
    void operator--() { T->prev(p); }
    void operator++(int) { T->next(p); }
    void operator--(int) { T->prev(p); }
    bool valid() { return p != 0; }
    Key operator*() { return p->key(); }
  };

  iterator first() const { return iterator(this, next(mi)); }
  iterator last() const { return iterator(this, prev(mi)); }
  
};

#endif



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
