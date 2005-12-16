// (c) 1994-2004  Michael E. Stillman

#ifndef _montable_h_
#define _montable_h_

#include <vector>

#include "../style.hpp"

typedef long *packed_monomial;
typedef int *exponent_vector;
typedef long *varpower_monomial;
typedef long *monomial;

class buffer;
class MonomialLookupTable;

struct tagged_monomial : public our_new_delete
{
  monomial monom; // pointer to some space
  void * bag; // perhaps a template parameter?

  tagged_monomial(monomial m, void *b) : monom(m), bag(b) {}
};


class mi_node : public our_new_delete // monomial ideal internal node ///
{
  friend class MonomialLookupTable;
protected:
  int                 var;
  int                 exp;		
  mi_node           * left;
  mi_node           * right;
  mi_node           * header;
  enum { node, leaf } tag;
  union {
    mi_node         * down;	// 'up' node, if this is a head of a list
    tagged_monomial  * bag;
  } val;
public:
  mi_node(int v, int e, mi_node *d) 
    : var(v), exp(e), left(0), right(0), header(0), tag(node)
  { val.down = d; }

  mi_node(int v, int e, tagged_monomial *b) 
    : var(v), exp(e), left(0), right(0), header(0), tag(leaf)
  { val.bag = b; }

  ~mi_node();

  mi_node *&down   () { return val.down; }
  tagged_monomial     *&baggage() { return val.bag; }
  monomial monom  () { return val.bag->monom; } // varpower

  void insert_to_left(mi_node *q)
    {
      q->header = header;
      q->left = left;
      q->right = this;
      left = left->right = q;
    }
};

class MonomialLookupTable : public our_new_delete
{
 private:
  mi_node *mi;
  int count;

  int size_of_exp; // in ints
  int *exp0; // Always set to be all zeros, except during searches, inserts?
private:
  void update_exponent_vector(monomial m);
  void reset_exponent_vector(monomial m);

  mi_node *next(mi_node *p) const;
  mi_node *prev(mi_node *p) const;

  mi_node *first_node() const { return next(mi); }
  mi_node *last_node() const { return prev(mi); }

  void insert1(mi_node *&p, tagged_monomial *b);
  void remove1(mi_node *p);
  void do_node(mi_node *p, int indent, int disp) const;
  void do_tree(mi_node *p, int depth, int indent, int disp) const;
  int debug_check(mi_node *p, mi_node *up) const;
public:
  typedef VECTOR(tagged_monomial *) vector_tagged_monomials;

  MonomialLookupTable() : mi(0), count(0) {}
  virtual ~MonomialLookupTable() { deleteitem(mi); }

  MonomialLookupTable(vector_tagged_monomials &elems);
  MonomialLookupTable(vector_tagged_monomials &elems, 
                      vector_tagged_monomials &rejects);

  MonomialLookupTable * copy() const;
  monomial first_elem() const; // returns varpower
  monomial second_elem() const; // returns varpower

  // Informational
  int length() const { return count; }
  int topvar() const { return (mi == 0 ? -1 : mi->var); }
  //  void text_out(buffer &o) const;

  // Insertion of new monomials.  
  void insert_minimal(tagged_monomial *b) { insert(mi,b); count++; }
        // Insert baggage 'b'.  It is assumed
        // that 'b' is not already in the monomial ideal.

  int insert(tagged_monomial *b);

  int remove(tagged_monomial *&b);
        // Deletion.  Remove the lexicographically largest element, placing
        // it into b.  Return 1 if an element is removed.

  int search_expvector(exponent_vector m, tagged_monomial *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is assumed to be an
	// exponent vector of length larger than the top variable occuring
	// in 'this'
  int search(monomial m, tagged_monomial *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is a varpower monomial.

  void find_all_divisors(exponent_vector exp, 
                         vector_tagged_monomials &b) const;
  // Search. Return a list of all elements which divide 'exp'.

  void debug_out(int disp=1) const;
  void debug_check() const;
  void text_out(buffer &o) const;

  class iterator;
  friend class iterator;

  class iterator {
    mi_node *p;
    const MonomialLookupTable *T;
  public:
    iterator(const MonomialLookupTable *T0) : p(T0->mi), T(T0) {}
    iterator(const MonomialLookupTable *T0, mi_node *p0) : p(p0), T(T0) {}
    void operator++() { T->next(p); }
    void operator--() { T->prev(p); }
    void operator++(int) { T->next(p); }
    void operator--(int) { T->prev(p); }
    bool valid() { return p != 0; }
    tagged_monomial * operator*() { return p->baggage(); }
  };

  iterator first() const { return iterator(this, next(mi)); }
  iterator last() const { return iterator(this, prev(mi)); }
  
};

#endif



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
