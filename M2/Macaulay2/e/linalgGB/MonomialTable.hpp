// (c) 1994-2004  Michael E. Stillman

#ifndef _MonomialLookupTable_h_
#define _MonomialLookupTable_h_

#include <vector>
#include <gc.h>

#include "../queue.hpp"
#include "../index.hpp"

typedef int * monomial;
typedef int * exponent_vector;

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

  mi_node(int v, int e, mi_node *d) 
    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(node)
  { val.down = d; }

  mi_node(int v, int e, tagged_monomial *b) 
    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(leaf)
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
 public:
 private:

  mi_node *mi;
  int count;

  int size_of_exp; // in ints
  int *exp0; // Always set to be all zeros, except during searches, inserts?
private:
  void update_exponent_vector(monomial m);
  void reset_exponent_vector(monomial m);

  mi_node *first_node() const;
  mi_node *last_node() const;
  mi_node *next(mi_node *p) const;
  mi_node *prev(mi_node *p) const;

  void insert1(mi_node *&p, tagged_monomial *b);
  void remove1(mi_node *p);
  void do_node(mi_node *p, int indent, int disp) const;
  void do_tree(mi_node *p, int depth, int indent, int disp) const;
  int debug_check(const mi_node *p, const mi_node *up) const;

  void k_basis1(int topvar) const;

public:
  MonomialLookupTable() : mi(0), count(0) {}
  virtual ~MonomialLookupTable() { deleteitem(mi); }
  int length_of() const { return count; }

  MonomialLookupTable(queue<tagged_monomial *> &elems);
  MonomialLookupTable(queue<tagged_monomial *> &elems, queue<tagged_monomial *> &rejects);

  MonomialLookupTable * copy() const;

  monomial first_elem() const; // returns varpower
  monomial second_elem() const; // returns varpower

  // Informational
  int length() const { return count; }
  int topvar() const { return (mi == NULL ? -1 : mi->var); }
  void text_out(buffer &o) const;

  // Insertion of new monomials.  
  void insert_minimal(tagged_monomial *b);
        // Insert baggage 'b'.  It is assumed
        // that 'b' is not already in the monomial ideal.

  int insert(tagged_monomial *b);

  void insert_w_deletions(tagged_monomial *b, queue<tagged_monomial *> &deletions);
        // Insert 'm', removing any monomials divisible by 'm', and
	// returning their baggage in a list of moninfo *'s.

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

  void find_all_divisors(exponent_vector exp, VECTOR(tagged_monomial *) &b) const;
  // Search. Return a list of all elements which divide 'exp'.
  
  tagged_monomial *operator[](Index< MonomialLookupTable  > i) const;
  Index< MonomialLookupTable  > first() const;
  Index< MonomialLookupTable  > last () const;

  void *next (void *p) const;
  void *prev (void *p) const;
  int   valid(void *p) const { return (p != NULL); }

  void debug_out(int disp=1) const;
  void debug_check() const;
};

//-----------------------------------------------------------------

inline void MonomialLookupTable::insert_minimal(tagged_monomial *b) 
{
  insert1(mi, b); 
  count++; 
}

inline tagged_monomial * MonomialLookupTable::operator[](Index<MonomialLookupTable > i) const
{
  mi_node *p = reinterpret_cast<mi_node *>(i.val());
  return p->baggage();
}

inline mi_node *MonomialLookupTable::first_node() const
{
  return next(mi);
}

inline mi_node *MonomialLookupTable::last_node() const
{
  return prev(mi);
}

inline Index<MonomialLookupTable > MonomialLookupTable::first() const 
{ 
  return Index<MonomialLookupTable >(next(reinterpret_cast<void *>(mi)), this);
}

inline Index<MonomialLookupTable > MonomialLookupTable::last() const 
{ 
  return Index<MonomialLookupTable >(prev(reinterpret_cast<void *>(mi)), this);
}

#endif



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
