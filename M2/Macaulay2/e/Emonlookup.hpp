// Copyright 1999  Michael E. Stillman
#ifndef _Emonlookup_hh_
#define _Emonlookup_hh_

#include "queue.hpp"
#include "varpower.hpp"
#include "object.hpp"
#include "int_bag.hpp"

class EMInode // monomial ideal internal node ///
{
  friend void i_Ecommands();
  static stash *mystash;
public:
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  int                 var;
  int                 exp;		
  EMInode           * left;
  EMInode           * right;
  EMInode           * header;
  enum { node, leaf } tag;
  union {
    EMInode         * down;	// 'up' node, if this is a head of a list
    Bag             * bag;
  } val;

  EMInode(int v, int e, EMInode *d) 
    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(node)
  { val.down = d; }

  EMInode(int v, int e, Bag *b) 
    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(leaf)
  { val.bag = b; }

  EMInode *&down   () { return val.down; }
  Bag     *&baggage() { return val.bag; }
  intarray  &monom  () { return val.bag->monom(); } // varpower
  const intarray  &monom  () const { return val.bag->monom(); } // varpower

  void insert_to_left(EMInode *q)
    {
      q->header = header;
      q->left = left;
      q->right = this;
      left = left->right = q;
    }
};

class EMonomialLookupTable
{
  EMInode *mi;
  int count;
public:  
  EMonomialLookupTable() : mi(0), count(0) {}
  ~EMonomialLookupTable() { delete mi; }

  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  static EMInode *next(EMInode *p);
  static EMInode *prev(EMInode *p);
private:
  EMInode *first_node() const;
  EMInode *last_node() const;
  void remove_nodes(EMInode *&p);

  void insert1(EMInode *&p, Bag *b);
  void remove1(EMInode *p);
  void do_node(EMInode *p, int indent, int disp) const;
  void do_tree(EMInode *p, int depth, int indent, int disp) const;
  int debug_check(EMInode *p, EMInode *up) const;

  void k_basis1(int topvar) const;
public:
  int length() const { return count; }

  // Insertion of new monomials.  
  void insert_minimal(Bag *b);
        // Insert baggage 'b'.  It is assumed
        // that 'b' is not already in the monomial ideal.

  bool insert(Bag *b);

  void insert_w_deletions(Bag *b, queue<Bag *> &deletions);
        // Insert 'm', removing any monomials divisible by 'm', and
	// returning their baggage in a list of moninfo *'s.

  int remove(Bag *&b);
        // Deletion.  Remove the lexicographically largest element, placing
        // it into b.  Return 1 if an element is removed.

  bool search_expvector(const int *m, Bag *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is assumed to be an
	// exponent vector of length larger than the top variable occuring
	// in 'this'
  bool search(const int *m, Bag *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is a varpower monomial.
  void find_all_divisors(const int *exp, array<Bag *> &b) const;
  // Search. Return a list of all elements which divide 'exp'.

  int topvar() const { return (mi == NULL ? -1 : mi->var); }
  void text_out(buffer &o) const;
  void debug_out(int disp=1) const;
  void debug_check() const;

  class iterator {
    EMInode *p;
  public:
    iterator(EMInode *p) : p(p) {}
    iterator() : p(0) {}

    bool valid() { return p != 0; }
    void operator++() { p = EMonomialLookupTable::next(p); }
    void operator--() { p = EMonomialLookupTable::prev(p); }

    const Bag *& operator*() const { return p->baggage() ; }
    const Bag *& operator->() const { return p->baggage(); }
  };

  iterator first() { return iterator(next(mi)); }
  iterator last() { return iterator(prev(mi)); }
};


//-----------------------------------------------------------------

inline void EMonomialLookupTable::insert_minimal(Bag *b) 
{
  insert1(mi, b); 
  count++; 
}

#endif
