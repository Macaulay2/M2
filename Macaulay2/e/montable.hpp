// Copyright 1999 by Michael E. Stillman
#ifndef _montable_hh_
#define _montable_hh_

#include <vector>
#include "varpower.hpp"

template <class Tag>
struct tagged_monomial
{
  intarray mon;
  Tag b;
  
  tagged_monomial(const intarray &mon, Tag b) : mon(mon), b(b) {}
  tagged_monomial(Tag b) : b(b) {}

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

template <class Tag>
class MonomialTable
{
  class iterator; 
  friend class iterator;
  typedef tagged_monomial<Tag> tagged_monomial;

protected:
  class node // monomial ideal internal node ///
  {
    friend void i_stashes();
    static stash *mystash;
    void *operator new(size_t) { return mystash->new_elem(); }
    void operator delete(void *p) { mystash->delete_elem(p); }
  public:
    int              var;
    int              exp;		
    node           * left;
    node           * right;
    node           * header;
    enum { NODE, LEAF } tag;
    union {
      node            * down;	// 'up' node, if this is a head of a list
      tagged_monomial * bag;
    } val;
    
    node(int v, int e, node *d) 
      : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(NODE)
      { val.down = d; }
    
    node(int v, int e, tagged_monomial *b) 
      : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(LEAF)
      { val.bag = b; }
    
    ~node();
    
    node *           &down   () { return val.down; }
    tagged_monomial *&baggage() { return val.bag; }
    intarray  &       monom  () { return val.bag->mon; } // varpower
    const intarray  & monom  () const { return val.bag->mon; } // varpower
    
    void insert_to_left(node *q)
      {
	q->header = header;
	q->left = left;
	q->right = this;
	left = left->right = q;
      }
  };
protected:
  node *mi;
  int count;

protected:
  static node *next(node *p);
  static node *prev(node *p);

  void remove_nodes(node *p);

  void insert1(node *&p, tagged_monomial *b);
  void remove1(node *p);

  void do_node(node *p, int indent, int disp) const;
  void do_tree(node *p, int depth, int indent, int disp) const;
  int debug_check(node *p, node *up) const;

public:  
  MonomialTable() : mi(0), count(0) {}
  ~MonomialTable();

  static MonomialTable<Tag> *create(vector<tagged_monomial *> &elems, 
				    vector<tagged_monomial *> &rejects);
  static MonomialTable<Tag> *create(vector<tagged_monomial *> &elems);

#if 0
  static void minimalize_monomials(vector<tagged_monomial *> &monoms, 
				   vector<tagged_monomial *> &rejects);
  // Removes elements which are strictly divisible by other elements.
  // The resulting minimal elements are placed into 'monoms', with the property that
  // all monomials in 'monoms' which are equal, and minimal, are contiguous in the result 'monoms'.
#endif

  int length() const { return count; }

  int topvar() const { return (mi == NULL ? -1 : mi->var); }

  void insert_minimal(tagged_monomial *b);
        // Insert baggage 'b'.  It is assumed
        // that 'b' is not already in the monomial ideal.

  bool insert(tagged_monomial *b);

  void insert_w_deletions(tagged_monomial *b, vector<tagged_monomial *> &deletions);
        // Insert 'm', removing any monomials divisible by 'm', and
	// returning their baggage in a list of moninfo *'s.

  bool remove(iterator &p, tagged_monomial *&b);
        // Remove the element currently pointed to by 'p'.  This iterator is no
        // longer valid after this operation.

  bool find_divisor(int nvars, const int *exp, tagged_monomial *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is assumed to be an
	// exponent vector of length larger than the top variable occuring
	// in 'this'

  bool find_divisor_of_varpower(const int *m, tagged_monomial *&b) const;
        // Search.  Return whether a monomial which divides 'm' is
	// found.  If so, return the baggage.  'm' is a varpower monomial.

  void find_all_divisors(int nvars, const int *exp, vector<const tagged_monomial *> &b) const;
        // Search. Return a list of all elements which divide 'exp'.

  void find_all_divisees(int nvars, const int *exp, vector<const tagged_monomial *> &b) const;
        // Search. Return a list of all elements which are divisible by 'exp'.

  void debug_out(int disp=1) const;

  void debug_check() const;

  class iterator {
    MonomialTable<Tag>::node *p;
  public:
    iterator(node *p) : p(p) {}
    iterator() : p(0) {}

    bool valid() { return p != 0; }
    
    void operator++() { p = MonomialTable<Tag>::next(p); }
    void operator--() { p = MonomialTable<Tag>::prev(p); }

    const tagged_monomial *& operator*() const { return p->val.bag ; }

    const intarray & get_monomial() const { return p->val.bag->mon; }

    const Tag & get_tag() const { return p->val.bag->b; }
    Tag & get_tag() { return p->val.bag->b; }  // User may change baggage if desired
  };

  iterator first() { return iterator(next(mi)); }
  iterator last() { return iterator(prev(mi)); }
};

template<class Tag>
MonomialTable<Tag>::node *MonomialTable<Tag>::next(node *p)
{
  while (p != NULL) 
    {
      p = p->left;
      if (p->tag == node::LEAF)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

template<class Tag>
MonomialTable<Tag>::node *MonomialTable<Tag>::prev(node *p)
{
  while (p != NULL) 
    {
      p = p->right;
      if (p->tag == node::LEAF)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

template<class Tag>
bool MonomialTable<Tag>::find_divisor(int nvars, const int *exp, tagged_monomial *&b) const
{
  if (mi == NULL) return false;

  node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
	{
	  if ((p = p->down()) == NULL) return false; 
	  continue;
	}
      
      if (p->exp > exp[p->var])
	{
	  if ((p = p->header->down()) == NULL) return false;
	  continue;
	}

      if (p->tag == node::LEAF) 
	{
	  b = p->baggage();
	  return true;
	} 

      p = p->down();	
    }
}

template <class Tag>
void MonomialTable<Tag>::find_all_divisors(int nvars, const int *exp, 
					   vector<const tagged_monomial *> &b) const
{
  if (mi == NULL) return;

  node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
	{
	  if ((p = p->down()) == NULL) return;
	  continue;
	}
      
      if (p->exp > exp[p->var])
	{
	  if ((p = p->header->down()) == NULL) return;
	  continue;
	}

      if (p->tag == node::LEAF) 
	{
	  b.push_back(p->baggage());
	} 
      else
	p = p->down();	
    }
}


template <class Tag>
bool MonomialTable<Tag>::find_divisor_of_varpower(const int *m, tagged_monomial *&b) const
{
  intarray exp;
  int nvars = 0;
  if (m[0] > 0) nvars = 1 + varpower::var(m[1]);
  varpower::to_ntuple(nvars, m, exp);
  return search_expvector(nvars, exp.raw(), b);
}

template <class Tag>
bool MonomialTable<Tag>::remove(iterator &p, tagged_monomial *&b)
{
  if (!p.valid()) return false;
  b = p->baggage();
  remove1(p);
  return true;
}

template <class Tag>
void MonomialTable<Tag>::insert_minimal(tagged_monomial *b) 
{
  insert1(mi, b); 
  count++; 
}

template <class Tag>
bool MonomialTable<Tag>::insert(tagged_monomial *b)
        // Insert the monomial 'b', if it
	// is not already in the monomial ideal.  Return whether the
	// monomial was actually inserted.  'b' is grabbed if true
        // is returned. Otherwise 'b' is not touched.
{
  tagged_monomial *old_b;
  const int *m = b->mon.raw();

  if (find_divisor_of_varpower(m, old_b)) 
    return false;
  insert_minimal(b);
  return true;
}

template <class Tag>
MonomialTable<Tag>::~MonomialTable()
{
  remove_nodes(mi);
  mi = 0;
  count = 0;
}

template <class Tag>
void MonomialTable<Tag>::remove_nodes(node *p)
{
  if (p->right != p->header) remove_nodes(p->right);
  if (p->tag == node::NODE)
    {
      if (p->header != p) remove_nodes(p->down());
    }
  else
    delete p->baggage();
}

template <class Tag>
MonomialTable<Tag> *MonomialTable<Tag>::create(vector<tagged_monomial *> &elems, 
					      vector<tagged_monomial *> &rejects)
  // Create a monomial table whose elements are the minimal elements of 'elems'.
  // Others are placed in 'rejects'.  'elems' is reset to size 0.
{
  MonomialTable<Tag> *result = new MonomialTable<Tag>;
  vector< vector<tagged_monomial *> * > bins;
  while (elems.size() > 0)
    {
      tagged_monomial *b;
      elems.pop_back(b);
      int d = varpower::simple_degree(b->mon.raw());
      if (d >= bins.size())
	for (int i=bins.size(); i<=d; i++)
	  bins.push_back(0);
      if (bins[d] == 0)
	bins[d] = new vector<tagged_monomial *>;
      bins[d]->push_back(b);
    }
  for (int i=0; i<bins.size(); i++)
    if (bins[i] != 0)
      {
	while (bins[i].size() > 0)
	  {
	    bins[i].pop_back(b);
	    if (!result->insert(b))
	      rejects.push_back(b);
	  }
	delete bins[i];
      }
  return result;
}

template <class Tag>
MonomialTable<Tag> *MonomialTable<Tag>::create(vector<tagged_monomial *> &elems)
  // Create a monomial table whose elements are in 'elems'.
  // Others are placed in 'rejects'.  'elems' is reset to size 0.
{
  MonomialTable<Tag> *result = new MonomialTable<Tag>;
  for (int i=0; i<elems.size(); i++)
    result->insert_minimimal(elems[i]);
  elems.clear();
}

// These final two routines do the complicated work for this data structure.
template <class Tag>
void MonomialTable<Tag>::insert1(node *&top, tagged_monomial *b)
{
  node **p = &top, *up = NULL;
  int one_element = 1;

  for (index_varpower i = b->monom().raw(); i.valid();)
    {
      one_element = 0;
      int insert_var = i.var();
      int insert_exp;
      
      if (*p == NULL)
	{
	  // make a new header node
	  *p = new node(insert_var, 0, up);
	  (*p)->header = (*p)->left = (*p)->right = *p;
	}
      else if ((*p)->var < insert_var)
	{
	  // make a new layer
	  node *header_node, *zero_node;
	  header_node = new node(insert_var, 0, up);
	  zero_node   = new node(insert_var, 0, *p);

	  header_node->left = header_node->right = zero_node;
	  (*p)->down() = zero_node;
	  *p = header_node->header = zero_node->header 
	    = zero_node->left = zero_node->right = header_node;
	}

      if ((*p)->var > insert_var)
	{ insert_var = (*p)->var; insert_exp = 0; }
      else
	{ insert_exp = i.exponent(); ++i; }

      node *q = (*p)->right;
      while((q != q->header) && (q->exp < insert_exp)) q = q->right;
      if (q->exp != insert_exp)
	{
	  node *insert_node;

	  if (i.valid())
	    {
	      insert_node
		= new node(insert_var, insert_exp,
					(node *)NULL);
	      q->insert_to_left(insert_node);
	      q = insert_node;
	    }
	  else
	    {
	      insert_node
		= new node(insert_var, insert_exp, b);
	      q->insert_to_left(insert_node);
	      return;
	    }
	}

      up = q;
      p = &(q->down());
    }
  if (one_element)
    {
      // insert a header node and a var/exp = 0/0 leaf
      top = new node(0, 0, (node *) NULL);
      node *leaf_node = new node(0, 0, b);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header 
	= leaf_node->left = leaf_node->right = top;
    }
}

template <class Tag>
void MonomialTable<Tag>::remove1(node *p)
{
  assert(p != NULL);
  assert(p->tag == node::LEAF);
  p->baggage() = NULL;
  count--;

  for(;p != NULL;)
    {
      p->left->right = p->right;
      p->right->left = p->left;
      node *q = p->header;
      p->left = p->right = NULL;
      delete p;

      if (q->right == q->header)  // only the header is left, so delete it
	{
	  p = q->down();
	  q->down() = NULL;
	  if (p != NULL) p->down() = NULL;
	  delete q;
	  continue;
	}

      if (q->left != q->right) return;

      if (q->left->exp > 0) return;

      node *dad = q->down();
      if (q->left->tag == node::LEAF)
	{
	  // set parent of q to be a leaf with baggage of q->left
	  // since this is a leaf, dad should be non null
	  assert(dad != NULL);
	  dad->tag = node::LEAF;
	  dad->baggage() = q->left->baggage();
	}
      else
	{
	  // set parent of q to be node pointing to q->left->down
	  q->left->down()->down() = dad;
	  if (dad != NULL)  
	    dad->down() = q->left->down();
	  else
	    mi = q->left->down();
	  q->left->down() = NULL;
	}
      q->down() = NULL;
      delete q;		// Deletes both nodes q, q->left.
      return;
    }
  if (p == NULL) mi = NULL;
}


#endif


