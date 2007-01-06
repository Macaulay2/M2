// (c) 1994-2004 Michael E. Stillman

#include "../text_io.hpp"

#include "MonomialTable.hpp"
#include "Monomials.hpp"
#include "monoms.h"

extern int gbTrace;

mi_node::~mi_node()
{
  if (right != header) deleteitem(right);
  if (tag == node)
    {
      if (header != this) deleteitem(down());
    }
  else
    deleteitem(baggage());
}

MonomialLookupTable::MonomialLookupTable(queue<tagged_monomial *> &elems, queue<tagged_monomial *> &rejects)
  : mi(0), count(0)
{
  size_of_exp = 10;
  exp0 = newarray_atomic_clear(int,size_of_exp);
  VECTOR( queue<tagged_monomial *> *) bins;
  tagged_monomial *b, *b1;
  while (elems.remove(b)) 
    {
      int d = monomial_simple_degree(b->monom);
      if (d >= bins.size())
	for (int i=bins.size(); i<=d; i++)
	  bins.push_back(NULL);
      if (bins[d] == NULL)
	bins[d] = new queue<tagged_monomial *>;
      bins[d]->insert(b);
    }
  for (int i=0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
	while (bins[i]->remove(b))
	  {
	    monomial mon = b->monom;
	    if (search(mon, b1))
	      rejects.insert(b);
	    else
	      insert_minimal(b);
	  }
	deleteitem(bins[i]);
      }
}

MonomialLookupTable::MonomialLookupTable(queue<tagged_monomial *> &elems)
  : mi(0), count(0)
{
  size_of_exp = 10;
  exp0 = newarray_atomic_clear(int,size_of_exp);
  VECTOR( queue<tagged_monomial *> *) bins;
  tagged_monomial *b;
  while (elems.remove(b)) 
    {
      int d = monomial_simple_degree(b->monom);
      if (d >= bins.size())
	for (int i=bins.size(); i<=d; i++)
	  bins.push_back(NULL);
      if (bins[d] == NULL)
	bins[d] = new queue<tagged_monomial *>;
      bins[d]->insert(b);
    }
  for (int i=0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
	while (bins[i]->remove(b)) insert(b);
	deleteitem(bins[i]);
      }
}

monomial MonomialLookupTable::first_elem() const
{
  return first_node()->monom();
}

monomial MonomialLookupTable::second_elem() const
{
  return next(first_node())->monom();
}

MonomialLookupTable * MonomialLookupTable::copy() const
{
  MonomialLookupTable *result = new MonomialLookupTable();
  for(Index<MonomialLookupTable> i = first(); i.valid(); i++)
    result->insert_minimal(new tagged_monomial(*( operator[](i)  )));
  return result;
}

int MonomialLookupTable::search_expvector(exponent_vector exp, tagged_monomial *&b) const
{
  if (mi == NULL) return 0;

  mi_node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
	{
	  if ((p = p->down()) == NULL) return 0; 
	  continue;
	}
      
      if (p->exp > exp[p->var])
	{
	  if ((p = p->header->down()) == NULL) return 0;
	  continue;
	}

      if (p->tag == mi_node::leaf) 
	{
	  b = p->baggage();
	  return 1;
	} 

      p = p->down();	
    }
}

void MonomialLookupTable::find_all_divisors(exponent_vector exp, VECTOR(tagged_monomial *) &b) const
{
  if (mi == NULL) return;

  mi_node *p = mi;

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

      if (p->tag == mi_node::leaf) 
	{
	  b.push_back(p->baggage());
	} 
      else
	p = p->down();	
    }
}

void MonomialLookupTable::update_exponent_vector(monomial m)
{
  int nvars = topvar() + 1;
  if (*m > 0 && m[1] >= nvars)
    nvars = m[1] + 1;
  if (size_of_exp <= nvars)
    {
      // Increase size of exponent vector
      deleteitem(exp0);
      if (nvars > 2*size_of_exp)
	size_of_exp = nvars;
      else
	size_of_exp *= 2;

      exp0 = newarray_atomic_clear(int,size_of_exp);
    }

  int nparts = *m++;
  for (int i=nparts; i>0; i--, m+=2)
    {
      exp0[*m] = m[1];
    }
}

void MonomialLookupTable::reset_exponent_vector(monomial m)
{
  int nparts = *m++;
  for (int i=nparts; i>0; i--, m+=2)
    {
      exp0[*m] = 0;
    }
}

int MonomialLookupTable::search(monomial m, tagged_monomial *&b) const
{
  MonomialLookupTable *me = const_cast<MonomialLookupTable *>(this);
  me->update_exponent_vector(m);
  int result = search_expvector(exp0, b);
  me->reset_exponent_vector(m);
  return result;
}

mi_node *MonomialLookupTable::next(mi_node *p) const
{
  while (p != NULL) 
    {
      p = p->left;
      if (p->tag == mi_node::leaf)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

void *MonomialLookupTable::next(void *p) const
{
  return reinterpret_cast<void *>(next(reinterpret_cast<mi_node *>(p)));
}

mi_node *MonomialLookupTable::prev(mi_node *p) const
{
  while (p != NULL) 
    {
      p = p->right;
      if (p->tag == mi_node::leaf)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

void *MonomialLookupTable::prev(void *p) const
{
  return reinterpret_cast<void *>(prev(reinterpret_cast<mi_node *>(p)));
}

void MonomialLookupTable::insert1(mi_node *&top, tagged_monomial *b)
{
  mi_node **p = &top, *up = NULL;
  int one_element = 1;

  for (index_monomial i = b->monom; i.valid();)
    {
      one_element = 0;
      int insert_var = i.var();
      int insert_exp;
      
      if (*p == NULL)
	{
	  // make a new header node
	  *p = new mi_node(insert_var, 0, up);
	  (*p)->header = (*p)->left = (*p)->right = *p;
	}
      else if ((*p)->var < insert_var)
	{
	  // make a new layer
	  mi_node *header_node, *zero_node;
	  header_node = new mi_node(insert_var, 0, up);
	  zero_node   = new mi_node(insert_var, 0, *p);

	  header_node->left = header_node->right = zero_node;
	  (*p)->down() = zero_node;
	  *p = header_node->header = zero_node->header 
	    = zero_node->left = zero_node->right = header_node;
	}

      if ((*p)->var > insert_var)
	{ insert_var = (*p)->var; insert_exp = 0; }
      else
	{ insert_exp = i.exponent(); ++i; }

      mi_node *q = (*p)->right;
      while((q != q->header) && (q->exp < insert_exp)) q = q->right;
      if (q->exp != insert_exp)
	{
	  mi_node *insert_node;

	  if (i.valid())
	    {
	      insert_node
		= new mi_node(insert_var, insert_exp, reinterpret_cast<mi_node *>(NULL));
	      q->insert_to_left(insert_node);
	      q = insert_node;
	    }
	  else
	    {
	      insert_node
		= new mi_node(insert_var, insert_exp, b);
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
      top = new mi_node(0, 0, reinterpret_cast<mi_node *>(NULL));
      mi_node *leaf_node = new mi_node(0, 0, b);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header 
	= leaf_node->left = leaf_node->right = top;
    }
}

void MonomialLookupTable::remove1(mi_node *p)
{
  assert(p != NULL);
  assert(p->tag == mi_node::leaf);
  p->baggage() = NULL;
  count--;

  for(;p != NULL;)
    {
      p->left->right = p->right;
      p->right->left = p->left;
      mi_node *q = p->header;
      p->left = p->right = NULL;
      deleteitem(p);

      if (q->right == q->header)  // only the header is left, so delete it
	{
	  p = q->down();
	  q->down() = NULL;
	  if (p != NULL) p->down() = NULL;
	  deleteitem(q);
	  continue;
	}

      if (q->left != q->right) return;

      if (q->left->exp > 0) return;

      mi_node *dad = q->down();
      if (q->left->tag == mi_node::leaf)
	{
	  // set parent of q to be a leaf with baggage of q->left
	  // since this is a leaf, dad should be non null
	  assert(dad != NULL);
	  dad->tag = mi_node::leaf;
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
      deleteitem(q);		// Deletes both nodes q, q->left.
      return;
    }
  if (p == NULL) mi = NULL;
}

int MonomialLookupTable::remove(tagged_monomial *&b)
{
  mi_node *p = reinterpret_cast<mi_node *>(next(mi));
  if (p == NULL) return 0;
  b = p->baggage();
  remove1(p);
  return 1;
}

static int nlists = 0;
static int nleaves = 0;
static int nnodes = 0;
static int ndepth = 0;

void MonomialLookupTable::do_node(mi_node *p, int indent, int disp) const
{
  buffer o;
  int i;
  assert(p->left != NULL);
  assert(p->right != NULL);
  assert(p->left->right == p);
  assert(p->right->left == p);
  if (disp)
    {
      for (i=1; i<=indent; i++) o << ' ';
      o << p->var << ' ' <<  p->exp;
    }
  if (p->tag == mi_node::leaf)
    {
      nleaves++;
      if (disp) o << ' ';
      monomial_text_out(o, p->baggage()->monom);
      o << '(';
      o << reinterpret_cast<long>(p->baggage()->bag);
      o << ')';
    }
  else if (p == p->header)
    nlists++;
  else
    nnodes++;
  emit_line(o.str());
}

void MonomialLookupTable::do_tree(mi_node *p, int depth, int indent, int disp) const
{
  if (depth > ndepth) ndepth = depth;
  do_node(p, indent, disp);
  mi_node *q = p->right;
  while (q != p) {
    do_node(q, indent, disp);
    if (q->tag != mi_node::leaf)
      do_tree(q->down(), depth+1, indent+2, disp);
    q = q->right;
  }
}

void MonomialLookupTable::debug_out(int disp) const
     // Display MonomialLookupTable in tree-like form, collect statistics
{
  nlists = 0;
  nnodes = 0;
  nleaves = 0;
  ndepth = 0;
  if (mi != NULL) do_tree(mi, 0, 0, disp);
  buffer o;
  o << "list nodes     = " << nlists << newline;
  o << "internal nodes = " << nnodes << newline;
  o << "monomials      = " << nleaves << newline;
  o << "max depth      = " << ndepth << newline;
  emit(o.str());
}

int MonomialLookupTable::debug_check(mi_node *p, mi_node *up) const
     // Returns the number of leaves at tree with root p.
     // Make sure that the list header is constructed ok, that the 
     // left/right pointers are ok on this level, that the
     // var, exp, values in this train are correct. 
     // Then loop through, checking each node (recursively) and each leaf
{
  mi_node *q;
  // First check the node 'p' itself
  assert(p != NULL);
  int v = p->var;
  assert(v >= 0);
  if (up != NULL) 
    assert(v < up->var);
  assert(p->header == p);
  assert(p->tag == mi_node::node);
  assert(p->down() == up);
  assert(p->left != NULL);
  assert(p->right != NULL);

  // Now loop through each element in left/right chain, checking that
  // v, e, left, right values are consistent.
  for (q = p->left; q != p; q = q->left)
    {
      assert(q->left != NULL);
      assert(q->right != NULL);
      assert(q->header == p);
      assert(q->right->left == q);
      assert(q->left->right == q);
      assert(q->var == v);
      assert((q->right == p) || (q->exp < q->right->exp));
      assert(q->exp >= 0);
    }

  // Now loop through again, this time descending into nodes
  int c = 0;
  for (q = p->right; q != p; q = q->right)
    if (q->tag == mi_node::node) 
      c += debug_check(q->down(), q);
    else
      c++;
  return c;
}

void MonomialLookupTable::debug_check() const
{
  if (count == 0) 
    {
      assert(mi == NULL);
      return;
    }
  assert(mi != NULL);
  assert(debug_check(mi, NULL) == count);
}

int MonomialLookupTable::insert(tagged_monomial *b)
        // Insert the monomial (and baggage) 'm', if it
	// is not already in the monomial ideal.  Return whether the
	// monomial was actually inserted.  
{
  tagged_monomial *old_b;
  monomial m = b->monom;

  if (search(m, old_b)) 
    {
      deleteitem(b);
      return 0;
    }
  insert_minimal(b);
  return 1;
}

void MonomialLookupTable::text_out(buffer &o) const
{
  o << "MonomialLookupTable (";
  o << count << " entries)\n";
  int i = 0;
  for (Index<MonomialLookupTable> j = last(); j.valid(); j--)
    {
      if ((++i) % 15 == 0)
	o << newline;
      monomial n = operator[](j)->monom;
      monomial_text_out(o,n);
      if (gbTrace > 0)
	o << '(' << reinterpret_cast<long>(operator[](j)->bag) << ")";
      o << ' ';
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
