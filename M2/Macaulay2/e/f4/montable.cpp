// (c) 1994-2006 Michael E. Stillman

#include "../text_io.hpp"

#include "varpower_monomial.hpp"
#include "montable.hpp"
//#include "../linalgGB/Monomials.hpp"
//#include "../linalgGB/monoms.h"


extern int gbTrace;

template <typename Key>
mi_node<Key>::~mi_node()
{
  if (right != header) deleteitem(right);
  if (tag == node)
    {
      if (header != this) deleteitem(down());
    }
}

template <typename Key>
MonomialLookupTable<Key>::MonomialLookupTable()
  : mi(0), count(0)
{
  size_of_exp = 10;
  exp0 = newarray(ntuple_word,size_of_exp);
}

#if 0
// The following 2 routines should be present, but not as constructors.
MonomialLookupTable::MonomialLookupTable(VECTOR(tagged_monomial *) &elems, 
					 VECTOR(tagged_monomial *) &rejects)
  : mi(0), count(0)
{
  size_of_exp = 10;
  exp0 = newarray(int,size_of_exp);
  VECTOR( VECTOR(tagged_monomial *) *) bins;
  tagged_monomial *b1;
  for (VECTOR(tagged_monomial *)::iterator j = elems.begin(); j != elems.end(); j++)
    {
      tagged_monomial *b = *j;
      int d = monomial_simple_degree(b->monom);
      if (d >= bins.size())
	for (int i=bins.size(); i<=d; i++)
	  bins.push_back(NULL);
      if (bins[d] == NULL)
	bins[d] = new VECTOR(tagged_monomial *);
      bins[d]->push_back(b);
    }
  for (int i=0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
	for (vector_tagged_monomials::iterator j = bins[i]->begin(); j != bins[i]->end(); j++)
	  {
	    tagged_monomial *b = *j;
	    monomial mon = b->monom;
	    if (search(mon, b1))
	      rejects.push_back(b);
	    else
	      insert_minimal(b);
	  }
	deleteitem(bins[i]);
      }
}

MonomialLookupTable::MonomialLookupTable(VECTOR(tagged_monomial *) &elems)
  : mi(0), count(0)
{
  size_of_exp = 10;
  exp0 = newarray(int,size_of_exp);
  VECTOR( VECTOR(tagged_monomial *) *) bins;
  for (VECTOR(tagged_monomial *)::iterator j = elems.begin(); j != elems.end(); j++)
    {
      tagged_monomial *b = *j;
      int d = monomial_simple_degree(b->monom);
      if (d >= bins.size())
	for (int i=bins.size(); i<=d; i++)
	  bins.push_back(NULL);
      if (bins[d] == NULL)
	bins[d] = new VECTOR(tagged_monomial *);
      bins[d]->push_back(b);
    }
  for (int i=0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
	for (vector_tagged_monomials::iterator j = bins[i]->begin(); j != bins[i]->end(); j++)
	  {
	    tagged_monomial *b = *j;
	    insert(b);
	  }
	deleteitem(bins[i]);
      }
}
#endif

template <typename Key>
int MonomialLookupTable<Key>::search_expvector(const_ntuple_monomial exp, Key &result_k) const
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
	  result_k = p->key();
	  return 1;
	} 

      p = p->down();	
    }
}

template <typename Key>
void MonomialLookupTable<Key>::find_all_divisors(ntuple_monomial exp, VECTOR(Key) &result_k) const
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
	  result_k.push_back(p->key());
	} 
      else
	p = p->down();	
    }
}

template <typename Key>
void MonomialLookupTable<Key>::update_exponent_vector(const_varpower_monomial m)
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

      exp0 = newarray(ntuple_word,size_of_exp);
      for (int i=0; i<size_of_exp; i++) exp0[i] = 0;
    }

  int nparts = *m++;
  for (int i=nparts; i>0; i--, m+=2)
    {
      exp0[*m] = m[1];
    }
}

template <typename Key>
void MonomialLookupTable<Key>::reset_exponent_vector(const_varpower_monomial m)
{
  int nparts = *m++;
  for (int i=nparts; i>0; i--, m+=2)
    {
      exp0[*m] = 0;
    }
}

template <typename Key>
int MonomialLookupTable<Key>::search(const_varpower_monomial m, Key &b) const
{
  MonomialLookupTable *me = const_cast<MonomialLookupTable *>(this);
  me->update_exponent_vector(m);
  int result = search_expvector(exp0, b);
  me->reset_exponent_vector(m);
  return result;
}

template <typename Key>
mi_node<Key> *MonomialLookupTable<Key>::next(mi_node *p) const
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

template <typename Key>
mi_node<Key> *MonomialLookupTable<Key>::prev(mi_node *p) const
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

template <typename Key>
void MonomialLookupTable<Key>::insert1(mi_node *&top, const_varpower_monomial b, Key k)
{
  count++;
  mi_node **p = &top, *up = NULL;
  int one_element = 1;

  for (index_varpower_monomial i = b; i.valid();)
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
		= new mi_node(insert_var, insert_exp, k);
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
      mi_node *leaf_node = new mi_node(0, 0, k);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header 
	= leaf_node->left = leaf_node->right = top;
    }
}

template <typename Key>
void MonomialLookupTable<Key>::remove1(mi_node *p)
{
  assert(p != NULL);
  assert(p->tag == mi_node::leaf);
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
	  // set parent of q to be a leaf with key of q->left
	  // since this is a leaf, dad should be non null
	  assert(dad != NULL);
	  dad->tag = mi_node::leaf;
	  dad->key() = q->left->key();
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

template <typename Key>
int MonomialLookupTable<Key>::remove(Key &result_k)
{
  mi_node *p = reinterpret_cast<mi_node *>(next(mi));
  if (p == NULL) return 0;
  result_k = p->key();
  remove1(p);
  return 1;
}

template <typename Key>
int MonomialLookupTable<Key>::insert(const_varpower_monomial m, Key k)
        // Insert the monomial 'm' with key 'k', if it
	// is not already in the monomial ideal.  Return whether the
	// monomial was actually inserted.  
{
  Key old_key;

  if (search(m, old_key)) 
    return 0;
  insert_minimal(m, k);
  return 1;
}

static int nlists = 0;
static int nleaves = 0;
static int nnodes = 0;
static int ndepth = 0;

template <typename Key>
void MonomialLookupTable<Key>::do_node(mi_node *p, int indent, int disp) const
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
      if (disp) o << ' ' << reinterpret_cast<long>(p->key());
    }
  else if (p == p->header)
    nlists++;
  else
    nnodes++;
  emit_line(o.str());
}

template <typename Key>
void MonomialLookupTable<Key>::do_tree(mi_node *p, int depth, int indent, int disp) const
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

template <typename Key>
void MonomialLookupTable<Key>::debug_out(int disp) const
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

template <typename Key>
int MonomialLookupTable<Key>::debug_check(mi_node *p, mi_node *up) const
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

template <typename Key>
void MonomialLookupTable<Key>::debug_check() const
{
  if (count == 0) 
    {
      assert(mi == NULL);
      return;
    }
  assert(mi != NULL);
  assert(debug_check(mi, NULL) == count);
}

template <typename Key>
void MonomialLookupTable<Key>::text_out(buffer &o) const
{
  o << "MonomialLookupTable (";
  o << count << " entries)\n";
  int i = 0;
  for (iterator j = this; j.valid(); j--)
    {
      if ((++i) % 15 == 0)
	o << newline;
      //monomial n = (*j)->monom;
      //monomial_text_out(o,n);
	o << reinterpret_cast<long>(*j) << "  ";
    }
}

template class MonomialLookupTable<packed_monomial>;
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
