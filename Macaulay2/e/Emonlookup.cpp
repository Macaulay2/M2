// Copyright 1999 Michael E. Stillman

#include "Emonlookup.hpp"
#include "text_io.hpp"

extern int comp_printlevel;

stash *EMonomialLookupTable::mystash;
stash *EMInode::mystash;

void EMonomialLookupTable::remove_nodes(EMInode *&p)
{
  if (p->right != p->header) delete p->right;
  if (p->tag == p->node)
    {
      if (p->header != p) delete p->down();
    }
  else
    delete p->baggage();
}

bool EMonomialLookupTable::search_expvector(const int *exp, Bag *&b) const
{
  if (mi == NULL) return false;

  EMInode *p = mi;

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

      if (p->tag == EMInode::leaf) 
	{
	  b = p->baggage();
	  return true;
	} 

      p = p->down();	
    }
}

void EMonomialLookupTable::find_all_divisors(const int *exp, array<Bag *> &b) const
{
  b.shrink(0);
  if (mi == NULL) return;

  EMInode *p = mi;

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

      if (p->tag == EMInode::leaf) 
	{
	  b.append(p->baggage());
	} 
      else
	p = p->down();	
    }
}

bool EMonomialLookupTable::search(const int *m, Bag *&b) const
{
  static intarray exp;
  exp.shrink(0);
  int nvars = topvar();
  if (nvars < 0) return false;
  varpower::to_ntuple(nvars, m, exp);
  return search_expvector(exp.raw(), b);
}

EMInode *EMonomialLookupTable::next(EMInode *p)
{
  while (p != NULL) 
    {
      p = p->left;
      if (p->tag == EMInode::leaf)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

EMInode *EMonomialLookupTable::prev(EMInode *p)
{
  while (p != NULL) 
    {
      p = p->right;
      if (p->tag == EMInode::leaf)
	return p;
      else
	p = p->down();
    }
  return NULL;
}

void EMonomialLookupTable::insert1(EMInode *&top, Bag *b)
{
  EMInode **p = &top, *up = NULL;
  int one_element = 1;

  for (index_varpower i = b->monom().raw(); i.valid();)
    {
      one_element = 0;
      int insert_var = i.var();
      int insert_exp;
      
      if (*p == NULL)
	{
	  // make a new header node
	  *p = new EMInode(insert_var, 0, up);
	  (*p)->header = (*p)->left = (*p)->right = *p;
	}
      else if ((*p)->var < insert_var)
	{
	  // make a new layer
	  EMInode *header_node, *zero_node;
	  header_node = new EMInode(insert_var, 0, up);
	  zero_node   = new EMInode(insert_var, 0, *p);

	  header_node->left = header_node->right = zero_node;
	  (*p)->down() = zero_node;
	  *p = header_node->header = zero_node->header 
	    = zero_node->left = zero_node->right = header_node;
	}

      if ((*p)->var > insert_var)
	{ insert_var = (*p)->var; insert_exp = 0; }
      else
	{ insert_exp = i.exponent(); ++i; }

      EMInode *q = (*p)->right;
      while((q != q->header) && (q->exp < insert_exp)) q = q->right;
      if (q->exp != insert_exp)
	{
	  EMInode *insert_node;

	  if (i.valid())
	    {
	      insert_node
		= new EMInode(insert_var, insert_exp,
					(EMInode *)NULL);
	      q->insert_to_left(insert_node);
	      q = insert_node;
	    }
	  else
	    {
	      insert_node
		= new EMInode(insert_var, insert_exp, b);
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
      top = new EMInode(0, 0, (EMInode *) NULL);
      EMInode *leaf_node = new EMInode(0, 0, b);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header 
	= leaf_node->left = leaf_node->right = top;
    }
}

void EMonomialLookupTable::remove1(EMInode *p)
{
  assert(p != NULL);
  assert(p->tag == EMInode::leaf);
  p->baggage() = NULL;
  count--;

  for(;p != NULL;)
    {
      p->left->right = p->right;
      p->right->left = p->left;
      EMInode *q = p->header;
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

      EMInode *dad = q->down();
      if (q->left->tag == EMInode::leaf)
	{
	  // set parent of q to be a leaf with baggage of q->left
	  // since this is a leaf, dad should be non null
	  assert(dad != NULL);
	  dad->tag = EMInode::leaf;
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

int EMonomialLookupTable::remove(Bag *&b)
{
  EMInode *p = (EMInode *) next(mi);
  if (p == NULL) return 0;
  b = p->baggage();
  remove1(p);
  return 1;
}

static int nlists = 0;
static int nleaves = 0;
static int nnodes = 0;
static int ndepth = 0;

void EMonomialLookupTable::do_node(EMInode *p, int indent, int disp) const
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
  if (p->tag == EMInode::leaf)
    {
      nleaves++;
      if (disp) o << ' ';
      varpower::elem_text_out(o, p->baggage()->monom().raw());
      o << '(';
      o << p->baggage()->basis_elem();
      o << ')';
    }
  else if (p == p->header)
    nlists++;
  else
    nnodes++;
  emit_line(o.str());
}

void EMonomialLookupTable::do_tree(EMInode *p, int depth, int indent, int disp) const
{
  if (depth > ndepth) ndepth = depth;
  do_node(p, indent, disp);
  EMInode *q = p->right;
  while (q != p) {
    do_node(q, indent, disp);
    if (q->tag != EMInode::leaf)
      do_tree(q->down(), depth+1, indent+2, disp);
    q = q->right;
  }
}

void EMonomialLookupTable::debug_out(int disp) const
     // Display EMonomialLookupTable in tree-like form, collect statistics
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

int EMonomialLookupTable::debug_check(EMInode *p, EMInode *up) const
     // Returns the number of leaves at tree with root p.
     // Make sure that the list header is constructed ok, that the 
     // left/right pointers are ok on this level, that the
     // var, exp, values in this train are correct. 
     // Then loop through, checking each node (recursively) and each leaf
{
  EMInode *q;
  // First check the node 'p' itself
  assert(p != NULL);
#ifndef NDEBUG
  int v = p->var;
#endif
  assert(v >= 0);
  if (up != NULL) 
    assert(v < up->var);
  assert(p->header == p);
  assert(p->tag == EMInode::node);
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
    if (q->tag == EMInode::node) 
      c += debug_check(q->down(), q);
    else
      c++;
  return c;
}

void EMonomialLookupTable::debug_check() const
{
  if (count == 0) 
    {
      assert(mi == NULL);
      return;
    }
  assert(mi != NULL);
  assert(debug_check(mi, NULL) == count);
}

bool EMonomialLookupTable::insert(Bag *b)
        // Insert the monomial (and baggage) 'm', if it
	// is not already in the monomial ideal.  Return whether the
	// monomial was actually inserted.  
{
  Bag *old_b;
  const int *m = b->monom().raw();

  if (search(m, old_b)) 
    {
      delete b;
      return false;
    }
  insert_minimal(b);
  return true;
}


