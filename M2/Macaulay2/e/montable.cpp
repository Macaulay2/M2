// Copyright 1999 by Michael E. Stillman

#include "montable.hpp"
#include "text_io.hpp"

template <class Tag>
void display(MonomialTable<Tag> &mi)
{
  buffer o;
  for (MonomialTable<Tag>::iterator p = mi.first(); p.valid(); ++p)
    {
      const MonomialTable<Tag>::tagged_monomial *t = *p;
      varpower::elem_text_out(o, p.get_monomial().raw());
      o << "(";
      o << p.get_tag();
      o << ") ";
    }
  o << newline;
  emit(o.str());
}
#if 0
void test_monideal2()
{
  // Here is a handful of monomials
  int nmonomials;
  intarray *mon;

  mon = new intarray[10];
  nmonomials = 1;

  // First test: insert a handful of monomials
  MonomialTable<int> mi;
  typedef MonomialTable<int>::tagged_monomial tagged_monomial;

  for (int i=0; i<nmonomials; i++)
    {
      tagged_monomial *b = new tagged_monomial(mon[i], i);
      mi.insert_minimal(b);
    }

  display<int>(mi);

  // Creation.

  // Find

  // Insertion

  // Removal

  // Display
}
#endif
#if 0
extern int comp_printlevel;

stash *MonomialIdeal_rec::mystash;
stash *Nmi_node::mystash;
stash *monideal_pair::mystash;

static int nlists = 0;
static int nleaves = 0;
static int nnodes = 0;
static int ndepth = 0;

void MonomialIdeal::do_node(Nmi_node *p, int indent, int disp) const
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
  if (p->tag == Nmi_node::leaf)
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

void MonomialIdeal::do_tree(Nmi_node *p, int depth, int indent, int disp) const
{
  if (depth > ndepth) ndepth = depth;
  do_node(p, indent, disp);
  Nmi_node *q = p->right;
  while (q != p) {
    do_node(q, indent, disp);
    if (q->tag != Nmi_node::leaf)
      do_tree(q->down(), depth+1, indent+2, disp);
    q = q->right;
  }
}

void MonomialIdeal::debug_out(int disp) const
     // Display MonomialIdeal in tree-like form, collect statistics
{
  nlists = 0;
  nnodes = 0;
  nleaves = 0;
  ndepth = 0;
  if (obj->mi != NULL) do_tree(obj->mi, 0, 0, disp);
  buffer o;
  o << "list nodes     = " << nlists << newline;
  o << "internal nodes = " << nnodes << newline;
  o << "monomials      = " << nleaves << newline;
  o << "max depth      = " << ndepth << newline;
  emit(o.str());
}

int MonomialIdeal::debug_check(Nmi_node *p, Nmi_node *up) const
     // Returns the number of leaves at tree with root p.
     // Make sure that the list header is constructed ok, that the 
     // left/right pointers are ok on this level, that the
     // var, exp, values in this train are correct. 
     // Then loop through, checking each node (recursively) and each leaf
{
  Nmi_node *q;
  // First check the node 'p' itself
  assert(p != NULL);
#ifndef NDEBUG
  int v = p->var;
#endif
  assert(v >= 0);
  if (up != NULL) 
    assert(v < up->var);
  assert(p->header == p);
  assert(p->tag == Nmi_node::node);
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
    if (q->tag == Nmi_node::node) 
      c += debug_check(q->down(), q);
    else
      c++;
  return c;
}

void MonomialIdeal::debug_check() const
{
  if (obj->count == 0) 
    {
      assert(obj->mi == NULL);
      return;
    }
  assert(obj->mi != NULL);
  assert(debug_check(obj->mi, NULL) == obj->count);
}

#endif
