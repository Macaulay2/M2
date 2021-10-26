// (c) 1994 Michael E. Stillman

#include "monideal.hpp"
#include "monoid.hpp"
#include "text-io.hpp"

unsigned int MonomialIdeal::computeHashValue() const
{
  // Incorporate the length and the first 5 elements
  unsigned int hashval = length();
  int count = 0;
  Index<MonomialIdeal> i = first();
  while (i.valid() and count < 5)
    {
      const int *m = operator[](i)->monom().raw();
      hashval = 4436435 * hashval + varpower::computeHashValue(m);
      i++;
      count++;
    }
  return hashval;
}

void MonomialIdeal::remove_MonomialIdeal()
{
  delete_mi_node(mi);
  if ((count % 2) == 1) delete mi_stash;
}

Nmi_node *MonomialIdeal::new_mi_node(int v, int e, Nmi_node *d)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = NULL;
  p->right = NULL;
  p->header = NULL;
  p->tag = Nmi_node::node;
  p->val.down = d;
  return p;
}

Nmi_node *MonomialIdeal::new_mi_node(int v, int e, Bag *b)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = NULL;
  p->right = NULL;
  p->header = NULL;
  p->tag = Nmi_node::leaf;
  p->val.bag = b;
  return p;
}

void MonomialIdeal::delete_mi_node(Nmi_node *p)
{
  if (p == 0) return;
  if (p->right != p->header) delete_mi_node(p->right);
  if (p->tag == Nmi_node::node)
    {
      if (p->header != p) delete_mi_node(p->down());
    }
  else
    delete p->baggage();
  mi_stash->delete_elem(p);
}

MonomialIdeal::MonomialIdeal(const PolynomialRing *RR, stash *mi_stash0)
    : R(RR), mi(0), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == 0)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }
}

MonomialIdeal::MonomialIdeal(const PolynomialRing *R0,
                             queue<Bag *> &elems,
                             queue<Bag *> &rejects,
                             stash *mi_stash0)
    : R(R0), mi(0), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == 0)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }
  VECTOR(queue<Bag *> *) bins;
  Bag *b, *b1;
  while (elems.remove(b))
    {
      int d = varpower::simple_degree(b->monom().raw());
      if (d >= bins.size())
        for (int i = bins.size(); i <= d; i++) bins.push_back(NULL);
      if (bins[d] == NULL) bins[d] = new queue<Bag *>;
      bins[d]->insert(b);
    }
  int n = get_ring()->n_vars();
  int *exp = newarray_atomic(int, n);
  for (int i = 0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
        while (bins[i]->remove(b))
          {
            const int *mon = b->monom().raw();
            varpower::to_ntuple(n, mon, exp);
            if (search_expvector(exp, b1))
              rejects.insert(b);
            else
              insert_minimal(b);
          }
        delete bins[i];
      }
}

MonomialIdeal::MonomialIdeal(const PolynomialRing *R0,
                             queue<Bag *> &elems,
                             stash *mi_stash0)
    : R(R0), mi(0), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == 0)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }
  VECTOR(queue<Bag *> *) bins;
  Bag *b;
  while (elems.remove(b))
    {
      int d = varpower::simple_degree(b->monom().raw());
      if (d >= bins.size())
        for (int i = bins.size(); i <= d; i++) bins.push_back(NULL);
      if (bins[d] == NULL) bins[d] = new queue<Bag *>;
      bins[d]->insert(b);
    }
  for (int i = 0; i < bins.size(); i++)
    if (bins[i] != NULL)
      {
        while (bins[i]->remove(b)) insert(b);
        delete bins[i];
      }
}

const int *MonomialIdeal::first_elem() const
{
  return first_node()->monom().raw();
}

const int *MonomialIdeal::second_elem() const
{
  return next(first_node())->monom().raw();
}

MonomialIdeal *MonomialIdeal::copy() const
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    result->insert_minimal(new Bag(*(operator[](i))));
  return result;
}

bool MonomialIdeal::is_equal(const MonomialIdeal &mi0) const
{
  if (this == &mi0) return true;
  if (length() != mi0.length()) return false;
  Index<MonomialIdeal> i = first();
  Index<MonomialIdeal> j = mi0.first();
  while (i.valid())
    {
      const int *m = operator[](i)->monom().raw();
      const int *n = mi0[j]->monom().raw();
      if (!varpower::is_equal(m, n)) return false;
      i++;
      j++;
    }
  return true;
}

int MonomialIdeal::search_expvector(const int *exp, Bag *&b) const
{
  if (mi == NULL) return 0;

  Nmi_node *p = mi;

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

      if (p->tag == Nmi_node::leaf)
        {
          b = p->baggage();
          return 1;
        }

      p = p->down();
    }
}

void MonomialIdeal::find_all_divisors(const int *exp, VECTOR(Bag *)& b) const
{
  b.clear();
  if (mi == NULL) return;

  Nmi_node *p = mi;

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

      if (p->tag == Nmi_node::leaf)
        {
          b.push_back(p->baggage());
        }
      else
        p = p->down();
    }
}

int MonomialIdeal::search(const int *m, Bag *&b) const
{
  int *exp = newarray_atomic(int, get_ring()->n_vars());
  varpower::to_ntuple(get_ring()->n_vars(), m, exp);
  int result = search_expvector(exp, b);
  freemem(exp);
  return result;
}

Nmi_node *MonomialIdeal::next(Nmi_node *p) const
{
  while (p != NULL)
    {
      p = p->left;
      if (p->tag == Nmi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return NULL;
}

void *MonomialIdeal::next(void *p) const
{
  return reinterpret_cast<void *>(next(reinterpret_cast<Nmi_node *>(p)));
}

Nmi_node *MonomialIdeal::prev(Nmi_node *p) const
{
  while (p != NULL)
    {
      p = p->right;
      if (p->tag == Nmi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return NULL;
}

void *MonomialIdeal::prev(void *p) const
{
  return reinterpret_cast<void *>(prev(reinterpret_cast<Nmi_node *>(p)));
}

void MonomialIdeal::insert1(Nmi_node *&top, Bag *b)
{
  Nmi_node **p = &top, *up = NULL;
  int one_element = 1;

  for (index_varpower i = b->monom().raw(); i.valid();)
    {
      one_element = 0;
      int insert_var = i.var();
      int insert_exp;

      if (*p == NULL)
        {
          // make a new header node
          *p = new_mi_node(insert_var, 0, up);
          (*p)->header = (*p)->left = (*p)->right = *p;
        }
      else if ((*p)->var < insert_var)
        {
          // make a new layer
          Nmi_node *header_node, *zero_node;
          header_node = new_mi_node(insert_var, 0, up);
          zero_node = new_mi_node(insert_var, 0, *p);

          header_node->left = header_node->right = zero_node;
          (*p)->down() = zero_node;
          *p = header_node->header = zero_node->header = zero_node->left =
              zero_node->right = header_node;
        }

      if ((*p)->var > insert_var)
        {
          insert_var = (*p)->var;
          insert_exp = 0;
        }
      else
        {
          insert_exp = i.exponent();
          ++i;
        }

      Nmi_node *q = (*p)->right;
      while ((q != q->header) && (q->exp < insert_exp)) q = q->right;
      if (q->exp != insert_exp)
        {
          Nmi_node *insert_node;

          if (i.valid())
            {
              insert_node = new_mi_node(
                  insert_var, insert_exp, reinterpret_cast<Nmi_node *>(NULL));
              q->insert_to_left(insert_node);
              q = insert_node;
            }
          else
            {
              insert_node = new_mi_node(insert_var, insert_exp, b);
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
      top = new_mi_node(0, 0, reinterpret_cast<Nmi_node *>(NULL));
      Nmi_node *leaf_node = new_mi_node(0, 0, b);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header = leaf_node->left = leaf_node->right =
          top;
    }
}

void MonomialIdeal::remove1(Nmi_node *p)
{
  assert(p != NULL);
  assert(p->tag == Nmi_node::leaf);
  p->baggage() = NULL;
  count -= 2;

  for (; p != NULL;)
    {
      p->left->right = p->right;
      p->right->left = p->left;
      Nmi_node *q = p->header;
      p->left = p->right = NULL;
      delete_mi_node(p);

      if (q->right == q->header)  // only the header is left, so delete it
        {
          p = q->down();
          q->down() = NULL;
          if (p != NULL) p->down() = NULL;
          delete_mi_node(q);
          continue;
        }

      if (q->left != q->right) return;

      if (q->left->exp > 0) return;

      Nmi_node *dad = q->down();
      if (q->left->tag == Nmi_node::leaf)
        {
          // set parent of q to be a leaf with baggage of q->left
          // since this is a leaf, dad should be non null
          assert(dad != NULL);
          dad->tag = Nmi_node::leaf;
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
      delete_mi_node(q);  // Deletes both nodes q, q->left.
      return;
    }
  if (p == NULL) mi = NULL;
}

int MonomialIdeal::remove(Bag *&b)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(next(mi));
  if (p == NULL) return 0;
  b = p->baggage();
  remove1(p);
  return 1;
}

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
      for (i = 1; i <= indent; i++) o << ' ';
      o << p->var << ' ' << p->exp;
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
  while (q != p)
    {
      do_node(q, indent, disp);
      if (q->tag != Nmi_node::leaf)
        do_tree(q->down(), depth + 1, indent + 2, disp);
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
  if (mi != NULL) do_tree(mi, 0, 0, disp);
  buffer o;
  o << "list nodes     = " << nlists << newline;
  o << "internal nodes = " << nnodes << newline;
  o << "monomials      = " << nleaves << newline;
  o << "max depth      = " << ndepth << newline;
  emit(o.str());
}

int MonomialIdeal::debug_check(Nmi_node *const p,
                               const Nmi_node *const up) const
// Returns the number of leaves at tree with root p.
// Make sure that the list header is constructed ok, that the
// left/right pointers are ok on this level, that the
// var, exp, values in this train are correct.
// Then loop through, checking each node (recursively) and each leaf
{
  Nmi_node *q;
  // First check the node 'p' itself
  assert(p != NULL);
  assert(p->var >= 0);
  if (up != NULL) assert(p->var < up->var);
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
      assert(q->var == p->var);
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
  if (count == 0)
    {
      assert(mi == NULL);
      return;
    }
  assert(mi != NULL);
  assert(debug_check(mi, NULL) == count / 2);
}

int MonomialIdeal::insert(Bag *b)
// Insert the monomial (and baggage) 'm', if it
// is not already in the monomial ideal.  Return whether the
// monomial was actually inserted.
{
  Bag *old_b;
  const int *m = b->monom().raw();

  if (search(m, old_b))
    {
      delete b;
      return 0;
    }
  insert_minimal(b);
  return 1;
}

void MonomialIdeal::text_out(buffer &o) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  assert(P != 0);
  const Monoid *M = P->getMonoid();
  int *m = M->make_one();
  for (Index<MonomialIdeal> j = last(); j.valid(); j--)
    {
      const int *n = operator[](j)->monom().raw();
      M->from_varpower(n, m);
      M->elem_text_out(o, m);
      if (M2_gbTrace > 0) o << '(' << operator[](j)->basis_elem() << ")";
      o << ' ';
    }
  M->remove(m);
}

// Original intersect code, pre 1/2/2006.  We wish to speed this up
//   for large monomial ideals.
// MonomialIdeal *MonomialIdeal::intersect(const MonomialIdeal &J) const
// {
//   queue<Bag *> new_elems;
//   for (Index<MonomialIdeal> i = first(); i.valid(); i++)
//     for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
//       {
//      Bag *b = new Bag(operator[](i)->basis_elem());
//      varpower::lcm(operator[](i)->monom().raw(),
//                    J[j]->monom().raw(), b->monom());
//      new_elems.insert(b);
//       }
//   MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
//   return result;
// }

MonomialIdeal *MonomialIdeal::intersect(const MonomialIdeal &J) const
{
  // The idea: take the elements of 'this'
  //   for each: if the element is in J, then keep it directly.
  //      otherwie compute the lcm's.
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *c;
      if (J.search(operator[](i)->monom().raw(), c))
        {
          Bag *b = new Bag(operator[](i));
          new_elems.insert(b);
        }
      else
        for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
          {
            Bag *b = new Bag(operator[](i)->basis_elem());
            varpower::lcm(operator[](i)->monom().raw(),
                          J[j]->monom().raw(),
                          b->monom());
            new_elems.insert(b);
          }
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::intersect(const int *m) const
// Compute (this : m), where m is a varpower monomial.
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::lcm(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::operator*(const MonomialIdeal &J) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
      {
        Bag *b = new Bag(operator[](i)->basis_elem());
        varpower::mult(operator[](i)->monom().raw(),
                       J[j]->monom().raw(),
                       b->monom());
        new_elems.insert(b);
      }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::operator+(const MonomialIdeal &J) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i));
      new_elems.insert(b);
    }
  for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
    {
      Bag *b = new Bag(J[j]);
      new_elems.insert(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::operator-(const MonomialIdeal &J) const
// Create the monomial ideal consisting of those elements of 'this'
// that are not in 'J'.  The baggage is left the same.
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *c;
      if (!J.search(operator[](i)->monom().raw(), c))
        {
          Bag *b = new Bag(operator[](i));
          result->insert_minimal(b);
        }
    }
  return result;
}

MonomialIdeal *MonomialIdeal::quotient(const int *m) const
// Compute (this : m), where m is a varpower monomial.
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::quotient(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::quotient(const MonomialIdeal &J) const
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Index<MonomialIdeal> i = J.first(); i.valid(); i++)
    {
      MonomialIdeal *result1 = quotient(operator[](i)->monom().raw());
      MonomialIdeal *next_result = result->intersect(*result1);
      delete result1;
      delete result;
      result = next_result;
    }
  return result;
}

static MonomialIdeal *varpower_monideal(const PolynomialRing *R,
                                        const M2_arrayint top,
                                        const int *vp)
{
  // If m is a varpower monomial, xi1^a1 ... xin^an, create the monomial ideal
  // (xi1^(top[i1]+1-a1), ..., xin^(top[in]+1-an))
  MonomialIdeal *result = new MonomialIdeal(R);
  for (index_varpower i = vp; i.valid(); ++i)
    {
      Bag *b = new Bag();
      varpower::var(
          i.var(), top->array[i.var()] + 1 - i.exponent(), b->monom());
      result->insert(b);
    }
  return result;
}
M2_arrayint MonomialIdeal::lcm() const
// Returns the lcm of all of the generators of this, as an array of ints
{
  M2_arrayint result = M2_makearrayint(get_ring()->n_vars());
  for (int i = 0; i < result->len; i++) result->array[i] = 0;

  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    for (index_varpower j = operator[](i)->monom().raw(); j.valid(); ++j)
      if (result->array[j.var()] < j.exponent())
        result->array[j.var()] = j.exponent();
  return result;
}

MonomialIdeal *MonomialIdeal::alexander_dual(const M2_arrayint a) const
// a is a vector which is entrywise >= lcm(this).
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      MonomialIdeal *I1 =
          varpower_monideal(get_ring(), a, operator[](i)->monom().raw());
      MonomialIdeal *next_result = result->intersect(*I1);
      delete I1;
      delete result;
      result = next_result;
    }
  return result;
}

MonomialIdeal *MonomialIdeal::erase(const int *m) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::erase(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::sat(const MonomialIdeal &J) const
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Index<MonomialIdeal> i = J.first(); i.valid(); i++)
    {
      MonomialIdeal *result1 = erase(operator[](i)->monom().raw());
      MonomialIdeal *next_result = result->intersect(*result1);
      delete result1;
      delete result;
      result = next_result;
    }
  return result;
}

MonomialIdeal *MonomialIdeal::radical() const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::radical(operator[](i)->monom().raw(), b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

static void borel1(queue<Bag *> &result, int *m, int loc, int nvars)
{
  if (loc == 0)
    {
      Bag *b = new Bag();
      varpower::from_ntuple(nvars, m, b->monom());
      result.insert(b);
    }
  else
    {
      int a = m[loc];
      for (int i = 0; i <= a; i++)
        {
          borel1(result, m, loc - 1, nvars);
          m[loc]--;
          m[loc - 1]++;
        }
      m[loc] += a + 1;
      m[loc - 1] -= a + 1;
    }
}

MonomialIdeal *MonomialIdeal::borel() const
// Return the smallest borel monomial ideal containing 'this'.
{
  queue<Bag *> new_elems;
  int *bexp = newarray_atomic(int, get_ring()->n_vars());
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = operator[](i);
      varpower::to_ntuple(get_ring()->n_vars(), b->monom().raw(), bexp);
      borel1(new_elems, bexp, get_ring()->n_vars() - 1, get_ring()->n_vars());
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  freemem(bexp);
  return result;
}

bool MonomialIdeal::is_borel() const
{
  int *bexp = newarray_atomic(int, get_ring()->n_vars());
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = operator[](i);
      Bag *c;
      varpower::to_ntuple(get_ring()->n_vars(), b->monom().raw(), bexp);
      for (int j = get_ring()->n_vars() - 1; j >= 1; j--)
        if (bexp[j] > 0)
          {
            bexp[j]--;
            bexp[j - 1]++;
            int isthere = search_expvector(bexp, c);
            bexp[j]++;
            bexp[j - 1]--;
            if (!isthere) return 0;
          }
    }
  freemem(bexp);
  return 1;
}

bool MonomialIdeal::is_one() const
{
  if (length() != 1) return false;
  Nmi_node *p = mi->left;
  if (p->var != 0 || p->exp != 0 || p->tag != Nmi_node::leaf) return false;
  return true;
}

int MonomialIdeal::n_pure_powers() const
// Is each variable to some power in the monideal?
{
  int npure = 0;
  int v, e;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      int *m = operator[](i)->monom().raw();
      if (varpower::is_pure_power(m, v, e)) npure++;
    }
  return npure;
}

// Other routines to add:
//   primary_decomposition(J)
//   partition(J)

//   linear versions of: quotient, ...
//   hilbert series

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
