// Copyright 1994-2021 Michael E. Stillman

// TODO MES:
//  rename Nmi_node
//  rename new_mi_node
//  remove union in the Nmi_node: Baggage, down pointers will always be there.
//  sat: do not loop if J is 0 or 1!
//       perhaps first call radical on J?  (or is this being done in m2 code?)
//  rewrite delete of a MonomialIdeal
//  MonomialIdeal ==> rename to e.g. MonomialLookupTable
//  A MonomialIdeal should be an engine object, which contains a MonomialLookupTable, and a ring.
// Maybe:
//   MonomialLookupTable
//   some functions in a namespace, otherwise global, that compute sat, quotient, radical, etc.
//   MonomialIdeal: engine object which is interned, and the monomial ideal is deleted on finalization.
// Memory layout for this data structure?
#include "monideal.hpp"
#include "monoid.hpp"
#include "text-io.hpp"

#include "debug.hpp"
#include <iostream>
#include <algorithm>

unsigned int MonomialIdeal::computeHashValue() const
{
  // Incorporate the size and the first 5 elements
  unsigned int hashval = size();
  int count = 0;
  for (Bag& b : *this)
    {
      if (count >= 5) break;
      const int *m = b.monom().raw();
      hashval = 4436435 * hashval + varpower::computeHashValue(m);
      count++;
    }
  return hashval;
}

void MonomialIdeal::remove_MonomialIdeal()
{
  delete_mi_node(mi);
  if ((count % 2) == 1) delete mi_stash;
}

Nmi_node *MonomialIdeal::new_internal_mi_node(int v, int e, Nmi_node *d)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = nullptr;
  p->right = nullptr;
  p->header = nullptr;
  p->tag = Nmi_node::node;
  p->val.down = d;
  return p;
}

Nmi_node *MonomialIdeal::new_leaf_mi_node(int v, int e, Bag *b)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = nullptr;
  p->right = nullptr;
  p->header = nullptr;
  p->tag = Nmi_node::leaf;
  p->val.bag = b;
  return p;
}

// TODO: this is recursive, not so good for large monomial ideals!
//   So replace this with something iterative.
// Really: should be named: delete_monideal_tree or something like that...
void MonomialIdeal::delete_mi_node(Nmi_node *p)
{
  if (p == nullptr) return;
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
    : R(RR), mi(nullptr), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == nullptr)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }
}


MonomialIdeal::MonomialIdeal(const PolynomialRing *R0,
                             VECTOR(Bag *) &elems, // we now own these elements
                             VECTOR(Bag *) &rejects, // except for the ones we place into here
                             stash *mi_stash0)
    : R(R0), mi(nullptr), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == nullptr)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }

  // create a vector of <simple degree, index> for each element of 'elems'.
  // sort them in increasing simple degree.
  // then loop through, adding them in.  This will insure that we only add in
  // minimal generators.

  std::vector<std::pair<int, int>> degs_and_indices;
  int count = 0;
  for (auto& b : elems)
    {
      int deg = varpower::simple_degree(b->monom().raw());
      degs_and_indices.push_back(std::make_pair(deg, count));
      ++count;
    }
  std::stable_sort(degs_and_indices.begin(), degs_and_indices.end());

  for (auto p : degs_and_indices)
    {
      Bag* b = elems[p.second];
      Bag* b1; // not used here...
      if (search(b->monom().raw(), b1))
        rejects.push_back(b);
      else
        insert_minimal(b);
    }
}

MonomialIdeal::MonomialIdeal(const PolynomialRing *R0,
                             VECTOR(Bag *) &elems, // we now own these elements
                             stash *mi_stash0)
    : R(R0), mi(nullptr), count(0), mi_stash(mi_stash0)
{
  if (mi_stash == nullptr)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(Nmi_node));
    }

  // create a vector of <simple degree, index> for each element of 'elems'.
  // sort them in increasing simple degree.
  // then loop through, adding them in.  This will insure that we only add in
  // minimal generators.

  std::vector<std::pair<int, int>> degs_and_indices;
  int count = 0;
  for (auto& b : elems)
    {
      int deg = varpower::simple_degree(b->monom().raw());
      degs_and_indices.push_back(std::make_pair(deg, count));
      ++count;
    }
  std::stable_sort(degs_and_indices.begin(), degs_and_indices.end());

  for (auto p : degs_and_indices)
    {
      Bag* b = elems[p.second];
      Bag* b1; // not used here...
      if (search(b->monom().raw(), b1))
        delete b;
      else
        insert_minimal(b);
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
  for (auto& b : *this)
    result->insert_minimal(new Bag(b));
  return result;
}

bool MonomialIdeal::is_equal(const MonomialIdeal &mi0) const
{
  if (this == &mi0) return true;
  if (size() != mi0.size()) return false;
  Iterator i = begin();
  Iterator j = mi0.begin();
  Iterator sentinel = end();
  while (i != sentinel)
    {
      const int *m = (*i).monom().raw();
      const int *n = (*i).monom().raw();
      if (!varpower::is_equal(m, n)) return false;
      i++;
      j++;
    }
  GC_reachable_here(&mi0);
  return true;
}

int MonomialIdeal::search_expvector(const int *exp, Bag *&b) const
{
  if (mi == nullptr) return 0;

  Nmi_node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
        {
          if ((p = p->down()) == nullptr) return 0;
          continue;
        }

      if (p->exp > exp[p->var])
        {
          if ((p = p->header->down()) == nullptr) return 0;
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
  if (mi == nullptr) return;

  Nmi_node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
        {
          if ((p = p->down()) == nullptr) return;
          continue;
        }

      if (p->exp > exp[p->var])
        {
          if ((p = p->header->down()) == nullptr) return;
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
  int* exp = ARRAY_ON_STACK(int, get_ring()->n_vars());
  varpower::to_ntuple(get_ring()->n_vars(), m, exp);
  int result = search_expvector(exp, b);
  return result;
}

Nmi_node *MonomialIdeal::next(Nmi_node *p) const
{
  while (p != nullptr)
    {
      p = p->left;
      if (p->tag == Nmi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return nullptr;
}

void *MonomialIdeal::next(void *p) const
{
  return reinterpret_cast<void *>(next(reinterpret_cast<Nmi_node *>(p)));
}

Nmi_node *MonomialIdeal::prev(Nmi_node *p) const
{
  while (p != nullptr)
    {
      p = p->right;
      if (p->tag == Nmi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return nullptr;
}

void *MonomialIdeal::prev(void *p) const
{
  return reinterpret_cast<void *>(prev(reinterpret_cast<Nmi_node *>(p)));
}

void MonomialIdeal::insert1(Nmi_node *&top, Bag *b)
{
  Nmi_node **p = &top, *up = nullptr;
  int one_element = 1;

  for (index_varpower i = b->monom().raw(); i.valid();)
    {
      one_element = 0;
      int insert_var = i.var();
      int insert_exp;

      if (*p == nullptr)
        {
          // make a new header node
          *p = new_internal_mi_node(insert_var, 0, up);
          (*p)->header = (*p)->left = (*p)->right = *p;
        }
      else if ((*p)->var < insert_var)
        {
          // make a new layer
          Nmi_node *header_node, *zero_node;
          header_node = new_internal_mi_node(insert_var, 0, up);
          zero_node = new_internal_mi_node(insert_var, 0, *p);

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
              insert_node = new_internal_mi_node(
                insert_var, insert_exp, static_cast<Nmi_node*>(nullptr));
              q->insert_to_left(insert_node);
              q = insert_node;
            }
          else
            {
              insert_node = new_leaf_mi_node(insert_var, insert_exp, b);
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
      top = new_internal_mi_node(0, 0, static_cast<Nmi_node *>(nullptr));
      Nmi_node *leaf_node = new_leaf_mi_node(0, 0, b);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header = leaf_node->left = leaf_node->right =
          top;
    }
}

void MonomialIdeal::remove1(Nmi_node *p)
{
  assert(p != nullptr);
  assert(p->tag == Nmi_node::leaf);
  p->baggage() = nullptr;
  count -= 2;

  for (; p != nullptr;)
    {
      p->left->right = p->right;
      p->right->left = p->left;
      Nmi_node *q = p->header;
      p->left = p->right = nullptr;
      delete_mi_node(p);

      if (q->right == q->header)  // only the header is left, so delete it
        {
          p = q->down();
          q->down() = nullptr;
          if (p != nullptr) p->down() = nullptr;
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
          assert(dad != nullptr);
          dad->tag = Nmi_node::leaf;
          dad->baggage() = q->left->baggage();
        }
      else
        {
          // set parent of q to be node pointing to q->left->down
          q->left->down()->down() = dad;
          if (dad != nullptr)
            dad->down() = q->left->down();
          else
            mi = q->left->down();
          q->left->down() = nullptr;
        }
      q->down() = nullptr;
      delete_mi_node(q);  // Deletes both nodes q, q->left.
      return;
    }
  if (p == nullptr) mi = nullptr;
}

int MonomialIdeal::remove(Bag *&b)
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(next(mi));
  if (p == nullptr) return 0;
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
  assert(p->left != nullptr);
  assert(p->right != nullptr);
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
  if (mi != nullptr) do_tree(mi, 0, 0, disp);
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
  assert(p != nullptr);
  assert(p->var >= 0);
  if (up != nullptr) assert(p->var < up->var);
  assert(p->header == p);
  assert(p->tag == Nmi_node::node);
  assert(p->down() == up);
  assert(p->left != nullptr);
  assert(p->right != nullptr);

  // Now loop through each element in left/right chain, checking that
  // v, e, left, right values are consistent.
  for (q = p->left; q != p; q = q->left)
    {
      assert(q->left != nullptr);
      assert(q->right != nullptr);
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
  if (count <= 1)
    {
      assert(mi == nullptr);
      return;
    }
  assert(mi != nullptr);
  assert(debug_check(mi, nullptr) == count / 2);
}

bool MonomialIdeal::isWellFormed() const
{
  if (mi == nullptr) return true; // nothing else to check.

  Nmi_node* p = mi;
  while (p != nullptr)
    {
      // check the current node
      if (p->left == nullptr) throw exc::engine_error("left node link is null");
      if (p->right == nullptr) throw exc::engine_error("right node link is null");
      if (p->header == nullptr) throw exc::engine_error("header node link is null");
      if (p->tag == Nmi_node::node and p != mi and p->val.down == nullptr) throw exc::engine_error("down node link is null");

      // now go to the next node
      // if this is a leaf, go right.
      // if this is a header, go down (up), and one left
      // if this is an internal node, go down (to subtree).
      if (p->tag == Nmi_node::node and p->header != p)
        p = p->val.down;
      else if (p->tag == Nmi_node::node and p->header == p)
        {
          // this is a header node (head of the double linked list at this level)
          // Let's check all of the elements in the double ring at this level.
          for (Nmi_node* q = p->right; q != p; q = q->right)
            {
              if (p->var != q->var) throw exc::engine_error("variable index is not consistent");
              if (q->left->right != q) throw exc::engine_error("the double link list is inconsistent");
              if (q->left != p and q->left->exp >= q->exp) throw exc::engine_error("exponents are not increasing going to the right");
            }
 
          // Now we continue 
          p = p->val.down;
          if (p != nullptr)
            p = p->right;
        }
      else if (p->tag == Nmi_node::leaf)
        {
          p = p->right;
        }
    }

  return true;
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
  assert(P != nullptr);
  const Monoid *M = P->getMonoid();
  if (size() == 0)
    {
      o << "0";
      return;
    }
  if (is_one())
    {
      if (size() != 1)
        std::cout << "bad news: count is not 1, but ideal is 1..." << std::endl;
      o << "1";
      return;
    }
  int *m = M->make_one();
  for (Bag& j : *this)
    {
      const int *n = j.monom().raw();
      M->from_varpower(n, m);
      M->elem_text_out(o, m);
      if (M2_gbTrace > 0) o << '(' << j.basis_elem() << ")";
      o << ' ';
    }
  M->remove(m);
}

MonomialIdeal *MonomialIdeal::intersect(const MonomialIdeal &J) const
{
  // The idea: take the elements of 'this'
  //   for each: if the element is in J, then keep it directly.
  //      otherwie compute the lcm's.
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      Bag *c;
      if (J.search(a.monom().raw(), c))
        {
          new_elems.push_back(new Bag(a));
        }
      else
        for (Bag& b : J)
          {
            Bag *new_elem = new Bag(a.basis_elem());
            varpower::lcm(a.monom().raw(),
                          b.monom().raw(),
                          new_elem->monom());
            new_elems.push_back(new_elem);
          }
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  GC_reachable_here(&J);
  return result;
}

MonomialIdeal *MonomialIdeal::intersect(const int *m) const
// Compute (this : m), where m is a varpower monomial.
{
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      Bag *b = new Bag(a.basis_elem());
      varpower::lcm(a.monom().raw(), m, b->monom());
      new_elems.push_back(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::operator*(const MonomialIdeal &J) const
{
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    for (Bag& b : J)
      {
        Bag *c = new Bag(a.basis_elem());
        varpower::mult(a.monom().raw(),
                       b.monom().raw(),
                       c->monom());
        new_elems.push_back(c);
      }
         
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  GC_reachable_here(&J);
  return result;
}

MonomialIdeal *MonomialIdeal::operator+(const MonomialIdeal &J) const
{
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      new_elems.push_back(new Bag(a));
    }
  for (Bag& a : J)
    {
      new_elems.push_back(new Bag(a));
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  GC_reachable_here(&J);
  return result;
}

MonomialIdeal *MonomialIdeal::operator-(const MonomialIdeal &J) const
// Create the monomial ideal consisting of those elements of 'this'
// that are not in 'J'.  The baggage is left the same.
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  for (Bag& a : *this)
    {
      Bag *c;
      if (!J.search(a.monom().raw(), c))
        {
          result->insert_minimal(new Bag(a));
        }
    }
  GC_reachable_here(&J);
  return result;
}

MonomialIdeal *MonomialIdeal::quotient(const int *m) const
// Compute (this : m), where m is a varpower monomial.
{
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      Bag *b = new Bag(a.basis_elem());
      varpower::quotient(a.monom().raw(), m, b->monom());
      new_elems.push_back(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *MonomialIdeal::quotient(const MonomialIdeal &J) const
{
  // std::cout << "--calling quotient -- I = ";
  // dmonideal(const_cast<MonomialIdeal*>(this));
  // std::cout << std::endl << "  -- J = ";
  // dmonideal(const_cast<MonomialIdeal*>(&J));
  // std::cout << std::endl << "  -- I:J = ";
  // debug_check();
  // J.debug_check();
  
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Bag& a : J)
    {
      MonomialIdeal *result1 = quotient(a.monom().raw());
      // result1->debug_check();
      MonomialIdeal *next_result = result->intersect(*result1);
      // next_result->debug_check();
      delete result1;
      delete result;
      result = next_result;
    }
  // dmonideal(result);
  // std::cout << "----" << std::endl;
  GC_reachable_here(&J);
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

  for (Bag& a : *this)
    {
      for (index_varpower j = a.monom().raw(); j.valid(); ++j)
        if (result->array[j.var()] < j.exponent())
          result->array[j.var()] = j.exponent();
    }
  return result;
}

MonomialIdeal *MonomialIdeal::alexander_dual(const M2_arrayint a) const
// a is a vector which is entrywise >= lcm(this).
{
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Bag& b : *this)
    {
      MonomialIdeal *I1 =
          varpower_monideal(get_ring(), a, b.monom().raw());
      MonomialIdeal *next_result = result->intersect(*I1);
      delete I1;
      delete result;
      result = next_result;
    }
  return result;
}

MonomialIdeal *MonomialIdeal::erase(const int *m) const
{
  debug_check();
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      Bag *b = new Bag(a.basis_elem());
      varpower::erase(a.monom().raw(), m, b->monom());
      new_elems.push_back(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  result->debug_check();
  return result;
}


MonomialIdeal *MonomialIdeal::sat(const MonomialIdeal &J) const
{
  // std::cout << "--calling sat -- I = ";
  // dmonideal(const_cast<MonomialIdeal*>(this));
  // std::cout << std::endl << "  -- J = ";
  // dmonideal(const_cast<MonomialIdeal*>(&J));
  // std::cout << std::endl << "  -- sat(I,J) = ";
  // debug_check();
  // J.debug_check();
  
  MonomialIdeal *result = new MonomialIdeal(get_ring());
  Bag *b = new Bag();
  varpower::one(b->monom());
  result->insert(b);
  for (Bag& a : J)
    {
      MonomialIdeal *result1 = erase(a.monom().raw());
      // result1->debug_check();
      MonomialIdeal *next_result = result->intersect(*result1);
      // next_result->debug_check();
      delete result1;
      delete result;
      result = next_result;
    }
  // dmonideal(result);
  // std::cout << "----" << std::endl;
  GC_reachable_here(&J);
  return result;
}

MonomialIdeal *MonomialIdeal::radical() const
{
  // std::cout << "monideal: calling radical on ";
  // dmonideal(const_cast<MonomialIdeal*>(this));
  // std::cout << std::endl << "  -- radical(I) = ";
  
  VECTOR(Bag*) new_elems;
  for (Bag& a : *this)
    {
      Bag *b = new Bag(a.basis_elem());
      varpower::radical(a.monom().raw(), b->monom());
      new_elems.push_back(b);
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  // dmonideal(result);
  // std::cout << "----" << std::endl;
  return result;
}

static void borel1(VECTOR(Bag *) &result, int *m, int loc, int nvars)
{
  if (loc == 0)
    {
      Bag *b = new Bag();
      varpower::from_ntuple(nvars, m, b->monom());
      result.push_back(b);
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
  VECTOR(Bag *) new_elems;
  int *bexp = newarray_atomic(int, get_ring()->n_vars());
  for (Bag& b : *this)
    {
      varpower::to_ntuple(get_ring()->n_vars(), b.monom().raw(), bexp);
      borel1(new_elems, bexp, get_ring()->n_vars() - 1, get_ring()->n_vars());
    }
  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  freemem(bexp);
  return result;
}

bool MonomialIdeal::is_borel() const
{
  int *bexp = newarray_atomic(int, get_ring()->n_vars());
  for (Bag& b : *this)
    {
      Bag *c;
      varpower::to_ntuple(get_ring()->n_vars(), b.monom().raw(), bexp);
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
  if (size() != 1) return false;
  Nmi_node *p = mi->left;
  if (p->var != 0 || p->exp != 0 || p->tag != Nmi_node::leaf) return false;
  return true;
}

int MonomialIdeal::n_pure_powers() const
// Is each variable to some power in the monideal?
{
  int npure = 0;
  int v, e;
  for (Bag& b : *this)
    {
      int *m = b.monom().raw();
      if (varpower::is_pure_power(m, v, e)) npure++;
    }
  return npure;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
