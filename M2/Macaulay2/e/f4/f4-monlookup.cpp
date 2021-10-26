// (c) 1994-2021 Michael E. Stillman

#include "f4/f4-monlookup.hpp"
#include "buffer.hpp"                // for buffer
#include "engine-exports.h"          // for newline
#include "f4/varpower-monomial.hpp"  // for varpower_word, const_varpower_mo...
#include "mem.hpp"                   // for stash
#include "style.hpp"                 // for INTSIZE
#include "text-io.hpp"               // for emit, emit_line

#include <cassert>                   // for assert
#include <gc/gc_allocator.h>         // for gc_allocator
#include <cstdint>                   // for int32_t
#include <vector>                    // for vector, vector<>::iterator

template <typename Key>
typename F4MonomialLookupTableT<Key>::mi_node *
F4MonomialLookupTableT<Key>::new_mi_node(varpower_word v,
                                         varpower_word e,
                                         mi_node *d)
{
  mi_node *p = reinterpret_cast<mi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = nullptr;
  p->right = nullptr;
  p->header = nullptr;
  p->tag = mi_node::node;
  p->val.down = d;
  return p;
}

template <typename Key>
typename F4MonomialLookupTableT<Key>::mi_node *
F4MonomialLookupTableT<Key>::new_mi_node(varpower_word v,
                                         varpower_word e,
                                         Key k)
{
  mi_node *p = reinterpret_cast<mi_node *>(mi_stash->new_elem());
  p->var = v;
  p->exp = e;
  p->left = nullptr;
  p->right = nullptr;
  p->header = nullptr;
  p->tag = mi_node::leaf;
  p->val.key = k;
  return p;
}

template <typename Key>
void F4MonomialLookupTableT<Key>::delete_mi_node(mi_node *p)
{
  if (p == nullptr) return;
  if (p->right != p->header) delete_mi_node(p->right);
  if (p->tag == mi_node::node)
    {
      if (p->header != p) delete_mi_node(p->down());
    }
  mi_stash->delete_elem(p);
}

template <typename Key>
F4MonomialLookupTableT<Key>::F4MonomialLookupTableT(int nvars, stash *mi_stash0)
{
  count = 0;
  mi_stash = mi_stash0;
  if (mi_stash == nullptr)
    {
      count = 1;
      mi_stash = new stash("mi_node", sizeof(mi_node));
    }

  size_of_exp = nvars;
  exp0 = newarray_atomic_clear(ntuple_word, size_of_exp);
}

template <typename Key>
F4MonomialLookupTableT<Key>::~F4MonomialLookupTableT()
{
  for (typename VECTOR(mi_node *)::const_iterator i = mis.begin();
       i != mis.end();
       i++)
    delete_mi_node(*i);
  if ((count % 2) == 1) delete mi_stash;
}

template <typename Key>
void F4MonomialLookupTableT<Key>::insert1(mi_node *&top,
                                          const_varpower_monomial b,
                                          Key k)
{
  count += 2;
  mi_node **p = &top, *up = nullptr;
  bool one_element = true;

  for (index_varpower_monomial i = b; i.valid();)
    {
      one_element = false;
      varpower_word insert_var = i.var();
      varpower_word insert_exp;

      if (*p == nullptr)
        {
          // make a new header node
          *p = new_mi_node(insert_var, 0, up);
          (*p)->header = (*p)->left = (*p)->right = *p;
        }
      else if ((*p)->var < insert_var)
        {
          // make a new layer
          mi_node *header_node, *zero_node;
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

      mi_node *q = (*p)->right;
      while ((q != q->header) && (q->exp < insert_exp)) q = q->right;
      if (q->exp != insert_exp)
        {
          mi_node *insert_node;

          if (i.valid())
            {
              insert_node = new_mi_node(
                  insert_var, insert_exp, static_cast<mi_node *>(nullptr));
              q->insert_to_left(insert_node);
              q = insert_node;
            }
          else
            {
              insert_node = new_mi_node(insert_var, insert_exp, k);
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
      top = new_mi_node(0, 0, static_cast<mi_node *>(nullptr));
      mi_node *leaf_node = new_mi_node(0, 0, k);
      top->left = top->right = leaf_node;
      top->header = leaf_node->header = leaf_node->left = leaf_node->right =
          top;
    }
}

template <typename Key>
bool F4MonomialLookupTableT<Key>::find_one_divisor1(mi_node *mi,
                                                    const_ntuple_monomial exp,
                                                    Key &result_k) const
// mi is the top: where to start looking
{
  if (mi == nullptr) return false;

  mi_node *p = mi;

  for (;;)
    {
      p = p->right;

      if (p == p->header)
        {
          if ((p = p->down()) == nullptr) return false;
          continue;
        }

      if (p->exp > exp[p->var])
        {
          if ((p = p->header->down()) == nullptr) return false;
          continue;
        }

      if (p->tag == mi_node::leaf)
        {
          result_k = p->key();
          return true;
        }

      p = p->down();
    }
}

template <typename Key>
void F4MonomialLookupTableT<Key>::find_all_divisors1(mi_node *mi,
                                                     const_ntuple_monomial exp,
                                                     VECTOR(Key) &
                                                         result_k) const
{
  mi_node *p = mi;

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

      if (p->tag == mi_node::leaf)
        {
          result_k.push_back(p->key());
        }
      else
        p = p->down();
    }
}

template <typename Key>
void F4MonomialLookupTableT<Key>::update_exponent_vector(
    int topvar,
    const_varpower_monomial m)
{
  int nvars = topvar + 1;
  if (*m > 0 && m[1] >= nvars) nvars = static_cast<int>(m[1] + 1);
  if (size_of_exp <= nvars)
    {
      // Increase size of exponent vector
      freemem(exp0);
      if (nvars > 2 * size_of_exp)
        size_of_exp = nvars;
      else
        size_of_exp *= 2;

      exp0 = newarray_atomic_clear(ntuple_word, size_of_exp);
    }

  int nparts = static_cast<int>(*m++);
  for (int i = nparts; i > 0; i--, m += 2)
    {
      exp0[*m] = m[1];
    }
}

template <typename Key>
void F4MonomialLookupTableT<Key>::reset_exponent_vector(
    const_varpower_monomial m)
{
  int nparts = static_cast<int>(*m++);
  for (int i = nparts; i > 0; i--, m += 2)
    {
      exp0[*m] = 0;
    }
}

template <typename Key>
bool F4MonomialLookupTableT<Key>::find_one_divisor_vp(long comp,
                                                      const_varpower_monomial m,
                                                      Key &result_k) const
{
  if (comp >= mis.size()) return false;
  mi_node *mi = mis[comp];
  if (mi == nullptr) return false;

  F4MonomialLookupTableT *me = const_cast<F4MonomialLookupTableT *>(this);
  me->update_exponent_vector(static_cast<int>(mi->var), m);
  bool result = find_one_divisor1(mi, exp0, result_k);
  me->reset_exponent_vector(m);
  return result;
}

template <typename Key>
void F4MonomialLookupTableT<Key>::find_all_divisors_vp(
    long comp,
    const_varpower_monomial m,
    VECTOR(Key) & result_k) const
{
  if (comp >= mis.size()) return;
  mi_node *mi = mis[comp];
  if (mi == nullptr) return;

  F4MonomialLookupTableT *me = const_cast<F4MonomialLookupTableT *>(this);
  me->update_exponent_vector(static_cast<int>(mi->var), m);
  find_all_divisors1(mi, exp0, result_k);
  me->reset_exponent_vector(m);
}

template <typename Key>
bool F4MonomialLookupTableT<Key>::find_one_divisor_packed(
    const MonomialInfo *M,
    const_packed_monomial m,
    Key &result_k) const
// mi is the top: where to start looking
{
  long comp = M->get_component(m);
  if (comp >= mis.size()) return false;
  mi_node *mi = mis[comp];
  if (mi == nullptr) return false;
  M->to_exponent_vector(m, exp0, comp);
  return find_one_divisor1(mi, exp0, result_k);
}

template <typename Key>
void F4MonomialLookupTableT<Key>::find_all_divisors_packed(
    const MonomialInfo *M,
    const_packed_monomial m,
    VECTOR(Key) & result_k) const
{
  long comp = M->get_component(m);
  if (comp >= mis.size()) return;
  mi_node *mi = mis[comp];
  if (mi == nullptr) return;
  M->to_exponent_vector(m, exp0, comp);
  find_all_divisors1(mi, exp0, result_k);
}

template <typename Key>
void F4MonomialLookupTableT<Key>::insert_minimal_vp(long comp,
                                                    const_varpower_monomial m,
                                                    Key k)
{
  if (comp >= mis.size())
    {
      for (long j = comp - mis.size(); j >= 0; j--) mis.push_back(nullptr);
    }
  insert1(mis[comp], m, k);
}

template <typename Key>
bool F4MonomialLookupTableT<Key>::insert_vp(long comp,
                                            const_varpower_monomial m,
                                            Key &k)
// Insert the monomial 'm' with key 'k', if it
// is not already in the monomial ideal.  Return whether the
// monomial was actually inserted.
{
  if (find_one_divisor_vp(comp, m, k)) return false;
  insert_minimal_vp(comp, m, k);
  return true;
}

template <typename Key>
typename F4MonomialLookupTableT<Key>::mi_node *
F4MonomialLookupTableT<Key>::next(mi_node *p) const
{
  while (p != nullptr)
    {
      p = p->left;
      if (p->tag == mi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return nullptr;
}

template <typename Key>
typename F4MonomialLookupTableT<Key>::mi_node *
F4MonomialLookupTableT<Key>::prev(mi_node *p) const
{
  while (p != nullptr)
    {
      p = p->right;
      if (p->tag == mi_node::leaf)
        return p;
      else
        p = p->down();
    }
  return nullptr;
}

static int nlists = 0;
static int nleaves = 0;
static int nnodes = 0;
static int ndepth = 0;

template <typename Key>
void F4MonomialLookupTableT<Key>::do_node(mi_node *p,
                                          int indent,
                                          int disp) const
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
  if (p->tag == mi_node::leaf)
    {
      nleaves++;
      if (disp) o << ' ' << p->key();
    }
  else if (p == p->header)
    nlists++;
  else
    nnodes++;
  emit_line(o.str());
}

template <typename Key>
void F4MonomialLookupTableT<Key>::do_tree(mi_node *p,
                                          int depth,
                                          int indent,
                                          int disp) const
{
  if (depth > ndepth) ndepth = depth;
  do_node(p, indent, disp);
  mi_node *q = p->right;
  while (q != p)
    {
      do_node(q, indent, disp);
      if (q->tag != mi_node::leaf)
        do_tree(q->down(), depth + 1, indent + 2, disp);
      q = q->right;
    }
}

template <typename Key>
void F4MonomialLookupTableT<Key>::debug_out(int disp) const
// Display F4MonomialLookupTableT in tree-like form, collect statistics
{
  nlists = 0;
  nnodes = 0;
  nleaves = 0;
  ndepth = 0;
  for (typename VECTOR(mi_node *)::const_iterator i = mis.begin();
       i != mis.end();
       i++)
    if (*i != nullptr) do_tree(*i, 0, 0, disp);
  buffer o;
  o << "list nodes     = " << nlists << newline;
  o << "internal nodes = " << nnodes << newline;
  o << "monomials      = " << nleaves << newline;
  o << "max depth      = " << ndepth << newline;
  emit(o.str());
}

template <typename Key>
int F4MonomialLookupTableT<Key>::debug_check(mi_node *const p,
                                             const mi_node *const up) const
// Returns the number of leaves at tree with root p.
// Make sure that the list header is constructed ok, that the
// left/right pointers are ok on this level, that the
// var, exp, values in this train are correct.
// Then loop through, checking each node (recursively) and each leaf
{
  mi_node *q;
  // First check the node 'p' itself
  assert(p != nullptr);
  assert(p->var >= 0);
  if (up != nullptr) assert(p->var < up->var);
  assert(p->header == p);
  assert(p->tag == mi_node::node);
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
    if (q->tag == mi_node::node)
      c += debug_check(q->down(), q);
    else
      c++;
  return c;
}

template <typename Key>
void F4MonomialLookupTableT<Key>::debug_check() const
{
  int nfound = 0;
  for (typename VECTOR(mi_node *)::const_iterator i = mis.begin();
       i != mis.end();
       i++)
    {
      if (*i != nullptr) nfound += debug_check(*i, nullptr);
    }
  assert(count / 2 == nfound);
}

template <typename Key>
void F4MonomialLookupTableT<Key>::text_out(buffer &o) const
{
  o << "F4MonomialLookupTableT (";
  o << count / 2 << " entries)\n";
  int a = 0;
  for (typename VECTOR(mi_node *)::const_iterator i = mis.begin();
       i != mis.end();
       i++)
    {
      for (mi_node *p = *i; p != nullptr; p = next(p))
        {
          if ((++a) % 15 == 0) o << newline;
          o << p->key() << "  ";
        }
    }
}

void minimalize_varpower_monomials(const VECTOR(varpower_monomial) & elems,
                                   VECTOR(int) & result_minimals,
                                   stash *mi_stash)
{
  VECTOR(VECTOR(int) *) bins;
  for (int j = 0; j < elems.size(); j++)
    {
      varpower_word d = varpower_monomials::simple_degree(elems[j]);
      if (d >= bins.size())
        for (int i = INTSIZE(bins); i <= d; i++) bins.push_back(nullptr);
      if (bins[d] == nullptr) bins[d] = new VECTOR(int);
      bins[d]->push_back(j);
    }

  // Now insert these into a lookup table
  F4MonomialLookupTableT<int> M(
      10, mi_stash);  // The 10 is simply a suggested start value
  for (int i = 0; i < bins.size(); i++)
    if (bins[i] != nullptr)
      {
        for (VECTOR(int)::iterator j = bins[i]->begin(); j != bins[i]->end();
             j++)
          {
            int k;
            if (!M.find_one_divisor_vp(0, elems[*j], k))
              {
                M.insert_minimal_vp(0, elems[*j], 0);
                result_minimals.push_back(*j);
              }
          }
        freemem(bins[i]);
      }
}

template class F4MonomialLookupTableT<int32_t>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
