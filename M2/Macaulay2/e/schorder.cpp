#include <algorithm>
#include <iostream>

#include "schorder.hpp"
#include "matrix.hpp"
#include "comb.hpp"
#include "polyring.hpp"
#include "Eschreyer.hpp"
#include "finalize.hpp"

SchreyerOrder *SchreyerOrder::create(const Monoid *M)
{
  SchreyerOrder *S = new SchreyerOrder(M);
  intern_SchreyerOrder(S);
  return S;
}

void SchreyerOrder::remove() { _order.remove(); }
void SchreyerOrder::append(int compare_num0, const int *baseMonom)
{
  int *me = _order.alloc(_nslots);
  *me++ = compare_num0;
  for (int i = 1; i < _nslots; i++) *me++ = *baseMonom++;
  _rank++;
}

SchreyerOrder *SchreyerOrder::create(const Matrix *m)
{
  int i;
  const Ring *R = m->get_ring();
  const SchreyerOrder *S = m->rows()->get_schreyer_order();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      throw exc::engine_error("expected polynomial ring");
    }
  const Monoid *M = P->getMonoid();
  SchreyerOrder *result = new SchreyerOrder(M);
  int rk = m->n_cols();
  if (rk == 0) return result;
  int *base = M->make_one();
  int *tiebreaks = newarray_atomic(int, rk);
  int *ties = newarray_atomic(int, rk);
  for (i = 0; i < rk; i++)
    {
      vec v = (*m)[i];
      if (v == NULL || S == NULL)
        tiebreaks[i] = i;
      else
        tiebreaks[i] = i + rk * S->compare_num(v->comp);
    }
  // Now sort tiebreaks in increasing order.
  std::sort<int *>(tiebreaks, tiebreaks + rk);
  for (i = 0; i < rk; i++) ties[tiebreaks[i] % rk] = i;
  for (i = 0; i < rk; i++)
    {
      vec v = (*m)[i];
      if (v == NULL)
        M->one(base);
      else if (S == NULL)
        M->copy(P->lead_flat_monomial(v->coeff), base);
      else
        {
          int x = v->comp;
          M->mult(P->lead_flat_monomial(v->coeff), S->base_monom(x), base);
        }

      result->append(ties[i], base);
    }

  intern_SchreyerOrder(result);
  M->remove(base);
  freemem(tiebreaks);
  freemem(ties);
  return result;
}

SchreyerOrder *SchreyerOrder::create(const GBMatrix *m)
{
#ifdef DEVELOPMENT
#warning "the logic in SchreyerOrder creation is WRONG!"
#endif
  int i;
  const FreeModule *F = m->get_free_module();
  const Ring *R = F->get_ring();
  const SchreyerOrder *S = F->get_schreyer_order();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  const Monoid *M = P->getMonoid();
  SchreyerOrder *result = new SchreyerOrder(M);
  int rk = INTSIZE(m->elems);
  if (rk == 0) return result;

  int *base = M->make_one();
  int *tiebreaks = newarray_atomic(int, rk);
  int *ties = newarray_atomic(int, rk);
  for (i = 0; i < rk; i++)
    {
      gbvector *v = m->elems[i];
      if (v == NULL || S == NULL)
        tiebreaks[i] = i;
      else
        tiebreaks[i] = i + rk * S->compare_num(v->comp - 1);
    }
  // Now sort tiebreaks in increasing order.
  std::sort<int *>(tiebreaks, tiebreaks + rk);
  for (i = 0; i < rk; i++) ties[tiebreaks[i] % rk] = i;
  for (i = 0; i < rk; i++)
    {
      gbvector *v = m->elems[i];
      if (v == NULL)
        M->one(base);
      else  // if (S == NULL)
        M->copy(v->monom, base);
#ifdef DEVELOPMENT
#warning "Schreyer unencoded case not handled here"
#endif
#if 0
//       else
//      M->mult(v->monom, S->base_monom(i), base);
#endif
      result->append(ties[i], base);
    }

  intern_SchreyerOrder(result);
  M->remove(base);
  freemem(tiebreaks);
  freemem(ties);
  return result;
}

bool SchreyerOrder::is_equal(const SchreyerOrder *G) const
// A schreyer order is never equal to a non-Schreyer order, even
// if the monomials are all ones.
{
  if (G == NULL) return false;
  for (int i = 0; i < rank(); i++)
    {
      if (compare_num(i) != G->compare_num(i)) return false;
      if (M->compare(base_monom(i), G->base_monom(i)) != 0) return false;
    }
  return true;
}

SchreyerOrder *SchreyerOrder::copy() const
{
  SchreyerOrder *result = new SchreyerOrder(M);
  for (int i = 0; i < rank(); i++)
    result->append(compare_num(i), base_monom(i));
  return result;
}

SchreyerOrder *SchreyerOrder::sub_space(int n) const
{
  if (n < 0 || n > rank())
    {
      ERROR("sub schreyer order: index out of bounds");
      return NULL;
    }
  SchreyerOrder *result = new SchreyerOrder(M);
  for (int i = 0; i < n; i++) result->append(compare_num(i), base_monom(i));
  return result;
}

SchreyerOrder *SchreyerOrder::sub_space(M2_arrayint a) const
{
  // Since this is called only from FreeModule::sub_space,
  // the elements of 'a' are all in bounds, and do not need to be checked...
  // BUT, we check anyway...
  SchreyerOrder *result = new SchreyerOrder(M);
  for (unsigned int i = 0; i < a->len; i++)
    if (a->array[i] >= 0 && a->array[i] < rank())
      result->append(compare_num(a->array[i]), base_monom(a->array[i]));
    else
      {
        ERROR("schreyer order subspace: index out of bounds");
        freemem(result);
        return NULL;
      }
  return result;
}

void SchreyerOrder::append_order(const SchreyerOrder *G)
{
  for (int i = 0; i < G->rank(); i++)
    append(G->compare_num(i), G->base_monom(i));
}

SchreyerOrder *SchreyerOrder::direct_sum(const SchreyerOrder *G) const
{
  SchreyerOrder *result = new SchreyerOrder(M);
  result->append_order(this);
  result->append_order(G);
  return result;
}

SchreyerOrder *SchreyerOrder::tensor(const SchreyerOrder *G) const
// tensor product
{
  // Since this is called only from FreeModule::tensor,
  // we assume that 'this', 'G' have the same monoid 'M'.

  SchreyerOrder *result = new SchreyerOrder(M);
  int *base = M->make_one();

  int next = 0;
  for (int i = 0; i < rank(); i++)
    for (int j = 0; j < G->rank(); j++)
      {
        M->mult(base_monom(i), G->base_monom(j), base);
        result->append(next++, base);
      }

  M->remove(base);
  return result;
}

SchreyerOrder *SchreyerOrder::exterior(int pp) const
// p th exterior power
{
  // This routine is only called from FreeModule::exterior.
  // Therefore: p is in the range 0 < p <= rk.
  SchreyerOrder *result = new SchreyerOrder(M);
  int rk = rank();

  assert(pp > 0);
  assert(pp <= rk);
  size_t p = static_cast<size_t>(pp);

  Subset a(p, 0);
  for (size_t i = 0; i < p; i++) a[i] = i;

  int *base = M->make_one();
  int next = 0;
  do
    {
      M->one(base);
      for (size_t r = 0; r < p; r++)
        M->mult(base, base_monom(static_cast<int>(a[r])), base);

      result->append(next++, base);
    }
  while (Subsets::increment(rk, a));

  M->remove(base);
  return result;
}

struct SchreyerOrder_symm
{
  const SchreyerOrder *S;  // original one
  int n;
  const Monoid *M;

  SchreyerOrder *symm1_result;  // what is being computed
  int *symm1_base;              // used in recursion
  int symm1_next;               // used in recursion

  void symm1(int lastn,  // can use lastn..rank()-1 in product
             int pow)    // remaining power to take
  {
    if (pow == 0)
      symm1_result->append(symm1_next++, symm1_base);
    else
      {
        for (int i = lastn; i < S->rank(); i++)
          {
            // increase symm1_base with e_i
            M->mult(symm1_base, S->base_monom(i), symm1_base);

            symm1(i, pow - 1);

            // decrease symm1_base back
            M->divide(symm1_base, S->base_monom(i), symm1_base);
          }
      }
  }

  SchreyerOrder_symm(const SchreyerOrder *S0, int n0)
      : S(S0),
        n(n0),
        M(S0->getMonoid()),
        symm1_result(0),
        symm1_base(0),
        symm1_next(0)
  {
  }

  SchreyerOrder *value()
  {
    if (symm1_result == 0)
      {
        symm1_result = SchreyerOrder::create(M);
        if (n >= 0)
          {
            symm1_base = M->make_one();
            symm1(0, n);
            M->remove(symm1_base);
          }
      }
    return symm1_result;
  }
};

SchreyerOrder *SchreyerOrder::symm(int n) const
// n th symmetric power
{
  SchreyerOrder_symm S(this, n);
  return S.value();
}

void SchreyerOrder::text_out(buffer &o) const
{
  for (int i = 0; i < _rank; i++)
    {
      if (i != 0) o << ' ';
      M->elem_text_out(o, base_monom(i));
      o << '.';
      o << compare_num(i);
    }
}

int SchreyerOrder::schreyer_compare(const int *m,
                                    int m_comp,
                                    const int *n,
                                    int n_comp) const
{
  const int *ms = base_monom(m_comp);
  const int *ns = base_monom(n_comp);
  for (int i = M->monomial_size(); i > 0; --i)
    {
      int cmp = *ms++ + *m++ - *ns++ - *n++;
      if (cmp < 0) return LT;
      if (cmp > 0) return GT;
    }
  int cmp = compare_num(m_comp) - compare_num(n_comp);
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

int SchreyerOrder::schreyer_compare_encoded(const int *m,
                                            int m_comp,
                                            const int *n,
                                            int n_comp) const
{
  int cmp = M->compare(m, n);
  if (cmp != EQ) return cmp;
  cmp = compare_num(m_comp) - compare_num(n_comp);
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
