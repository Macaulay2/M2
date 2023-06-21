// Copyright 2003  Michael E. Stillman

#include "ring.hpp"
#include "text-io.hpp"
#include <vector>
#include "matrix.hpp"
#include "geovec.hpp"
#include "ringmap.hpp"
#include "poly.hpp"
//  Notes: ring_elem's are treated as immutable objects: they are not changed,
//  and
// the fact that one cannot change is used throughout.

vec Ring::new_vec() const { return new vecterm; }
void Ring::remove_vec_node(vec n) const
{
  freemem(n);
}

vec Ring::make_vec(int r, ring_elem a) const
{
  if (is_zero(a)) return nullptr;
  vec result = new_vec();
  result->next = nullptr;
  result->comp = r;
  result->coeff = a;
  return result;
}

vec Ring::make_vec_from_array(int len, Nterm **array) const
{
  vec result = nullptr;
  for (int i = 0; i < len; i++)
    {
      if (array[i] != nullptr)
        {
          vec v = make_vec(i, array[i]);
          v->next = result;
          result = v;
        }
    }
  return result;
}

vec Ring::e_sub_i(int i) const
{
  ring_elem a = from_long(1);
  return make_vec(i, a);
}

vec Ring::copy_vec(const vecterm *v) const
{
  vecterm head;
  vec result = &head;
  for (const vecterm *p = v; p != nullptr; p = p->next)
    {
      vec w = new_vec();
      result->next = w;
      result = w;
      w->comp = p->comp;
      w->coeff = p->coeff;  // copy is not done
    }
  result->next = nullptr;
  return head.next;
}

void Ring::remove_vec(vec v) const
{
  while (v != nullptr)
    {
      vec tmp = v;
      v = v->next;
      remove_vec_node(tmp);
    }
}

///////////////////////////////////////
// Routines which do not modify vecs //
///////////////////////////////////////

bool Ring::is_equal(const vecterm *a, const vecterm *b) const
{
  for (;; a = a->next, b = b->next)
    {
      if (a == nullptr)
        {
          if (b == nullptr) return true;
          return false;
        }
      if (b == nullptr) return false;
      if (a->comp != b->comp) return false;
      if (!this->is_equal(a->coeff, b->coeff)) return false;
    }
}

int Ring::compare_vecs(vec v, vec w) const
{
  for (;; v = v->next, w = w->next)
    {
      if (v == nullptr)
        {
          if (w == nullptr) return 0;
          return -1;
        }
      if (w == nullptr) return 1;
      int cmp = v->comp - w->comp;
      if (cmp > 0) return cmp;
      if (cmp < 0) return cmp;
      cmp = this->compare_elems(v->coeff, w->coeff);
      if (cmp > 0) return cmp;
      if (cmp < 0) return cmp;
    }
}

bool Ring::get_entry(const vecterm *v, int r, ring_elem &result) const
{
  for (const vecterm *p = v; p != nullptr; p = p->next)
    if (p->comp < r)
      break;
    else if (p->comp == r)
      {
        result = p->coeff;
        return true;
      }
  return false;
}

ring_elem Ring::get_entry(vec v, int r) const
{
  while (v != nullptr)
    {
      if (v->comp == r) return v->coeff;
      if (v->comp < r) return from_long(0);
      v = v->next;
    }
  return from_long(0);
}

int Ring::n_nonzero_terms(const vecterm *v) const
{
  int result = 0;
  for (; v != nullptr; v = v->next) result++;
  return result;
}

vec Ring::negate_vec(vec v) const
{
  vecterm result;
  vecterm *b = &result;
  for (vecterm *a = v; a != nullptr; a = a->next)
    {
      b->next = make_vec(a->comp, negate(a->coeff));
      b = b->next;
    }
  b->next = nullptr;
  return result.next;
}

vec Ring::add_vec(vec v, vec w) const
{
  vec f = copy_vec(v);
  vec g = copy_vec(w);
  add_vec_to(f, g);
  return f;
}

vec Ring::subtract_vec(vec v, vec w) const
{
  vec f = copy_vec(v);
  vec g = negate_vec(w);
  add_vec_to(f, g);
  return f;
}

vec Ring::mult_vec(int n, vec v) const
{
  ring_elem f = from_long(n);
  vec result = mult_vec(f, v);
  return result;
}

vec Ring::mult_vec(const ring_elem f, const vec w) const
{
  if (is_zero(f)) return nullptr;
  vecterm head;
  vec result = &head;
  for (vec v = w; v != nullptr; v = v->next)
    {
      ring_elem a = mult(f, v->coeff);
      if (!is_zero(a))
        {
          vec t = make_vec(v->comp, a);
          result->next = t;
          result = t;
        }
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::rightmult_vec(const vec w, const ring_elem f) const
{
  if (is_zero(f)) return nullptr;
  vecterm head;
  vec result = &head;
  for (vec v = w; v != nullptr; v = v->next)
    {
      ring_elem a = mult(v->coeff, f);
      if (!is_zero(a))
        {
          vec t = make_vec(v->comp, a);
          result->next = t;
          result = t;
        }
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::sub_vector(const vecterm *v, M2_arrayint r) const
{
  if (v == nullptr) return nullptr;
  // Largest component which occurs in v occurs first.
  VECTOR(int) trans(v->comp + 1);
  for (int i = 0; i < v->comp; i++) trans.push_back(-1);

  for (unsigned j = 0; j < r->len; j++)
    if (r->array[j] >= 0 && r->array[j] <= v->comp) trans[r->array[j]] = j;

  vecterm head;
  vecterm *result = &head;
  for (; v != nullptr; v = v->next)
    if (trans[v->comp] != -1)
      {
        result->next = new_vec();
        result = result->next;
        result->next = nullptr;
        result->coeff = v->coeff;
        result->comp = trans[v->comp];
      }
  result->next = nullptr;
  result = head.next;

  vec_sort(result);
  return result;
}

vec Ring::component_shift(int n, vec v) const
{
  vecterm head;
  vec result = &head;
  for (const vecterm *p = v; p != nullptr; p = p->next)
    {
      vec w = new_vec();
      result->next = w;
      result = w;
      w->comp = p->comp + n;
      w->coeff = p->coeff;  // copy is not done
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::tensor_shift(int n, int m, vec v) const
{
  vecterm head;
  vecterm *result = &head;

  for (; v != nullptr; v = v->next)
    {
      vec w = new_vec();
      result->next = w;
      result = w;
      w->comp = n * v->comp + m;
      w->coeff = v->coeff;  // copy is not done
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::tensor(const FreeModule *F, vec v, const FreeModule *G, vec w) const
{
  vecHeap H(F);
  for (; v != nullptr; v = v->next)
    {
      vec w1 = component_shift(v->comp * G->rank(), w);
      mult_vec_to(w1, v->coeff, false);
      H.add(w1);
    }
  return H.value();
}

void Ring::vec_text_out(buffer &o,
                        const vecterm *v,
                        bool p_one,
                        bool p_plus,
                        bool p_parens) const
{
  if (v == nullptr)
    {
      o << "0";
      return;
    }

  p_one = false;
  for (const vecterm *t = v; t != nullptr; t = t->next)
    {
      this->elem_text_out(o, t->coeff, p_one, p_plus, p_parens);
      o << "<" << t->comp << ">";
      p_plus = true;
    }
}

vec Ring::vec_eval(const RingMap *map, const FreeModule *F, const vec v) const
// v is a vector over 'this'
{
  const Ring *targetRing = map->get_ring();

  vecterm head;
  vec result = &head;

  for (vec t = v; t != nullptr; t = t->next)
    {
      ring_elem a = eval(map, t->coeff, 0);  // a is now in the target ring
      if (!targetRing->is_zero(a))
        {
          result->next = targetRing->make_vec(t->comp, a);
          result = result->next;
        }
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::vec_zeroize_tiny(gmp_RR epsilon, const vec v) const
{
  vecterm head;
  vec result = &head;
  for (const vecterm *p = v; p != nullptr; p = p->next)
    {
      ring_elem a = zeroize_tiny(epsilon, p->coeff);
      if (!is_zero(a))
        {
          vec w = new_vec();
          result->next = w;
          result = w;
          w->comp = p->comp;
          w->coeff = a;
        }
    }
  result->next = nullptr;
  return head.next;
}

void Ring::vec_increase_maxnorm(gmp_RRmutable norm, const vec v) const
// If any real number appearing in f has larger absolute value than norm,
// replace norm.
// Default for rings not over RRR or CCC is to do nothing.
{
  for (const vecterm *p = v; p != nullptr; p = p->next)
    increase_maxnorm(norm, p->coeff);
}
///////////////////////////////////////
// Routines which modify a vec ////////
///////////////////////////////////////

void Ring::mult_vec_to(vec &v, const ring_elem r, bool opposite_mult) const
{
  if (this->is_zero(r))
    {
      remove_vec(v);
      v = nullptr;
      return;
    }
  vecterm head;
  head.next = v;
  vec p = &head;
  while (p->next != nullptr)
    {
      // old version: this->mult_to(p->next->coeff, a);
      ring_elem c;
      if (opposite_mult)
        c = this->mult(p->next->coeff, r);
      else
        c = this->mult(r, p->next->coeff);
      p->next->coeff = c;
      if (this->is_zero(p->next->coeff))
        {
          vec tmp = p->next;
          p->next = tmp->next;
          remove_vec_node(tmp);
        }
      else
        p = p->next;
    }
  v = head.next;
}

void Ring::mult_row(vec &v, const ring_elem r, int i, bool opposite_mult) const
{
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != nullptr; p = p->next)
    if (p->next->comp < i)
      break;
    else if (p->next->comp == i)
      {
        ring_elem c;
        if (opposite_mult)
          c = mult(p->next->coeff, r);
        else
          c = mult(r, p->next->coeff);
        p->next->coeff = c;
        if (this->is_zero(p->next->coeff))
          {
            vec tmp = p->next;
            p->next = tmp->next;
            remove_vec_node(tmp);
          }
        break;
      }
  v = head.next;
}

vec Ring::mult_vec_matrix(const Matrix *m, vec v, bool opposite_mult) const
{
  // Multiply m * v, using left or right mult for each scalar mult.

  // Each loop below should read
  // result = 0
  // for each non-zero term f e[component] of the vector v
  //    result += f M[v]
  vec result = nullptr;
  for (; v != nullptr; v = v->next)
    {
      vec w = this->copy_vec(m->elem(v->comp));
      mult_vec_to(w, v->coeff, !opposite_mult);
      this->add_vec_to(result, w);
    }
  return result;
}

void Ring::divide_vec_to(vec &v, const ring_elem a) const
{
  if (this->is_zero(a))
    {
      remove_vec(v);
      v = nullptr;
    }
  vecterm head;
  head.next = v;
  vec p = &head;
  while (p->next != nullptr)
    {
      // old version: this->mult_to(p->next->coeff, a);
      ring_elem c =
          this->divide(p->next->coeff, a);  // exact or quotient?? MES MES
      p->next->coeff = c;
      if (this->is_zero(p->next->coeff))
        {
          vec tmp = p->next;
          p->next = tmp->next;
          remove_vec_node(tmp);
        }
      else
        p = p->next;
    }
  v = head.next;
}

void Ring::divide_row(vec &v, int r, const ring_elem a) const
{
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != nullptr; p = p->next)
    if (p->next->comp < r)
      break;
    else if (p->next->comp == r)
      {
        ring_elem c =
            this->divide(p->next->coeff, a);  // exact or quotient?? MES MES
        p->next->coeff = c;
        if (this->is_zero(p->next->coeff))
          {
            vec tmp = p->next;
            p->next = tmp->next;
            remove_vec_node(tmp);
          }
        break;
      }
}

void Ring::negate_vec_to(vec &v) const
{
  vec w = v;
  while (w != nullptr)
    {
      negate_to(w->coeff);
      w = w->next;
    }
}

void Ring::subtract_vec_to(vec &v, vec &w) const
{
  negate_vec_to(w);
  add_vec_to(v, w);
}

void Ring::add_vec_to(vec &v, vec &w) const
{
  if (w == nullptr) return;
  if (v == nullptr)
    {
      v = w;
      w = nullptr;
      return;
    }
  vecterm head;
  vec result = &head;
  while (true)
    if (v->comp < w->comp)
      {
        result->next = w;
        result = result->next;
        w = w->next;
        if (w == nullptr)
          {
            result->next = v;
            v = head.next;
            return;
          }
      }
    else if (v->comp > w->comp)
      {
        result->next = v;
        result = result->next;
        v = v->next;
        if (v == nullptr)
          {
            result->next = w;
            v = head.next;
            w = nullptr;
            return;
          }
      }
    else
      {
        vec tmv = v;
        vec tmw = w;
        v = v->next;
        w = w->next;
        tmv->coeff = this->add(tmv->coeff, tmw->coeff);
        if (this->is_zero(tmv->coeff))
          {
            remove_vec_node(tmv);
          }
        else
          {
            result->next = tmv;
            result = result->next;
          }
        remove_vec_node(tmw);
        if (w == nullptr)
          {
            result->next = v;
            v = head.next;
            return;
          }
        if (v == nullptr)
          {
            result->next = w;
            v = head.next;
            w = nullptr;
            return;
          }
      }
}

ring_elem Ring::dot_product(const vecterm *v, const vecterm *w) const
{
  ring_elem result = this->from_long(0);
  while (true)
    {
      if (v == nullptr) return result;
      if (w == nullptr) return result;
      if (v->comp > w->comp)
        v = v->next;
      else if (v->comp < w->comp)
        w = w->next;
      else
        {
          ring_elem a = this->mult(v->coeff, w->coeff);
          result = this->add(result, a);
          v = v->next;
          w = w->next;
        }
    }
}

void Ring::set_entry(vec &v, int r, ring_elem a) const
{
  vec p;
  bool iszero = this->is_zero(a);
  vecterm head;
  head.next = v;
  for (p = &head; p->next != nullptr; p = p->next)
    if (p->next->comp <= r) break;

  if (p->next == nullptr || p->next->comp < r)
    {
      if (iszero) return;
      vec w = new_vec();
      w->next = p->next;
      w->comp = r;
      w->coeff = a;
      p->next = w;
    }
  else if (p->next->comp == r)
    {
      if (iszero)
        {
          // delete node
          vec tmp = p->next;
          p->next = tmp->next;
          remove_vec_node(tmp);
        }
      else
        p->next->coeff = a;
    }
  v = head.next;
}

void Ring::vec_sort(vecterm *&f) const
{
  // Internal routine to place the elements back in order after
  // an operation such as subvector.
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.

  if (f == nullptr || f->next == nullptr) return;
  vecterm *f1 = nullptr;
  vecterm *f2 = nullptr;
  while (f != nullptr)
    {
      vecterm *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == nullptr) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }

  vec_sort(f1);
  vec_sort(f2);
  add_vec_to(f1, f2);
  f = f1;
}

vec Ring::vec_lead_term(int nparts, const FreeModule *F, vec v) const
{
  // May be over-ridden by subclasses.  In particular, by polynomial classes.
  if (v == nullptr) return nullptr;
  return make_vec(v->comp, v->coeff);
}

vec Ring::vec_diff(vec v, int rankFw, vec w, int use_coeff) const
// rankFw is the rank of the free module corresponding to w.
{
  vec result = nullptr;
  for (; v != nullptr; v = v->next)
    for (vecterm *p = w; p != nullptr; p = p->next)
      {
        ring_elem a = diff(v->coeff, p->coeff, use_coeff);
        if (is_zero(a))
          {
            remove(a);
            continue;
          }
        vecterm *t = new_vec();
        t->comp = rankFw * v->comp + p->comp;
        t->coeff = a;
        t->next = result;
        result = t;
      }
  vec_sort(result);
  return result;
}

int Ring::vec_in_subring(int nslots, const vec v) const
{
  const PolynomialRing *PR = cast_to_PolynomialRing();
  if (PR == nullptr || v == nullptr) return true;
  const Monoid *M = PR->getMonoid();
  for (vec w = v; w != nullptr; w = w->next)
    if (!M->in_subring(nslots, PR->lead_flat_monomial(w->coeff))) return false;
  return true;
}

void Ring::vec_degree_of_var(int n, const vec v, int &lo, int &hi) const
{
  if (v == nullptr)
    {
      ERROR("attempting to find degree of zero vector");
      return;
    }
  degree_of_var(n, v->coeff, lo, hi);
  for (vec w = v->next; w != nullptr; w = w->next)
    {
      int lo1, hi1;
      degree_of_var(n, w->coeff, lo1, hi1);
      if (lo1 < lo) lo = lo1;
      if (hi1 > hi) hi = hi1;
    }
}

vec Ring::vec_divide_by_var(int n, int d, const vec v) const
{
  vecterm head;
  vecterm *result = &head;
  for (vec w = v; w != nullptr; w = w->next)
    {
      ring_elem a = divide_by_var(n, d, w->coeff);
      if (!is_zero(a))
        {
          vec t = make_vec(w->comp, a);
          result->next = t;
          result = t;
        }
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::vec_divide_by_expvector(const_exponents exp, const vec v) const
{
  vecterm head;
  vecterm *result = &head;
  for (vec w = v; w != nullptr; w = w->next)
    {
      ring_elem a = divide_by_expvector(exp, w->coeff);
      if (!is_zero(a))
        {
          vec t = make_vec(w->comp, a);
          result->next = t;
          result = t;
        }
    }
  result->next = nullptr;
  return head.next;
}

//////////////////////////////////////////////
//  Homogeneity and the grading //////////////
//////////////////////////////////////////////

bool Ring::vec_multi_degree(const FreeModule *F, const vec f, monomial degf) const
// Returns true if the element is homogeneous
// Sets degf to be the highest degree found (actually, the join of the
//   degree vectors occurring).
{
  monomial degv;
  degree_monoid()->one(degf);
  if (f == nullptr) return true;
  bool result = multi_degree(f->coeff, degf);
  degree_monoid()->mult(degf, F->degree(f->comp), degf);
  degv = degree_monoid()->make_one();

  for (vec v = f->next; v != nullptr; v = v->next)
    {
      bool ishom = multi_degree(v->coeff, degv);
      result = result && ishom;
      degree_monoid()->mult(degv, F->degree(v->comp), degv);

      if (0 != degree_monoid()->compare(degf, degv))
        {
          result = false;
          degree_monoid()->lcm(degf, degv, degf);
        }
    }
  degree_monoid()->remove(degv);
  return result;
}

bool Ring::vec_is_homogeneous(const FreeModule *F, const vec f) const
{
  if (!this->is_graded()) return false;
  if (f == nullptr) return true;
  monomial d = degree_monoid()->make_one();
  monomial e = degree_monoid()->make_one();
  bool result = multi_degree(f->coeff, d);
  if (result)
    {
      degree_monoid()->mult(d, F->degree(f->comp), d);
      for (vecterm *t = f->next; (t != nullptr) && result; t = t->next)
        {
          bool ishom = multi_degree(t->coeff, e);
          result = result && ishom;
          if (result)
            {
              degree_monoid()->mult(e, F->degree(t->comp), e);
              if (0 != degree_monoid()->compare(d, e)) result = false;
            }
        }
    }
  degree_monoid()->remove(d);
  degree_monoid()->remove(e);
  return result;
}

void Ring::vec_degree_weights(const FreeModule *F,
                              const vec f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const
{
  vecterm *t = f;
  if (t == nullptr)
    {
      lo = hi = 0;
      return;
    }
  degree_weights(t->coeff, wts, lo, hi);
  lo += F->primary_degree(t->comp);
  hi += F->primary_degree(t->comp);
  for (t = t->next; t != nullptr; t = t->next)
    {
      int lo1, hi1;
      degree_weights(t->coeff, wts, lo1, hi1);
      lo1 += F->primary_degree(t->comp);
      hi1 += F->primary_degree(t->comp);
      if (hi1 > hi) hi = hi1;
      if (lo1 < lo) lo = lo1;
    }
}

vec Ring::vec_homogenize(const FreeModule *F,
                         const vec f,
                         int v,
                         int d,
                         M2_arrayint wts) const
// Any terms which can't be homogenized are silently set to 0
{
  vecterm head;
  vecterm *result = &head;
  assert(wts->array[v] != 0);
  // If an error occurs, then return 0, and set ERROR

  for (vec w = f; w != nullptr; w = w->next)
    {
      int e = F->primary_degree(w->comp);
      ring_elem a = homogenize(w->coeff, v, d - e, wts);
      if (!is_zero(a))
        {
          result->next = make_vec(w->comp, a);
          result = result->next;
        }
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::vec_homogenize(const FreeModule *F,
                         const vec f,
                         int v,
                         M2_arrayint wts) const
{
  vecterm *result = nullptr;
  if (f == nullptr) return result;
  int lo, hi;
  vec_degree_weights(F, f, wts, lo, hi);
  assert(wts->array[v] != 0);
  int d = (wts->array[v] > 0 ? hi : lo);
  return vec_homogenize(F, f, v, d, wts);
}

//////////////////////////////////////////////
//  Divisibility checks               ////////
//                                    ////////
//////////////////////////////////////////////

bool static check_nterm_multiples(const PolyRing *R,
                                  ring_elem f1,  // in R
                                  ring_elem g1,  // in R
                                  ring_elem c,   // in flat coeffs of R
                                  ring_elem d)   // in flat coeffs of R
{
  Nterm *f;
  Nterm *g;
  const Monoid *M = R->getMonoid();
  const Ring *K = R->getCoefficients();
  for (f = f1, g = g1; f != nullptr && g != nullptr; f = f->next, g = g->next)
    {
      if (M->compare(f->monom, g->monom) != 0) return false;
      ring_elem c1 = K->mult(c, g->coeff);
      ring_elem d1 = K->mult(d, f->coeff);
      int isequal = K->is_equal(c1, d1);
      if (!isequal) return false;
    }
  if (f == nullptr && g == nullptr) return true;
  return false;
}

bool Ring::vec_is_scalar_multiple(vec f, vec g) const
// is df = cg, some scalars c,d?
// These scalars are over the very bottom base field/ZZ.
{
  if (f == nullptr) return true;
  if (g == nullptr) return true;
  const PolynomialRing *PR = cast_to_PolynomialRing();
  if (PR == nullptr) return true;
  const PolyRing *PR1 = PR->getNumeratorRing();
#ifdef DEVELOPMENT
#warning "use numerator only"
#endif
  if (f->comp != g->comp) return false;
  Nterm *f1 = f->coeff;
  Nterm *g1 = g->coeff;
  ring_elem c = f1->coeff;
  ring_elem d = g1->coeff;
  vec p, q;
  for (p = f, q = g; p != nullptr && q != nullptr; p = p->next, q = q->next)
    {
      if (p->comp != q->comp) return 0;
      if (!check_nterm_multiples(PR1, p->coeff, q->coeff, c, d)) return false;
    }
  if (q == nullptr && p == nullptr) return true;
  return false;
}

vec Ring::vec_remove_monomial_factors(vec f, bool make_squarefree_only) const
{
  const PolynomialRing *PR = cast_to_PolynomialRing();
  if (PR == nullptr) return copy_vec(f);
  if (f == nullptr) return nullptr;

  exponents_t exp = newarray_atomic(int, PR->n_vars());

  Nterm *t = f->coeff;
  PR->getMonoid()->to_expvector(t->monom, exp);  // Get the process started

  for (vec a = f; a != nullptr; a = a->next) monomial_divisor(a->coeff, exp);

  if (make_squarefree_only)
    // Now divide each term by exp[i]-1, if exp[i] >= 2
    for (int i = 0; i < PR->n_vars(); i++)
      if (exp[i] >= 1) exp[i]--;

  vec result = vec_divide_by_expvector(exp, f);

  freemem(exp);
  return result;
}

ring_elem Ring::vec_content(vec f) const
{
  if (f == nullptr) return zero();
  ring_elem c = content(f->coeff);
  for (vec t = f->next; t != nullptr; t = t->next) lower_content(c, t->coeff);
  return c;
}

vec Ring::vec_divide_by_given_content(vec f, ring_elem c) const
{
  if (f == nullptr) return nullptr;
  vecterm head;
  vec result = &head;
  for (const vecterm *p = f; p != nullptr; p = p->next)
    {
      vec w = new_vec();
      result->next = w;
      result = w;
      w->comp = p->comp;
      w->coeff = divide_by_given_content(p->coeff, c);
    }
  result->next = nullptr;
  return head.next;
}

vec Ring::vec_divide_by_content(vec f) const
{
  if (f == nullptr) return nullptr;
  ring_elem c = vec_content(f);
  return vec_divide_by_given_content(f, c);
}

ring_elem Ring::vec_split_off_content(vec f, vec &result) const
{
  ring_elem c = vec_content(f);
  if (f == nullptr)
    result = nullptr;
  else
    result = vec_divide_by_given_content(f, c);
  return c;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
