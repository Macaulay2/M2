// Copyright 1996 Michael E. Stillman

#include "res-a1-poly.hpp"
#include "text-io.hpp"
#include "polyring.hpp"
#include "freemod.hpp"
#include "geovec.hpp"

res_poly::res_poly(PolynomialRing *RR)
    : R(RR), M(R->getMonoid()), K(R->getCoefficientRing())
{
  element_size = sizeof(resterm *) + sizeof(res_pair *) + sizeof(ring_elem) +
                 sizeof(int) * M->monomial_size();
  resterm_stash = new stash("resterm", element_size);
}

res_poly::~res_poly() { delete resterm_stash; }
inline int res_poly::compare(const resterm *a, const resterm *b) const
{
  int cmp = M->compare(a->monom, b->monom);
  if (cmp != 0) return cmp;
  cmp = a->comp->compare_num - b->comp->compare_num;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  return 0;
}

resterm *res_poly::new_term() const
{
  resterm *result = reinterpret_cast<resterm *>(resterm_stash->new_elem());
  result->next = NULL;
  return result;
}
resterm *res_poly::new_term(ring_elem c, const int *m, res_pair *x) const
// Note: this does NOT copy 'c'
{
  resterm *result = new_term();
  result->coeff = c;
  M->copy(m, result->monom);
  result->comp = x;
  return result;
}

resterm *res_poly::mult_by_monomial(const resterm *f, const int *m) const
{
  resterm head;
  resterm *result = &head;
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = tm->comp;
      result->coeff = K->copy(tm->coeff);
      M->mult(tm->monom, m, result->monom);
    }
  result->next = NULL;
  return head.next;
}

resterm *res_poly::copy(const resterm *f) const
{
  resterm head;
  resterm *result = &head;
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = tm->comp;
      result->coeff = K->copy(tm->coeff);
      M->copy(tm->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}
void res_poly::remove(resterm *&f) const
{
  while (f != NULL)
    {
      resterm *tmp = f;
      f = f->next;
      K->remove(tmp->coeff);
      resterm_stash->delete_elem(tmp);
    }
}

resterm *res_poly::mult_by_term(const resterm *f,
                                ring_elem c,
                                const int *m) const
{
  resterm head;
  resterm *result = &head;
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = tm->comp;
      result->coeff = K->mult(c, tm->coeff);
      M->mult(tm->monom, m, result->monom);
    }
  result->next = NULL;
  return head.next;
}
resterm *res_poly::ring_mult_by_term(const ring_elem f,
                                     ring_elem c,
                                     const int *m,
                                     res_pair *x) const
{
  resterm head;
  resterm *result = &head;
  for (const Nterm *tm = f; tm != NULL; tm = tm->next)
    {
      result->next = new_term();
      result = result->next;
      result->comp = x;
      result->coeff = K->mult(c, tm->coeff);
      M->mult(tm->monom, m, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void res_poly::make_monic(resterm *&f) const
{
  if (f == NULL) return;
  ring_elem c_inv = K->invert(f->coeff);

  for (resterm *tm = f; tm != NULL; tm = tm->next) K->mult_to(tm->coeff, c_inv);

  K->remove(c_inv);
}

void res_poly::add_to(resterm *&f, resterm *&g) const
{
  if (g == NULL) return;
  if (f == NULL)
    {
      f = g;
      g = NULL;
      return;
    }
  resterm head;
  resterm *result = &head;
  while (1) switch (compare(f, g))
      {
        case -1:
          result->next = g;
          result = result->next;
          g = g->next;
          if (g == NULL)
            {
              result->next = f;
              f = head.next;
              return;
            }
          break;
        case 1:
          result->next = f;
          result = result->next;
          f = f->next;
          if (f == NULL)
            {
              result->next = g;
              f = head.next;
              g = NULL;
              return;
            }
          break;
        case 0:
          resterm *tmf = f;
          resterm *tmg = g;
          f = f->next;
          g = g->next;
          K->add_to(tmf->coeff, tmg->coeff);
          if (K->is_zero(tmf->coeff))
            resterm_stash->delete_elem(tmf);
          else
            {
              result->next = tmf;
              result = result->next;
            }
          resterm_stash->delete_elem(tmg);
          if (g == NULL)
            {
              result->next = f;
              f = head.next;
              return;
            }
          if (f == NULL)
            {
              result->next = g;
              f = head.next;
              g = NULL;
              return;
            }
          break;
      }
}

void res_poly::subtract_multiple_to(resterm *&f,
                                    ring_elem c,
                                    const int *m,
                                    const resterm *g) const
// f := f - c * m * g
{
  ring_elem minus_c = K->negate(c);
  resterm *h = mult_by_term(g, minus_c, m);
  add_to(f, h);
  K->remove(minus_c);
}
void res_poly::ring_subtract_multiple_to(resterm *&f,
                                         ring_elem c,
                                         const int *m,
                                         res_pair *x,
                                         const ring_elem g) const
// f := f - c * m * g * x, where g is a ring element
{
  ring_elem minus_c = K->negate(c);
  resterm *h = ring_mult_by_term(g, minus_c, m, x);
  add_to(f, h);
  K->remove(minus_c);
}

int res_poly::n_terms(const resterm *f) const
{
  int result = 0;
  for (; f != NULL; f = f->next) result++;
  return result;
}

void res_poly::elem_text_out(const resterm *f) const
{
  buffer o;
  elem_text_out(o, f);
  emit(o.str());
}
void res_poly::elem_text_out(buffer &o, const resterm *f) const
{
  if (f == NULL)
    {
      o << "0";
      return;
    }

  bool p_one = false;
  bool p_parens = true;
  bool p_plus = false;
  for (const resterm *t = f; t != NULL; t = t->next)
    {
      int isone = M->is_one(t->monom);
      K->elem_text_out(o, t->coeff, p_one, p_parens, p_plus);
      if (!isone) M->elem_text_out(o, t->monom, p_one);
      o << "<" << t->comp->me << ">";
      p_plus = true;
    }
}

vec res_poly::to_vector(const resterm *f,
                        const FreeModule *F,
                        int /*to_minimal*/) const
{
  vecHeap H(F);
  int *mon = M->make_one();
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    {
      //    int x = (to_minimal ? tm->comp->minimal_me : tm->comp->me);
      int x =
          tm->comp
              ->minimal_me;  // MES: Currently used for non-minimal as well...
      M->divide(tm->monom, tm->comp->base_monom, mon);

      ring_elem a = R->make_flat_term(tm->coeff, mon);
      vec tmp = R->make_vec(x, a);
      H.add(tmp);
    }
  M->remove(mon);
  return H.value();
}

resterm *res_poly::from_vector(const VECTOR(res_pair *)& base, const vec v) const
{
  resterm head;
  resterm *result = &head;
  for (vecterm *w = v; w != NULL; w = w->next)
    for (Nterm *t = w->coeff; t != 0; t = t->next)
      {
        result->next = new_term();
        result = result->next;
        result->comp = base[w->comp];
        result->coeff = t->coeff;
        M->copy(t->monom, result->monom);
        M->mult(result->monom, result->comp->base_monom, result->monom);
      }
  result->next = NULL;
  // Now we must sort these
  sort(head.next);
  return head.next;
}

resterm *res_poly::strip(const resterm *f) const
{
  resterm head;
  resterm *result = &head;
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    if (tm->comp->syz_type != SYZ_NOT_MINIMAL)
      {
        result->next = new_term();
        result = result->next;
        result->comp = tm->comp;
        result->coeff = K->copy(tm->coeff);
        M->copy(tm->monom, result->monom);
      }
  result->next = NULL;
  return head.next;
}

const resterm *res_poly::component_occurs_in(const res_pair *x,
                                             const resterm *f) const
{
  for (const resterm *tm = f; tm != NULL; tm = tm->next)
    if (tm->comp == x) return tm;
  return NULL;
}

void res_poly::sort(resterm *&f) const
{
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.

  if (f == NULL || f->next == NULL) return;
  resterm *f1 = NULL;
  resterm *f2 = NULL;
  while (f != NULL)
    {
      resterm *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == NULL) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }

  sort(f1);
  sort(f2);
  add_to(f1, f2);
  f = f1;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
