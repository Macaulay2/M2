// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "hermite.hpp"
#include "text_io.hpp"
#include "vector.hpp"
extern Z *ZZ;

hm_elem *HermiteComputation::new_gen(int i)
{
  hm_elem *result = new hm_elem;
  mpz_init(result->lead);
  if ((*gens)[i] == NULL)
    {
      result->f = NULL;
    }
  else
    {
      mpz_abs(result->lead, MPZ_VAL((*gens)[i]->coeff));
      result->f = gens->rows()->copy((*gens)[i]);
    }
  if (i < n_comps_per_syz)
    result->fsyz = syz->rows()->e_sub_i(i);
  else
    result->fsyz = NULL;
  result->next = NULL;
  return result;
}
void HermiteComputation::insert(hm_elem *p)
{
  if (p->f == NULL)
    {
      if (p->fsyz != NULL && collect_syz)
	syz->append(p->fsyz);
      mpz_clear(p->lead);
      delete p;
    }
  else
    {
      int i = p->f->comp;
      p->next = initial[i];
      initial[i] = p;
    }
}

HermiteComputation::HermiteComputation(const Matrix *m, int collsyz, int nsyz)
  : gb_comp(COMP_HERMITE),
  row(m->n_rows()-1),
  gens(m),
  GB_list(NULL),
  n_gb(0),
  collect_syz(collsyz)
{
  int i;

  for (i=0; i<m->n_rows(); i++)
    initial.append((hm_elem *)NULL);

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  n_comps_per_syz = nsyz;
  const FreeModule *F = m->cols()->sub_space(nsyz);  
  syz = new Matrix(F);

  for (i=0; i<m->n_cols(); i++)
    {
      hm_elem *p = new_gen(i);
      insert(p);
    }
}

void HermiteComputation::remove_hm_elem(hm_elem *&p)
{
  mpz_clear(p->lead);
  gens->rows()->remove(p->f);
  syz->rows()->remove(p->fsyz);
  delete p;
  p = NULL;
}

HermiteComputation::~HermiteComputation()
{
  for (int i=0; i<initial.length(); i++)
    {
      // remove the hm_elem list
      hm_elem *p = initial[i];
      while (p != NULL)
	{
	  hm_elem *tmp = p;
	  p = p->next;
	  remove_hm_elem(tmp);
	}
    }
  
  // Now remove the Groebner basis

  while (GB_list != NULL)
    {
      hm_elem *tmp = GB_list;
      GB_list = tmp->next;
      remove_hm_elem(tmp);
    }

  
}

int HermiteComputation::compare_elems(hm_elem *f, hm_elem *g) const
{
  int c = mpz_cmp(f->lead, g->lead);
  if (c < 0) return -1;
  if (c > 0) return 1;
  return 0;
}

hm_elem *HermiteComputation::merge(hm_elem *f, hm_elem *g)
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  hm_elem head;
  hm_elem *result = &head;
  hm_elem *h;
  while (1)
    switch (compare_elems(f, g))
      {
      case 1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case 0:
	// In this case, we remove one, and re-insert:
	if (mpz_cmp(MPZ_VAL(f->f->coeff), MPZ_VAL(g->f->coeff)) == 0)
	  {
	    vec f1 = gens->rows()->copy(f->f);
	    vec f2 = syz->rows()->copy(f->fsyz);
	    gens->rows()->subtract_to(g->f, f1);
	    syz->rows()->subtract_to(g->fsyz, f2);
	  }
	else
	  {
	    vec f1 = gens->rows()->copy(f->f);
	    vec f2 = syz->rows()->copy(f->fsyz);
	    gens->rows()->add_to(g->f, f1);
	    syz->rows()->add_to(g->fsyz, f2);
	  }

	h = g;
	// We need to reset the lead term
	if (g->f != NULL)
	  mpz_abs(h->lead, MPZ_VAL(g->f->coeff));
	g = g->next;
	insert(h);
	if (g == NULL)
	  {
	    result->next = f;
	    return head.next;
	  }
	// Now fall through to merge f into the result:
      case -1:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
	
      }
}

void HermiteComputation::sort(hm_elem *& p)
{
  // Sort in ascending absolute value of lead term
  if (p == NULL || p->next == NULL) return;
  hm_elem *p1 = NULL;
  hm_elem *p2 = NULL;
  while (p != NULL)
    {
      hm_elem *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort(p1);
  sort(p2);
  p = merge(p1, p2);
}

void HermiteComputation::reduce(hm_elem *&p, hm_elem *q)
{
  // compute (u,v) s.t. u lead(p) + v lead(q) = gcd
  // set p <- u*p + v*q;
  // set q <- lead(q)/gcd * p - lead(p)/gcd * q
  // DOn't forget to also reset the 'lead' fields!
  ring_elem u, v;
  ring_elem g = ZZ->gcd_extended(p->f->coeff, q->f->coeff, u, v);
  ring_elem a = ZZ->divide(q->f->coeff, g); // exact
  ring_elem b = ZZ->divide(p->f->coeff, g); // exact
  ZZ->negate_to(b);

  vec p1 = gens->rows()->mult(u, p->f);
  vec p2 = gens->rows()->mult(v, q->f);
  gens->rows()->add_to(p1, p2);

  vec syz1 = syz->rows()->mult(u, p->fsyz);
  vec syz2 = syz->rows()->mult(v, q->fsyz);
  syz->rows()->add_to(syz1, syz2);

  vec q1 = gens->rows()->mult(a, p->f);
  vec q2 = gens->rows()->mult(b, q->f);
  gens->rows()->add_to(q1, q2);

  vec qsyz1 = syz->rows()->mult(a, p->fsyz);
  vec qsyz2 = syz->rows()->mult(b, q->fsyz);
  syz->rows()->add_to(qsyz1, qsyz2);
  
  gens->rows()->remove(p->f);
  gens->rows()->remove(q->f);
  syz->rows()->remove(p->fsyz);
  syz->rows()->remove(q->fsyz);
  ZZ->remove(a);
  ZZ->remove(b);
  ZZ->remove(g);
  ZZ->remove(u);
  ZZ->remove(v);

  // Now that the arithmetic has been done, put back into 'p', 'q':
  p->f = p1;
  p->fsyz = syz1;
  mpz_set(p->lead, MPZ_VAL(p->f->coeff));

  q->f = q1;
  q->fsyz = qsyz1;
  if (q->f != NULL)
    mpz_abs(q->lead, MPZ_VAL(q->f->coeff));

  insert(q);
}

int HermiteComputation::calc(const int *, const intarray &/*stop*/)
{
  // ngb = stop[0]
  // nsyz = stop[1]
  // npairs = stop[2]
  for ( ; row >= 0; row--)
    {
      hm_elem *p = initial[row];
      if (p == NULL) continue;
      initial[row] = NULL;
      sort(p);			// This can remove elements, inserting them back
      while (p != NULL && p->next != NULL)
	{
	  hm_elem *pnext = p->next->next;
	  reduce(p, p->next);		// replaces p1, and re-inserts p2.
	  p->next = pnext;
	}
      // At this point, 'p' is the only remaining element with this lead term
      // So insert it into GB_list
      p->next = GB_list;
      GB_list = p;
      n_gb++;
    }
  return COMP_DONE;
}

Matrix *HermiteComputation::min_gens_matrix()
{
  return gb_matrix();
}

Matrix *HermiteComputation::initial_matrix(int)
{
#warning "implement HermiteComputation::initial_matrix"
#if 0
  // MES aug 2002
  Matrix *result = new Matrix(gens->rows());
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    result->append(gens->rows()->lead_term(p->f));
  return result;
#endif
  return 0;
}

Matrix *HermiteComputation::gb_matrix()
{
  Matrix *result = new Matrix(gens->rows());
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    result->append(gens->rows()->copy(p->f));
  return result;
}

Matrix *HermiteComputation::change_matrix()
{
  Matrix *result = new Matrix(syz->rows());
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    result->append(syz->rows()->copy(p->fsyz));
  return result;
}

Matrix *HermiteComputation::syz_matrix()
{
  return syz;
}
void HermiteComputation::stats() const
{
  buffer o;
  for (int i=0; i<gens->n_rows(); i++)
    if (initial[i] != NULL)
      {
	o << "--- component " << i << " -----" << newline;
	for (hm_elem *p = initial[i]; p!=NULL; p=p->next)
	  {
	    bignum_text_out(o, p->lead);
	    o << " ## ";
	    gens->rows()->elem_text_out(o, p->f);
	    o << " ## ";
	    syz->rows()->elem_text_out(o, p->fsyz);
	    o << newline;
	  }
      }
  o << newline;
  syz->text_out(o);
  o << newline;
  emit(o.str());
}

void HermiteComputation::gb_reduce(vec &f, vec & /*fsyz*/) const
{
  // Reduce f so that each of its terms are < corresponding initial term
  // (in absolute value).
  vecterm head;
  vecterm *result = &head;
  ERROR("reduction over ZZ not implemented yet");
#if 0
  ring_elem coeff;
  while (f != NULL)
    {
      int c = f->comp;
      
      hm_elem *p = GB_list;
      while (p != NULL)
	{
	  if (p->f->comp == c)
	    break;
	  if (p->f->comp > c)
	    {
	      p = NULL;
	      break;
	    }
	  p = p->next;
	}
      if (p != NULL)
	{
	  // Check whether we should subtract some multiple
	  mpz_abs(a, MPZ_VAL(f->coeff));
	  int cmp = mpz_cmp(a, MPZ_VAL(p->f->coeff));
	  int sgn = mpz_sgn(MPZ_VAL(f->coeff));
	  if (sgn > 0 && cmp >= 0)
	    {
	      // determine the multiple
	      // subtract off from f, fsyz

	      // Perform the division:
	      mpz_...(a, f->coeff, p->f->coeff);
	      vec f1 = gens->rows()->mult(a, p->f);
	      vec fsyz1 = syz->rows()->mult(a,p->fsyz);
	      ges.rows()->subtract_to(f, f1);
	      syz->rows()->subtract_to(fsyz, fsyz1);
	    }
	}
      // If no divisors, or the divisor is larger than lead coeff of f:
      if (f->comp == c)
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }
#endif
  result->next = NULL;
  f = head.next;
}
Matrix *HermiteComputation::reduce(const Matrix *m, Matrix *&lift)
{
  Matrix *red = new Matrix(m->rows(), m->cols(), m->degree_shift());
  lift = new Matrix(syz->rows(), m->cols());
  if (m->n_rows() != gens->rows()->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = gens->rows()->copy((*m)[i]);
      vec fsyz = NULL;

      gb_reduce(f, fsyz);
      syz->rows()->negate_to(fsyz);
      (*red)[i] = f;
      (*lift)[i] = fsyz;
    }
  return red;
}

Vector *HermiteComputation::reduce(const Vector *v, Vector *&lift)
{
  if (!v->free_of()->is_equal(gens->rows()))
    {
      ERROR("reduce: vector is in incorrect free module");
      return 0;
    }
  vec f = gens->rows()->copy(v->get_value());
  vec fsyz = NULL;

  gb_reduce(f, fsyz);
  syz->rows()->negate_to(fsyz);

  lift = Vector::make_raw(syz->rows(), fsyz);
  return Vector::make_raw(gens->rows(), f);

}
int HermiteComputation::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = gens->rows()->translate(m->rows(),(*m)[i]);
      vec fsyz = NULL;
      gb_reduce(f, fsyz);
      syz->rows()->remove(fsyz);
      if (f != NULL)
	{
	  gens->rows()->remove(f);
	  return i;
	}
    }
  return -1;
}
bool HermiteComputation::is_equal(const gb_comp *q1)
{
  if (kind() != q1->kind()) return false;
  const HermiteComputation *qq = (const HermiteComputation *) q1;
  if (gens->rows()->rank() != qq->gens->rows()->rank()) return false;

  hm_elem *q = qq->GB_list;
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    {
      if (q == NULL) return false;
      if (!gens->rows()->is_equal(p->f, q->f))
	return false;
      q = q->next;
    }
  if (q != NULL) return false;
  return true;
}
