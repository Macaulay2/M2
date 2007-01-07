// Copyright 1996-2005  Michael E. Stillman

#include "style.hpp"
#include "hermite.hpp"
#include "text_io.hpp"
#include "matrixcon.hpp"

extern RingZZ *globalZZ;

int HermiteComputation::complete_thru_degree() const
  // The computation is complete up through this degree.
{
  if (status() == COMP_DONE) return 0;
  return -1;
}

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
      mpz_abs(result->lead, (*gens)[i]->coeff.get_mpz());
      result->f = globalZZ->copy_vec(gens->elem(i));
    }
  if (i < n_comps_per_syz)
    result->fsyz = globalZZ->e_sub_i(i);
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
	syz_list.push_back(p->fsyz);
      mpz_clear(p->lead);
      deleteitem(p);
    }
  else
    {
      int i = p->f->comp;
      p->next = initial[i];
      initial[i] = p;
    }
}

HermiteComputation::HermiteComputation(const Matrix *m, int collsyz, int nsyz)
  : row(m->n_rows()-1),
  gens(m),
  GB_list(NULL),
  n_gb(0),
  collect_syz(collsyz)
{
  int i;

  for (i=0; i<m->n_rows(); i++)
    initial.push_back(NULL);

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  n_comps_per_syz = nsyz;
  Fsyz = m->cols()->sub_space(nsyz);  

  for (i=0; i<m->n_cols(); i++)
    {
      hm_elem *p = new_gen(i);
      insert(p);
    }
}

void HermiteComputation::remove_hm_elem(hm_elem *&p)
{
  mpz_clear(p->lead);
  globalZZ->remove_vec(p->f);
  globalZZ->remove_vec(p->fsyz);
  deleteitem(p);
  p = NULL;
}

HermiteComputation::~HermiteComputation()
{
  for (int i=0; i<initial.size(); i++)
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
	if (mpz_cmp(f->f->coeff.get_mpz(), g->f->coeff.get_mpz()) == 0)
	  {
	    vec f1 = globalZZ->copy_vec(f->f);
	    vec f2 = globalZZ->copy_vec(f->fsyz);
	    globalZZ->subtract_vec_to(g->f, f1);
	    globalZZ->subtract_vec_to(g->fsyz, f2);
	  }
	else
	  {
	    vec f1 = globalZZ->copy_vec(f->f);
	    vec f2 = globalZZ->copy_vec(f->fsyz);
	    globalZZ->add_vec_to(g->f, f1);
	    globalZZ->add_vec_to(g->fsyz, f2);
	  }

	h = g;
	// We need to reset the lead term
	if (g->f != NULL)
	  mpz_abs(h->lead, g->f->coeff.get_mpz());
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
  ring_elem g = globalZZ->gcd_extended(p->f->coeff, q->f->coeff, u, v);
  ring_elem a = globalZZ->divide(q->f->coeff, g); // exact
  ring_elem b = globalZZ->divide(p->f->coeff, g); // exact
  globalZZ->negate_to(b);

  vec p1 = globalZZ->mult_vec(u, p->f);
  vec p2 = globalZZ->mult_vec(v, q->f);
  globalZZ->add_vec_to(p1, p2);

  vec syz1 = globalZZ->mult_vec(u, p->fsyz);
  vec syz2 = globalZZ->mult_vec(v, q->fsyz);
  globalZZ->add_vec_to(syz1, syz2);

  vec q1 = globalZZ->mult_vec(a, p->f);
  vec q2 = globalZZ->mult_vec(b, q->f);
  globalZZ->add_vec_to(q1, q2);

  vec qsyz1 = globalZZ->mult_vec(a, p->fsyz);
  vec qsyz2 = globalZZ->mult_vec(b, q->fsyz);
  globalZZ->add_vec_to(qsyz1, qsyz2);
  
  globalZZ->remove_vec(p->f);
  globalZZ->remove_vec(q->f);
  globalZZ->remove_vec(p->fsyz);
  globalZZ->remove_vec(q->fsyz);
  globalZZ->remove(a);
  globalZZ->remove(b);
  globalZZ->remove(g);
  globalZZ->remove(u);
  globalZZ->remove(v);

  // Now that the arithmetic has been done, put back into 'p', 'q':
  p->f = p1;
  p->fsyz = syz1;
  mpz_set(p->lead, p->f->coeff.get_mpz());

  q->f = q1;
  q->fsyz = qsyz1;
  if (q->f != NULL)
    mpz_abs(q->lead, q->f->coeff.get_mpz());

  insert(q);
}

void HermiteComputation::start_computation()
{
  // ngb = stop[0]
  // nsyz = stop[1]
  // npairs = stop[2]
  if (status() == COMP_DONE) return;
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
  // At this point, we are done, so reset initial[...] (it is all NULL right now)

  // Now let's auto reduce the result...
  // We will reduce the GB elems 
  // one by one, placing them back into the 'initial' table.
  // We use the following: the lead components of elements of GB_list are in increasing order

  for (hm_elem *p = GB_list; p != 0; p = p->next)
    {
      if (!globalZZ->is_positive(p->f->coeff))
	{
	  vec f = globalZZ->negate_vec(p->f);
	  vec fsyz = globalZZ->negate_vec(p->fsyz);
	  globalZZ->remove_vec(p->f);
	  globalZZ->remove_vec(p->fsyz);
	  p->f = f;
	  p->fsyz = fsyz;
	}
      gb_reduce(p->f, p->fsyz);
      initial[p->f->comp] = p;
    }

  //  for (hm_elem *p = GB_list; p != 0; p = p->next)
  //    initial[p->f->comp] = p;
  set_status(COMP_DONE);
}



/*************************
 ** Top level interface **
 *************************/

const MatrixOrNull *HermiteComputation::get_gb()
{
  MatrixConstructor mat(gens->rows(), 0);
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    mat.append(globalZZ->copy_vec(p->f));
  return mat.to_matrix();
}

const MatrixOrNull *HermiteComputation::get_mingens()
{
  // return the minimal generators (or as minimal as possible?)
  return get_gb();
}

const MatrixOrNull *HermiteComputation::get_change()
{
  MatrixConstructor mat(Fsyz, 0);
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    mat.append(globalZZ->copy_vec(p->fsyz));
  return mat.to_matrix();
}

const MatrixOrNull *HermiteComputation::get_syzygies()
{
  MatrixConstructor mat(Fsyz, 0);
  for (int i=0; i<syz_list.size(); i++)
    mat.append(globalZZ->copy_vec(syz_list[i]));
  return mat.to_matrix();
}

const MatrixOrNull *HermiteComputation::get_initial(int nparts)
{
  MatrixConstructor mat(gens->rows(), 0);
  for (hm_elem *p = GB_list; p != NULL; p = p->next)
    {
      vec v = p->f;
      mat.append(globalZZ->make_vec(v->comp, v->coeff));
    }
  return mat.to_matrix();
}

void HermiteComputation::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  for (int i=0; i<gens->n_rows(); i++)
    if (initial[i] != NULL)
      {
	o << "--- component " << i << " -----" << newline;
	for (hm_elem *p = initial[i]; p!=NULL; p=p->next)
	  {
	    bignum_text_out(o, p->lead);
	    o << " ## ";
	    globalZZ->vec_text_out(o, p->f);
	    o << " ## ";
	    globalZZ->vec_text_out(o, p->fsyz);
	    o << newline;
	    // If the computation is done, this is the GB we are displaying
	    // but we only want the first element in each list.
	    if (status() == COMP_DONE) break;
	  }
      }
  o << newline << "--- syzygies ---" << newline;
  for (int i=0; i<syz_list.size(); i++)
    globalZZ->vec_text_out(o,syz_list[i]);
  o << newline;
}

void HermiteComputation::gb_reduce(vec &f) const
{
  // Reduce f so that each of its terms are < corresponding initial term
  // (in absolute value).
  vecterm head;
  vecterm *result = &head;
  head.next = 0;
  while (f != 0) 
    {
      int x = f->comp;
      hm_elem *h = initial[x];
      if (h != 0)
	{
	  ring_elem v;
	  ring_elem rem = globalZZ->remainderAndQuotient(f->coeff,h->f->coeff,v);
	  bool do_reduce = !globalZZ->is_zero(v);
	  if (do_reduce)
	    {
	      v = globalZZ->negate(v);
	      vec g = globalZZ->mult_vec(v,h->f);
	      globalZZ->add_vec_to(f,g);
	    }
	  if (globalZZ->is_zero(rem))
	    continue;
	}
      // The lead term stays
      result->next = f;
      f = f->next;
      result = result->next;
      result->next = 0;
      continue;
    }

  f = head.next;
}

void HermiteComputation::gb_reduce(vec &f, vec &fsyz) const
{
  // Reduce f so that each of its terms are < corresponding initial term
  // (in absolute value).
  vecterm head;
  vecterm *result = &head;
  head.next = 0;
  while (f != 0) 
    {
      int x = f->comp;
      hm_elem *h = initial[x];
      if (h != 0)
	{
	  ring_elem v;
	  ring_elem rem = globalZZ->remainderAndQuotient(f->coeff,h->f->coeff,v);
	  bool do_reduce = !globalZZ->is_zero(v);
	  if (do_reduce)
	    {
	      v = globalZZ->negate(v);
	      vec g = globalZZ->mult_vec(v,h->f);
	      globalZZ->add_vec_to(f,g);
	      vec gsyz = globalZZ->mult_vec(v,h->fsyz);
	      globalZZ->add_vec_to(fsyz,gsyz);

	    }
	  if (globalZZ->is_zero(rem))
	    continue;
	}
      // The lead term stays
      result->next = f;
      f = f->next;
      result = result->next;
      result->next = 0;
      continue;
    }

  f = head.next;
}

const MatrixOrNull *HermiteComputation::matrix_remainder(const Matrix *m)
{
  if (m->get_ring() != globalZZ)
    {
      ERROR("expected matrix over ZZ");
      return 0;
    }
  if (m->n_rows() != gens->rows()->rank()) 
    {
      ERROR("expected matrices to have same number of rows");
      return 0;
    }
  MatrixConstructor mat_remainder(m->rows(), m->cols(), m->degree_shift());
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = globalZZ->copy_vec(m->elem(i));

      gb_reduce(f);
      mat_remainder.set_column(i, f);
    }
  return mat_remainder.to_matrix();
}


void HermiteComputation::matrix_lift(const Matrix *m,
				     MatrixOrNull **result_remainder,
				     MatrixOrNull **result_quotient)
{
  if (m->get_ring() != globalZZ)
    {
      ERROR("expected matrix over ZZ");
      *result_remainder = 0;
      *result_quotient = 0;
    }
  if (m->n_rows() != gens->rows()->rank()) 
    {
      ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
    }
  MatrixConstructor mat_remainder(m->rows(), m->cols(), m->degree_shift());
  MatrixConstructor mat_quotient(Fsyz, m->cols(), 0);
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = globalZZ->copy_vec(m->elem(i));
      vec fsyz = NULL;

      gb_reduce(f, fsyz);
      globalZZ->negate_vec_to(fsyz);
      mat_remainder.set_column(i, f);
      mat_quotient.set_column(i, fsyz);
    }
  *result_remainder = mat_remainder.to_matrix();
  *result_quotient = mat_quotient.to_matrix();
}

int HermiteComputation::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  if (m->get_ring() != globalZZ)
    {
      ERROR("expected matrix over ZZ");
      return -1;
    }
  // Reduce each column of m one by one.
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = globalZZ->copy_vec(m->elem(i));
      gb_reduce(f);
      if (f != NULL)
	{
	  globalZZ->remove_vec(f);
	  return i;
	}
    }
  return -1;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
