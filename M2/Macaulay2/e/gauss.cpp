// Copyright 1997  Michael E. Stillman

#include "style.hpp"
#include "gauss.hpp"
#include "text_io.hpp"
#include "vector.hpp"

extern ZZ *globalZZ;

gm_elem *GaussElimComputation::new_gen(int i)
{
  gm_elem *result = new gm_elem;
  result->f = gens->rows()->copy((*gens)[i]);

  if (i < n_comps_per_syz)
    result->fsyz = syz->rows()->e_sub_i(i);
  else
    result->fsyz = NULL;
  result->next = NULL;
  return result;
}
void GaussElimComputation::insert(gm_elem *p)
{
  if (p->f == NULL)
    {
      if (p->fsyz != NULL && collect_syz)
	{
	  syz->append(p->fsyz);
	  n_syz++;
	}
      deleteitem(p);
    }
  else
    {
      gens->rows()->make_monic(p->f, p->fsyz);
      p->nterms = gens->rows()->n_terms(p->f);
      int i = p->f->comp;
      if (gb_list[i] == NULL)
	{
	  gb_list[i] = p;
	  n_gb++;
	}
      else 
	{
	  if (p->nterms < gb_list[i]->nterms)
	    {
	      gm_elem *q = p;
	      p = gb_list[i];
	      gb_list[i] = q;
	    }
	  p->next = reduce_list[i];
	  reduce_list[i] = p;
	}
    }
}

GaussElimComputation::GaussElimComputation(const Matrix *m, int collsyz, int nsyz)
  : gb_comp(COMP_GAUSS),
  row(m->n_rows()-1),
  gens(m),
  n_gb(0),
  n_pairs(0),
  n_syz(0),
  collect_syz(collsyz)
{
  int i;

  typedef struct gm_elem *gm_elem_ptr;
  reduce_list = new gm_elem_ptr[m->n_rows()];
  gb_list = new gm_elem_ptr[m->n_rows()];

  for (i=0; i<m->n_rows(); i++)
    {
      reduce_list[i] = (gm_elem *)NULL;
      gb_list[i] = (gm_elem *)NULL;
    }

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  n_comps_per_syz = nsyz;
  const FreeModule *F = m->cols()->sub_space(nsyz);  
  syz = new Matrix(F);

  for (i=0; i<m->n_cols(); i++)
    {
      gm_elem *p = new_gen(i);
      insert(p);
    }
}

void GaussElimComputation::remove_gm_elem(gm_elem *&p)
{
  if (p != NULL)
    {
      gens->rows()->remove(p->f);
      syz->rows()->remove(p->fsyz);
      deleteitem(p);
      p = NULL;
    }
}

GaussElimComputation::~GaussElimComputation()
{
  for (int i=0; i<gens->n_rows(); i++)
    {
      // remove the gm_elem list
      gm_elem *p = reduce_list[i];
      while (p != NULL)
	{
	  gm_elem *tmp = p;
	  p = p->next;
	  remove_gm_elem(tmp);
	}
      remove_gm_elem(gb_list[i]);
    }
}


void GaussElimComputation::reduce(gm_elem *&p, gm_elem *q)
{
  // MES: rewrite.
  // Reduce q by p.  This is a single step reduction.
  // The element is then inserted further down.
  
  ring_elem c1 = p->f->coeff;
  ring_elem c2 = q->f->coeff;
  ring_elem d2 = gens->get_ring()->negate(c2);
  vec v1 = gens->rows()->mult(c1, q->f);
  vec v2 = gens->rows()->mult(d2, p->f);
  vec s1 = syz->rows()->mult(c1, q->fsyz);
  vec s2 = syz->rows()->mult(d2, p->fsyz);
  gens->rows()->remove(q->f);
  syz->rows()->remove(q->fsyz);
  gens->get_ring()->remove(d2);
  gens->rows()->add_to(v1, v2);
  syz->rows()->add_to(s1, s2);
  q->f = v1;
  q->fsyz = s1;
}

void GaussElimComputation::reduce(vec &f, vec &fsyz)
{
  vecterm head;
  vecterm *result = &head;

  while (f != NULL)
    {
      int r = f->comp;
      if (gb_list[r] != NULL)
	{
	  // Reduce w.r.t. this term
	  ring_elem c = f->coeff;
	  syz->rows()->subtract_multiple_to(fsyz, c, NULL, gb_list[r]->fsyz);
	  gens->rows()->subtract_multiple_to(f, c, NULL, gb_list[r]->f);
	}
      else
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }

  result->next = NULL;
  f = head.next;
  
}
int GaussElimComputation::calc(const int *, const intarray &stop)
{
  // ngb = stop[0]
  // nsyz = stop[1]
  // npairs = stop[2]
  for ( ; row >= 0; row--)
    {
      if (gb_list[row] == NULL) continue;
      while (reduce_list[row] != NULL)
	{
	  gm_elem *p = reduce_list[row];
	  reduce_list[row] = p->next;
	  p->next = NULL;
	  reduce(gb_list[row], p); // replaces p
	  if (comp_printlevel >= 3)
	    if (p->f == NULL)
	      if (p->fsyz == NULL)
		emit_wrapped("o");
	      else
		emit_wrapped("z");
	    else
	      emit_wrapped("r");
	  insert(p);
	  n_pairs++;
	  if (system_interrupted)
	    return COMP_INTERRUPTED;
	  if (n_pairs == stop[2])
	    return COMP_DONE_PAIR_LIMIT;
	  if (stop[1] == n_syz)
	    return COMP_DONE_SYZYGY_LIMIT;
	}
    }
  return COMP_DONE;
}

Matrix *GaussElimComputation::min_gens_matrix()
{
  return gb_matrix();
}

Matrix *GaussElimComputation::initial_matrix(int)
{
  Matrix *result = new Matrix(gens->rows());
  for (int i=0; i<gens->n_rows(); i++)
    if (gb_list[i] != NULL)
      result->append(gens->rows()->lead_term(gb_list[i]->f));
  return result;
}

Matrix *GaussElimComputation::gb_matrix()
{
  Matrix *result = new Matrix(gens->rows());
  for (int i=0; i<gens->n_rows(); i++)
    if (gb_list[i] != NULL)
      result->append(gens->rows()->copy(gb_list[i]->f));
  return result;
}

Matrix *GaussElimComputation::change_matrix()
{
  Matrix *result = new Matrix(syz->rows());
  for (int i=0; i<gens->n_rows(); i++)
    if (gb_list[i] != NULL)
      result->append(syz->rows()->copy(gb_list[i]->fsyz));
  return result;
}

Matrix *GaussElimComputation::syz_matrix()
{
  return syz;
}
void GaussElimComputation::stats() const
{
  buffer o;
  for (int i=0; i<gens->n_rows(); i++) {
    if (gb_list[i] != NULL)
      {
	o << "--- component " << i << " -----" << newline;
	o << "gb elem = ";
	gens->rows()->elem_text_out(o, gb_list[i]->f);
	o << newline;
      }
    else if (reduce_list[i] != NULL)
	o << "--- component " << i << " -----" << newline;
    for (gm_elem *p = reduce_list[i]; p!=NULL; p=p->next)
      {
	o << p->nterms;
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

Matrix *GaussElimComputation::reduce(const Matrix *m, Matrix *&lift)
{
  if (m->n_rows() != gens->rows()->rank()) {
       ERROR("expected matrices to have same number of rows");
       lift = 0;
       return 0;
  }
  Matrix *red = new Matrix(m->rows(), m->cols(), m->degree_shift());
  lift = new Matrix(syz->rows(), m->cols());
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = gens->rows()->translate(m->rows(),(*m)[i]);
      vec fsyz = NULL;

      reduce(f, fsyz);
      syz->rows()->negate_to(fsyz);
      (*red)[i] = f;
      (*lift)[i] = fsyz;
    }
  return red;
}

int GaussElimComputation::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m->n_cols(); i++)
    {
      vec f = gens->rows()->translate(m->rows(),(*m)[i]);
      vec fsyz = NULL;
      reduce(f, fsyz);
      syz->rows()->remove(fsyz);
      if (f != NULL)
	{
	  gens->rows()->remove(f);
	  return i;
	}
    }
  return -1;
}
bool GaussElimComputation::is_equal(const gb_comp *q)
{
  if (kind() != q->kind()) return false;
  GaussElimComputation *qq = (GaussElimComputation *) q;
  if (gens->rows() != qq->gens->rows()) return false;

  for (int i=0; i<gens->n_rows(); i++)
    {
      if (gb_list[i] == NULL)
	{
	  if (qq->gb_list[i] != NULL) return false;
	  continue;
	}
      if (!gens->rows()->is_equal(gb_list[i]->f, qq->gb_list[i]->f))
	return false;
    }
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
