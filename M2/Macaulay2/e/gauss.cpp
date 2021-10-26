// Copyright 1997-2005  Michael E. Stillman

#include "style.hpp"
#include "gauss.hpp"
#include "text-io.hpp"
#include "matrix-con.hpp"
#include "newdelete.hpp"
#include "interrupted.hpp"

#include <iostream>
extern RingZZ *globalZZ;

int GaussElimComputation::complete_thru_degree() const
// The computation is complete up through this degree.
{
  if (status() == COMP_DONE) return 0;
  return -1;
}

gm_elem *GaussElimComputation::new_gen(int i)
{
  gm_elem *result = new gm_elem;
  result->f = R->copy_vec(gens->elem(i));

  if (i < n_comps_per_syz)
    result->fsyz = R->make_vec(i, R->one());
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
          syz_list.push_back(p->fsyz);
          n_syz++;
        }
      freemem(p);
    }
  else
    {
      ring_elem leadinv = R->invert(p->f->coeff);
      R->mult_vec_to(p->f, leadinv, false);
      R->mult_vec_to(p->fsyz, leadinv, false);
      p->nterms = R->n_nonzero_terms(p->f);
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

GaussElimComputation::GaussElimComputation(const Matrix *m,
                                           int collsyz,
                                           int nsyz)
    : row(m->n_rows() - 1),
      R(m->get_ring()),
      gens(m),
      n_gb(0),
      n_pairs(0),
      n_syz(0),
      collect_syz(collsyz)
{
  int i;

  typedef struct gm_elem *gm_elem_ptr;
  reduce_list = newarray(gm_elem_ptr, m->n_rows());
  gb_list = newarray(gm_elem_ptr, m->n_rows());

  for (i = 0; i < m->n_rows(); i++)
    {
      reduce_list[i] = NULL;
      gb_list[i] = NULL;
    }

  if (nsyz < 0 || nsyz > m->n_cols()) nsyz = m->n_cols();
  n_comps_per_syz = nsyz;
  Fsyz = m->cols()->sub_space(nsyz);

  for (i = 0; i < m->n_cols(); i++)
    {
      gm_elem *p = new_gen(i);
      insert(p);
    }
}

void GaussElimComputation::remove_gm_elem(gm_elem *&p)
{
  if (p != NULL)
    {
      R->remove_vec(p->f);
      R->remove_vec(p->fsyz);
      freemem(p);
      p = NULL;
    }
}

GaussElimComputation::~GaussElimComputation()
{
  for (int i = 0; i < gens->n_rows(); i++)
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
  ring_elem d2 = R->negate(c2);
  vec v1 = R->mult_vec(c1, q->f);
  vec v2 = R->mult_vec(d2, p->f);
  vec s1 = R->mult_vec(c1, q->fsyz);
  vec s2 = R->mult_vec(d2, p->fsyz);
  R->remove_vec(q->f);
  R->remove_vec(q->fsyz);
  R->remove(d2);
  R->add_vec_to(v1, v2);
  R->add_vec_to(s1, s2);
  q->f = v1;
  q->fsyz = s1;
}

void GaussElimComputation::reduce(vec &f)
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
          c = R->negate(c);
          vec g = R->mult_vec(c, gb_list[r]->f);
          R->add_vec_to(f, g);
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
void GaussElimComputation::reduce(vec &f, vec &fsyz, bool tail_only)
{
  if (f == 0) return;
  vecterm head;
  vecterm *result = &head;

  if (tail_only)
    {
      // Don't reduce the head term.
      result->next = f;
      f = f->next;
      result = result->next;
    }
  while (f != NULL)
    {
      int r = f->comp;
      if (gb_list[r] != NULL)
        {
          // Reduce w.r.t. this term
          ring_elem c = f->coeff;
          c = R->negate(c);
          vec gsyz = R->mult_vec(c, gb_list[r]->fsyz);
          vec g = R->mult_vec(c, gb_list[r]->f);
          R->add_vec_to(f, g);
          R->add_vec_to(fsyz, gsyz);
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
void GaussElimComputation::start_computation()
{
  if (status() == COMP_DONE) return;
  for (; row >= 0; row--)
    {
      if (gb_list[row] == NULL) continue;
      while (reduce_list[row] != NULL)
        {
          gm_elem *p = reduce_list[row];
          reduce_list[row] = p->next;
          p->next = NULL;
          reduce(gb_list[row], p);  // replaces p
          if (M2_gbTrace >= 3)
            {
              if (p->f == NULL)
                {
                  if (p->fsyz == NULL)
                    emit_wrapped("o");
                  else
                    emit_wrapped("z");
                }
              else
                emit_wrapped("r");
            }
          else
            {
            }
          insert(p);
          n_pairs++;
          if (system_interrupted())
            {
              set_status(COMP_INTERRUPTED);
              return;
            }
          if (n_pairs == stop_.pair_limit)
            {
              set_status(COMP_DONE_PAIR_LIMIT);
              return;
            }
          if (n_syz == stop_.syzygy_limit)
            {
              set_status(COMP_DONE_SYZYGY_LIMIT);
              return;
            }
        }
    }
  // Now auto reduce these
  for (int r = 1; r < gens->n_rows(); r++)
    {
      if (gb_list[r] == 0) continue;
      reduce(gb_list[r]->f, gb_list[r]->fsyz, true);
    }
  if (M2_gbTrace >= 10)
    {
      buffer o;
      text_out(o);
      emit(o.str());
    }
  set_status(COMP_DONE);
}

const Matrix *GaussElimComputation::get_mingens() { return get_gb(); }
const Matrix *GaussElimComputation::get_initial(int nparts)
{
  MatrixConstructor mat(gens->rows(), 0);
  for (int i = 0; i < gens->n_rows(); i++)
    if (gb_list[i] != NULL)
      {
        vec v = gb_list[i]->f;
        mat.append(R->make_vec(v->comp, v->coeff));
      }
  return mat.to_matrix();
}

const Matrix *GaussElimComputation::get_gb()
{
  MatrixConstructor mat(gens->rows(), 0);
  for (int i = 0; i < gens->n_rows(); i++)
    if (gb_list[i] != NULL) mat.append(R->copy_vec(gb_list[i]->f));
  return mat.to_matrix();
}

const Matrix *GaussElimComputation::get_change()
{
  MatrixConstructor mat(Fsyz, 0);
  for (int i = 0; i < gens->n_rows(); i++)
    if (gb_list[i] != NULL) mat.append(R->copy_vec(gb_list[i]->fsyz));
  return mat.to_matrix();
}

const Matrix *GaussElimComputation::get_syzygies()
{
  MatrixConstructor mat(Fsyz, 0);
  for (int i = 0; i < syz_list.size(); i++)
    mat.append(R->copy_vec(syz_list[i]));
  return mat.to_matrix();
}

void GaussElimComputation::text_out(buffer &o) const
{
  o << newline;
  for (int i = 0; i < gens->n_rows(); i++)
    {
      if (gb_list[i] != NULL)
        {
          o << "--- component " << i << " -----" << newline;
          o << "gb elem = ";
          R->vec_text_out(o, gb_list[i]->f);
          o << newline;
        }
      else if (reduce_list[i] != NULL)
        o << "--- component " << i << " -----" << newline;
      for (gm_elem *p = reduce_list[i]; p != NULL; p = p->next)
        {
          o << p->nterms;
          o << " ## ";
          R->vec_text_out(o, p->f);
          o << " ## ";
          R->vec_text_out(o, p->fsyz);
          o << newline;
        }
    }
  o << newline;
  for (int i = 0; i < syz_list.size(); i++) R->vec_text_out(o, syz_list[i]);
  o << newline;
}

const Matrix /* or null */ *GaussElimComputation::matrix_remainder(
    const Matrix *m)
{
  if (m->get_ring() != R)
    {
      ERROR("encountered different rings");
      return 0;
    }
  if (m->n_rows() != gens->rows()->rank())
    {
      ERROR("expected matrices to have same number of rows");
      return 0;
    }
  MatrixConstructor mat_remainder(m->rows(), m->cols(), m->degree_shift());
  for (int i = 0; i < m->n_cols(); i++)
    {
      vec f = R->copy_vec(m->elem(i));

      reduce(f);
      mat_remainder.set_column(i, f);
    }
  return mat_remainder.to_matrix();
}

M2_bool GaussElimComputation::matrix_lift(
    const Matrix *m,
    const Matrix /* or null */ **result_remainder,
    const Matrix /* or null */ **result_quotient)
{
  if (m->get_ring() != R)
    {
      ERROR("encountered different rings");
      *result_remainder = 0;
      *result_quotient = 0;
      return false;
    }
  if (m->n_rows() != gens->rows()->rank())
    {
      ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
      return false;
    }
  MatrixConstructor mat_remainder(m->rows(), m->cols(), m->degree_shift());
  MatrixConstructor mat_quotient(Fsyz, m->cols(), 0);
  bool all_zeroes = true;
  for (int i = 0; i < m->n_cols(); i++)
    {
      vec f = R->copy_vec(m->elem(i));
      vec fsyz = NULL;

      reduce(f, fsyz);
      R->negate_vec_to(fsyz);
      if (f != 0) all_zeroes = false;
      mat_remainder.set_column(i, f);
      mat_quotient.set_column(i, fsyz);
    }
  *result_remainder = mat_remainder.to_matrix();
  *result_quotient = mat_quotient.to_matrix();
  return all_zeroes;
}

int GaussElimComputation::contains(const Matrix *m)
// Return -1 if every column of 'm' reduces to zero.
// Otherwise return the index of the first column that
// does not reduce to zero.
{
  if (m->get_ring() != R)
    {
      ERROR("encountered different ring");
      return -1;
    }
  // Reduce each column of m one by one.
  for (int i = 0; i < m->n_cols(); i++)
    {
      vec f = R->copy_vec(m->elem(i));
      reduce(f);
      if (f != NULL)
        {
          R->remove_vec(f);
          return i;
        }
    }
  return -1;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
