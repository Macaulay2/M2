// Copyright 1997  Michael E. Stillman

#include "res-a2.hpp"
#include "hilb.hpp"
#include "text-io.hpp"
#include "matrix-con.hpp"
#include "interrupted.hpp"
extern ring_elem hilb(const Matrix &M, const Ring *RR);

gb_emitter::gb_emitter(const Matrix *m)
    : gens(m), g(NULL), n_left(m->n_cols()), n_i(0)
{
  originalR = m->get_ring()->cast_to_PolynomialRing();
  assert(originalR != 0);
  GR = originalR->get_gb_ring();
  this_degree = m->cols()->lowest_primary_degree() - 1;
  n_gens = 0;  // Also needs to be set at that time.
  these = newarray_atomic_clear(int, m->n_cols());
}

gb_emitter::~gb_emitter() { freemem(these); }
RingElement *gb_emitter::hilbertNumerator()
{
  assert(0);  // This routine should NEVER be called
  return 0;
}
enum ComputationStatusCode gb_emitter::calc_gb(int degree)
{
  // This is when we ship off the elements in this degree.  This should NEVER
  // be called if elements of lower degree have not been sent.
  if (this_degree != degree)
    {
      start_degree(degree);
    }
  for (;;)
    {
      if (system_interrupted()) return COMP_INTERRUPTED;
      if (n_i >= n_gens) return COMP_DONE;
      if (g != NULL)
        {
          ring_elem denom;
          gbvector *v = originalR->translate_gbvector_from_vec(
              gens->rows(), (*gens)[these[n_i]], denom);
          g->receive_generator(v, these[n_i], denom);
        }
      n_i++;
      n_left--;
    }
}
enum ComputationStatusCode gb_emitter::calc_gens(int degree)
{
  return calc_gb(degree);
}

int gb_emitter::start_degree(int deg)
{
  this_degree = deg;
  n_gens = 0;
  n_i = 0;
  for (int i = 0; i < gens->n_cols(); i++)
    {
      if (gens->cols()->primary_degree(i) == this_degree) these[n_gens++] = i;
    }
  return n_gens;
}

void gb_emitter::flush()
{
  n_left -= (n_gens - n_i);
  n_gens = 0;
  n_i = 0;
}

bool gb_emitter::is_done() { return (n_left == 0); }
void gb_emitter::stats() const {}
void gb_emitter::text_out(buffer &o) const {}
typedef gb_node *gb_node_ptr;

void gbres_comp::setup(const Matrix *m, int length, int origsyz, int strategy)
{
  int i;
  originalR = m->get_ring()->cast_to_PolynomialRing();
  if (originalR == NULL) assert(0);
  GR = originalR->get_gb_ring();
  mi_stash = new stash("res mi nodes", sizeof(Nmi_node));

  FreeModule *Fsyz = originalR->make_Schreyer_FreeModule();
  if (length <= 0)
    {
      ERROR("resolution length must be at least 1");
      length = 1;
    }

  // If origsyz, and length>1, create Fsyz as a Schreyer free
  // if origsyz is smaller, truncate this module...

  if (length > 1 && origsyz > 0)
    {
      if (origsyz > m->n_cols()) origsyz = m->n_cols();
      int *one = originalR->getMonoid()->make_one();
      const int *mon;
      for (i = 0; i < origsyz; i++)
        {
          if ((*m)[i] == NULL)
            mon = one;
          else
            {
              Nterm *t = (*m)[i]->coeff;
              mon = t->monom;
            }
          Fsyz->append_schreyer(m->cols()->degree(i), mon, i);
        }
      originalR->getMonoid()->remove(one);
    }

  lo_degree = m->cols()->lowest_primary_degree();
  last_completed_degree = lo_degree - 1;

  n_nodes = length + 1;
  nodes = newarray(gb_node_ptr, n_nodes);

  nodes[0] = new gb_emitter(m);
  nodes[1] =
      new gb2_comp(Fsyz, mi_stash, nodes[0], lo_degree, origsyz, 1, strategy);
  nodes[0]->set_output(nodes[1]);
  if (n_nodes == 2)
    {
      // Don't compute syzygies at all.
      nodes[1]->set_output(NULL);
    }
  else if (n_nodes >= 3)
    {
      // Compute a resolution to length 'length', with last being
      // a gb node.
      int deg = lo_degree + 1;
      if (origsyz > 0) deg--;
      for (i = 2; i < n_nodes - 1; i++)
        {
          FreeModule *F = originalR->make_Schreyer_FreeModule();
          nodes[i] =
              new gb2_comp(F, mi_stash, nodes[i - 1], deg++, -1, i, strategy);
          nodes[i - 1]->set_output(nodes[i]);
        }
      FreeModule *F = originalR->make_Schreyer_FreeModule();
      nodes[n_nodes - 1] = new gb2_comp(
          F, mi_stash, nodes[n_nodes - 2], deg++, 0, n_nodes - 1, strategy);
      nodes[n_nodes - 1]->set_output(NULL);
    }
  strategy_flags = strategy;
}

gbres_comp::gbres_comp(const Matrix *m, int length, int origsyz, int strategy)
{
  setup(m, length, origsyz, strategy);
}

gbres_comp::gbres_comp(const Matrix *m,
                       int length,
                       int origsyz,
                       const RingElement * /*hf*/,
                       int strategy)
{
  // MES: check homogeneity
  setup(m, length, origsyz, strategy);
  // nodes[0]->set_HF(hf);
}

gbres_comp::~gbres_comp()
{
  for (int i = 0; i < n_nodes; i++)
    {
      nodes[i]->set_output(NULL);
      delete nodes[i];
    }

  delete mi_stash;
}

//---- state machine (roughly) for the computation ----

bool gbres_comp::stop_conditions_ok()
{
  if (stop_.length_limit != 0 && stop_.length_limit->len > 0)
    {
      ERROR("cannot change length of resolution using this algorithm");
      return false;
    }
  return true;
}

void gbres_comp::start_computation()
{
  //  int old_compare_type = compare_type;
  //  compare_type = (strategy_flags >> 10);
  //  if (M2_gbTrace >= 4)
  //    {
  //      buffer o;
  //      o << "compare=" << compare_type << newline;
  //      emit(o.str());
  //    }
  for (int i = lo_degree; !is_done(); i++)
    {
      if (stop_.stop_after_degree && stop_.degree_limit->array[0] < i)
        {
          set_status(COMP_DONE_DEGREE_LIMIT);
          //      compare_type = old_compare_type;
          return;
        }
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << "{" << i << "}";
          emit(o.str());
        }
      enum ComputationStatusCode ret =
          nodes[n_nodes - 1]->calc_gens(i + n_nodes - 3);
      if (ret != COMP_DONE)
        {
          set_status(ret);
          //      compare_type = old_compare_type;
          return;
        }
      last_completed_degree = i;
    }
  //  compare_type = old_compare_type;
  set_status(COMP_DONE);
}

bool gbres_comp::is_done()
{
  for (int i = 0; i < n_nodes; i++)
    if (!nodes[i]->is_done()) return false;
  return true;
}

int gbres_comp::complete_thru_degree() const { return last_completed_degree; }
//--- Reduction --------------------------

Matrix *gbres_comp::reduce(const Matrix *m, Matrix *&lift)
{
  const FreeModule *F = nodes[0]->output_free_module();
  if (m->n_rows() != F->rank())
    {
      ERROR("expected matrices to have same number of rows");
      return 0;
    }
  MatrixConstructor mat_red(m->rows(), m->cols(), m->degree_shift());
  MatrixConstructor mat_lift(nodes[1]->output_free_module(), m->cols(), 0);

  for (int i = 0; i < m->n_cols(); i++)
    {
      const FreeModule *Fsyz = nodes[1]->output_free_module();

      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);
      gbvector *fsyz = GR->gbvector_zero();

      nodes[1]->reduce(f, fsyz);

      vec fv = originalR->translate_gbvector_to_vec_denom(F, f, denom);
      GR->get_flattened_coefficients()->negate_to(denom);
      vec fsyzv = originalR->translate_gbvector_to_vec_denom(Fsyz, fsyz, denom);
      mat_red.set_column(i, fv);
      mat_lift.set_column(i, fsyzv);
    }
  lift = mat_lift.to_matrix();
  return mat_red.to_matrix();
}

//////////////////////////////////
// Obtaining matrices as output //
//////////////////////////////////

const FreeModule *gbres_comp::free_module(int level) const
{
  if (level >= 0 && level <= n_nodes - 1)
    return const_cast<FreeModule *>(nodes[level]->output_free_module());
  return nodes[0]->output_free_module()->get_ring()->make_FreeModule();
}
const Matrix *gbres_comp::min_gens_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix::zero(free_module(level - 1), free_module(level));
  return nodes[level]->min_gens_matrix();
}
const Matrix *gbres_comp::get_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix::zero(free_module(level - 1), free_module(level));
  return nodes[level]->get_matrix();
}

const Matrix *gbres_comp::initial_matrix(int n, int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix::zero(free_module(level - 1), free_module(level));
  return nodes[level]->initial_matrix(n);
}

const Matrix *gbres_comp::gb_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix::zero(free_module(level - 1), free_module(level));
  return nodes[level]->gb_matrix();
}

const Matrix *gbres_comp::change_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix::zero(free_module(level - 1), free_module(level));
  return nodes[level]->change_matrix();
}

void gbres_comp::stats() const
{
  emit_line(
      "  #gb #pair #comp     m     z     o     u #hilb  #gcd #mons  #chg");
  for (int i = 1; i < n_nodes; i++) nodes[i]->stats();
}
void gbres_comp::text_out(buffer &o) const
{
  o << "  #gb #pair #comp     m     z     o     u #hilb  #gcd #mons  #chg";
  for (int i = 1; i < n_nodes; i++) nodes[i]->text_out(o);
}

M2_arrayint gbres_comp::betti_minimal() const
// Negative numbers represent upper bounds
{
  int lev, i, d;
  int lo = nodes[0]->output_free_module()->lowest_primary_degree();
  if (n_nodes >= 2)
    {
      int lo1 = nodes[1]->output_free_module()->lowest_primary_degree() - 1;
      if (lo1 < lo) lo = lo1;
    }
  if (n_nodes >= 3)
    {
      int lo2 = nodes[2]->output_free_module()->lowest_primary_degree() - 2;
      if (lo2 < lo) lo = lo2;
    }
  int hi = lo;
  int len = 1;

  // Set the hi degree, and len
  for (lev = 0; lev < n_nodes; lev++)
    {
      const FreeModule *F = free_module(lev);
      if (F->rank() > 0) len = lev;

      for (i = 0; i < F->rank(); i++)
        {
          d = F->primary_degree(i) - lev;
          if (d > hi) hi = d;
        }
    }

  int *bettis;
  betti_init(lo, hi, len, bettis);

  for (lev = 0; lev <= len; lev++)
    {
      const FreeModule *F = free_module(lev);

      for (i = 0; i < F->rank(); i++)
        {
          d = F->primary_degree(i) - lev - lo;
          bettis[lev + (len + 1) * d]++;
        }
    }
  M2_arrayint result = betti_make(lo, hi, len, bettis);
  freemem(bettis);
  return result;
}

M2_arrayint gbres_comp::get_betti(int type) const
// Only type = 0 (minimal) is supported by this type.
// Should we do the other types as well?
// type is documented under rawResolutionBetti, in engine.h
{
  if (type == 0) return betti_minimal();
  if (type == 4)
    {
      ERROR(
          "cannot use Minimize=>true unless res(...,FastNonminimal=>true) was "
          "used");
      return 0;
    }

  ERROR("received unsupported betti type for this algorithm");
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
