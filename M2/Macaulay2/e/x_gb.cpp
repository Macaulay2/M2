// Copyright 2002 Michael E. Stillman

#include "engine.h"
#include "hilb.hpp"
#include "comp_gb.hpp"

const RingElementOrNull * IM2_Matrix_Hilbert(const Matrix *M)
  /* This routine computes the numerator of the Hilbert series
     for coker leadterms(M), using the degrees of the rows of M. 
     NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */
{
  return hilb_comp::hilbertNumerator(M);
}

ComputationOrNull *IM2_GB_make(const Matrix *m,
			       M2_bool collect_syz,
			       int n_rows_to_keep,
			       M2_arrayint gb_degrees,
			       M2_bool use_max_degree,
			       int max_degree,
			       int algorithm,
			       int strategy) /* drg: connected rawGB */
{
  // Choose the correct computation here.
  return GBComputation::choose_gb(
				  m,
				  collect_syz,
				  n_rows_to_keep,
				  gb_degrees,
				  use_max_degree,
				  max_degree,
				  algorithm,
				  strategy);
}

ComputationOrNull *IM2_res_make(
           const Matrix *m,
	   M2_bool resolve_cokernel,
	   int max_level,
	   M2_bool use_max_slanted_degree,
	   int max_slanted_degree,
	   int algorithm,
	   int strategy
	   )
{
  // Choose the correct computation here.
#warning "implement choose_res"
#if 0
  return Computation::choose_res(m,
				 resolve_cokernel,
				 max_level,
				 use_max_slanted_degree,
				 max_slanted_degree,
				 algorithm,
				 strategy);
#endif
  ERROR("resolutions not yet re-implemented");
  return 0;
}

ComputationOrNull *
IM2_GB_set_hilbert_function(Computation *C,
			    const RingElement *h)
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->set_hilbert_function(h);
  ERROR("computation type unknown or not implemented");
  return 0;
}

ComputationOrNull *
IM2_GB_force(const Matrix *m,
	     const Matrix *gb,
	     const Matrix *change)
{
  return GBComputation::force(m,gb,change);
}

ComputationOrNull* 
IM2_Computation_set_stop(Computation *G,
		M2_bool always_stop,
		M2_bool stop_after_degree,
		M2_arrayint degree_limit,
		int basis_element_limit,
		int syzygy_limit,
		int pair_limit,
		int codim_limit,
		int subring_limit,
		M2_bool just_min_gens,
		M2_arrayint length_limit) /* TODO */
  /* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
  /* Res: SortStrategy, 0, 1, 2, 3 ?? */
{
  return  G->set_stop_conditions(always_stop,
				 stop_after_degree,
				 degree_limit,
				 basis_element_limit,
				 syzygy_limit,
				 pair_limit,
				 codim_limit,
				 subring_limit,
				 just_min_gens,
				 length_limit);
}

void rawStartComputation(Computation *C)
  /* start or continue the computation */
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    G->start_computation();
}

enum ComputationStatusCode rawStatus1(Computation *C)
{
  return C->status();
}

int rawStatus2(Computation *C)
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->gb_complete_thru_degree();
  ERROR("computation type unknown or not implemented");
  return 0;
}

const MatrixOrNull *rawGBGetMatrix(Computation *C)
  /* Get the minimal, auto-reduced GB of a GB computation.
     Each call to this will produce a different raw matrix */
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->get_gb();
  ERROR("computation type unknown or not implemented");
  return 0;
}

const MatrixOrNull *rawGBMinimalGenerators(Computation *C)
  /* Yields a matrix whose columns form a minimal generating set
     for the ideal or submodule, as computed so far.  In the
     inhomogeneous case, this yields a generating set which is
     sometimes smaller than the entire Groebner basis. */
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->get_mingens();
  ERROR("computation type unknown or not implemented");
  return 0;
}

const MatrixOrNull *rawGBChangeOfBasis(Computation *C)
  /* Yields the change of basis matrix from the Groebner basis to
     the original generators, at least if n_rows_to_keep was set
     when creating the GB computation.  This matrix, after the 
     computation has run to completion, should satisfy:
     (original matrix) = (GB matrix) * (change of basis matrix). */
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->get_change();
  ERROR("computation type unknown or not implemented");
  return 0;
}

const MatrixOrNull *rawGBSyzygies(Computation *C)
  /* Yields a matrix containing the syzygies computed so far
     via the GB computation C, assuming that 'collect_syz' was
     set when the computation was created.  If 'n_rows_to_keep' was
     set to a non-negative integer, then only that many rows of each
     syzygy are kept. */
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->get_syzygies();
  ERROR("computation type unknown or not implemented");
  return 0;
}

const MatrixOrNull *
rawGBMatrixRemainder(Computation *C, 
		     const Matrix *m)
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->matrix_remainder(m);
  ERROR("computation type unknown or not implemented");
  return 0;
}

void IM2_GB_matrix_lift(Computation *C,
			const Matrix *m,
			MatrixOrNull **result_remainder,
			MatrixOrNull **result_quotient
			)
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    G->matrix_lift(m, result_remainder, result_quotient);
  ERROR("computation type unknown or not implemented");
}

int 
IM2_GB_contains(Computation *C, 
		const Matrix *m)
{
  GBComputation *G = C->cast_to_GBComputation();
  if (G != 0)
    return G->contains(m);
  ERROR("computation type unknown or not implemented");
  return -2;
}

const MatrixOrNull *
rawResolutionGetMatrix(Computation *C, 
		       int level)
{
#warning write this routine
#if 0
  return G->get_matrix(level);
#endif
  ERROR("resolutions need to be re-implemented");
  return 0;
}

const FreeModuleOrNull *
rawResolutionGetFree(Computation *C, 
		     int level)
{
#warning write this routine
#if 0
  return G->get_free(level);
#endif
  ERROR("resolutions need to be re-implemented");
  return 0;
}

int IM2_Resolution_status(Computation *C,
			  int * complete_up_through_this_degree,
			  int * complete_up_through_this_level)
{
#warning "IM2_Resolution_status to be written"
  ERROR("not re-implemented yet");
  return -1;
}


int 
IM2_Resolution_status_level(Computation *C, 
		    int level, 
		    M2_bool minimize,
		    int * complete_up_through_this_degree)
  /* Same return values */
{
#warning write this routine
#if 0
  return G->status_level(level, 
			 minimize,
			 complete_up_through_this_degree);
#endif
  ERROR("resolutions need to be re-implemented");
  return 0;
}

const M2_arrayint_OrNull
rawResolutionBetti(Computation *C,
	     int type)
  /* 0: minimal betti numbers,
     1:
     2:
     3:
  */
{
#warning write this routine
#if 0
  return G->betti(type);
#endif
  ERROR("resolutions need to be re-implemented");
  return 0;
}

const M2_string
IM2_GB_to_string(Computation *C)
  /* TODO */
{
  buffer o;
  o << "-- a raw Groebner basis --";
    // C->text_out(o);
  return o.to_string();
}

int IM2_GB_hash(const Computation *C)
{
  return C->get_hash_value();
}

#if 0
#include "gb_comp.hpp"

#include "gbbinom.hpp"
#include "comp.hpp"
#include "hilb.hpp"

#include "gb2.hpp"
#include "res.hpp"
#include "res2.hpp"

#include "ringmap.hpp"
#include "sagbi.hpp"

#include "lattice.hpp"
#include "LLL.hpp"

#include "Eschreyer.hpp"

extern int gbTrace;
extern ZZ *globalZZ;

#if 0


void i_EGB()
{
}
gb_comp * make_EGB_comp(const Matrix &m, bool dosyz, int nsyz, int strategy)
{
  return 0;
}
#endif

void cmd_NGB_tracing(object &on)
{
  int n = on->int_of();
  gStack.insert(make_object_int(gbTrace));
  gbTrace = n;
}


void cmd_gb_make(object &om, 
		 object &odosyz,
		 object &osyz,
		 object &ostrategy)
{
  Matrix m = om->cast_to_Matrix();
  int dosyz = odosyz->int_of();
  int nsyz = osyz->int_of();
  int strategy = ostrategy->int_of();

  gStack.insert(gb_comp::make(m,dosyz,nsyz,strategy));
}

void cmd_gb_make1(object &om, 
		  object &odosyz,
		  object &osyz,
		  object &ohf,
		  object &ostrategy)
{
  int dosyz = odosyz->int_of();
  Matrix m = om->cast_to_Matrix();
  int nsyz = osyz->int_of();
  int strategy = ostrategy->int_of();
  RingElement hf = ohf->cast_to_RingElement();

  gStack.insert(gb_comp::make(m,dosyz,nsyz,hf,strategy));
}


void cmd_NGB_force(object &ogens, object &ogb, object &ochange)
{
  Matrix gens = ogens->cast_to_Matrix();
  Matrix m    = ogb->cast_to_Matrix();
  Matrix change = ochange->cast_to_Matrix();
  Matrix syz(change.rows());
  object_element *p = gb_comp::force(gens,m,change,syz);
  gStack.insert(p);
}

void cmd_NGB_force1(object &ogens, object &ogb, object &ochange, object &osyz)
{
  Matrix gens   = ogens->cast_to_Matrix();
  Matrix m      = ogb->cast_to_Matrix();
  Matrix change = ochange->cast_to_Matrix();
  Matrix syz    = osyz->cast_to_Matrix();
  object_element *p = gb_comp::force(gens,m,change,syz);
  gStack.insert(p);
}

void cmd_NGB_calc(object &op, object &odeg, object &oargs)
{
  gb_comp *p = op->cast_to_gb_comp();
  intarray *adeg = odeg->intarray_of();
  const int *deg;
  if (adeg->length() == 0)
    deg = NULL;
  else
    deg = adeg->raw();
  intarray *args = oargs->intarray_of();
  gStack.insert(make_object_int(p->calc(deg, *args)));
}

void cmd_NGB_stats(object &op)
{
  gb_comp *p = op->cast_to_gb_comp();
  p->stats();
}
void cmd_NGB_getgb(object &op)
{
  gb_comp *p = op->cast_to_gb_comp();
  gStack.insert(p->gb_matrix());
}
void cmd_NGB_getchange(object &op)
{
  gb_comp *p = op->cast_to_gb_comp();
  gStack.insert(p->change_matrix());
}
void cmd_NGB_getsyz(object &op)
{
  gb_comp *p = op->cast_to_gb_comp();
  gStack.insert(p->syz_matrix());
}
void cmd_NGB_getmingens(object &op)
{
  gb_comp *p = op->cast_to_gb_comp();
  gStack.insert(p->min_gens_matrix());
}
void cmd_NGB_in(object &op, object &on)
{
  gb_comp *p = op->cast_to_gb_comp();
  gStack.insert(p->initial_matrix(on->int_of()));
}

void cmd_NGB_reduce(object &op, object &om)
{
  gb_comp *p = op->cast_to_gb_comp();
  Matrix m = om->cast_to_Matrix();
  Matrix lift;
  Matrix red = p->reduce(m, lift);
  gStack.insert(red);
  gStack.insert(lift);
}

void cmd_NGB_vecreduce(object &op, object &ov)
{
  gb_comp *p = op->cast_to_gb_comp();
  Vector v = ov->cast_to_Vector();
  Vector lift;
  Vector red = p->reduce(v, lift);
  gStack.insert(red);
  gStack.insert(lift);
}
void cmd_gb_issubset(object &om, object &op)
{
  Matrix m = om->cast_to_Matrix();
  gb_comp *p = op->cast_to_gb_comp();
  int result = p->contains(m);
  gStack.insert(make_object_int(result));
}

void cmd_gb_isequal(object &op, object &oq)
{
  gb_comp *p = op->cast_to_gb_comp();
  gb_comp *q = oq->cast_to_gb_comp();
  int result = p->is_equal(q);
  gStack.insert(make_object_int(result));
}

void cmd_comp_calc(object &ocomp, object &onsteps)
{
  int nsteps = onsteps->int_of();
  computation *c = ocomp->cast_to_computation();
  gStack.insert(make_object_int(c->calc(nsteps)));
}
void cmd_comp_calc2(object &ocomp, 
		    object &onsteps, 
		    object &odiscard,
		    object &orows,
		    object &ocols
		    )
{
  computation *c = ocomp->cast_to_computation();
  int nsteps = onsteps->int_of();
  int discard = odiscard->int_of();
  intarray *rows = orows->intarray_of();
  intarray *cols = ocols->intarray_of();
  if (discard) c->discard();
  if (rows->length() > 0 && cols->length() > 0)
    {
      c->set_next_minor(rows->raw(), cols->raw());
    }
  gStack.insert(make_object_int(c->calc(nsteps)));
}

void cmd_hilb_make(object &oR, object &oM)
{
  Ring *R = oR->cast_to_Ring();
  PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == NULL)
    {
      gError << "Hilbert function: result ring must be a polynomial ring";
      return;
    }
  Matrix M = oM->cast_to_Matrix();
  if (M.get_ring()->degree_monoid() != P->Nmonoms())
    {
      gError << "Hilbert function: result monoid must be degree monoid";
      return;
    }
  gStack.insert(new hilb_comp(P, M));
}

void cmd_hilb1_make(object &oR, object &omi)
{
  Ring *R = oR->cast_to_Ring();
  PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == NULL)
    {
      gError << "Hilbert function: result ring must be a polynomial ring";
      return;
    }
  MonomialIdeal mi = omi->cast_to_MonomialIdeal();
  if (mi.get_ring()->degree_monoid() != P->Nmonoms())
    {
      gError << "Hilbert function: result monoid must be degree monoid";
      return;
    }
  gStack.insert(new hilb_comp(P, mi));
}

void cmd_hilb_calc(object &oh, object &on)
{
  hilb_comp *h = oh->cast_to_hilb_comp();
  int result = h->calc(on->int_of());
  gStack.insert(make_object_int(result));
}

void cmd_hilb_stats(object &oh)
{
  const hilb_comp *h = oh->cast_to_hilb_comp();
  h->stats();
}

void cmd_hilb_value(object &oh)
{
  hilb_comp *h = oh->cast_to_hilb_comp();
  if (!h->is_done())
    {
      gError << "Hilbert function value is not yet computed";
      return;
    }
  gStack.insert(h->value());
}

void cmd_res(object &om, object &oalg, object &olength, object &odegree, object &ostrategy)
{
  Matrix m = om->cast_to_Matrix();
  int alg = oalg->int_of();
  int maxlev = olength->int_of();
  int strategy = ostrategy->int_of();
  intarray *deg = odegree->intarray_of();
  bool usedeg;
  int maxdeg, origsyz;
  res2_comp *p0;
  res_comp *p1;
  gbres_comp *p2;
  if (deg->length() > 0)
    {
      usedeg = (deg->length() > 0);
      maxdeg = (*deg)[0];
    }
  else
    {
      usedeg = 0;
      maxdeg = 0;
    }
  switch (alg) {
  case 0:
    p0 = new res2_comp(m, maxlev, usedeg, maxdeg, strategy);
    gStack.insert(p0);
    break;
  case 1:
    p1 = new res_comp(m, maxlev, strategy);
    gStack.insert(p1);
    break;
  case 2:
    origsyz = m.n_cols();
    p2 = new gbres_comp(m, maxlev+1, origsyz, strategy);
    gStack.insert(p2);
    break;
  case 3:
    origsyz = m.n_cols();
    p2 = new gbres_comp(m, maxlev+1, origsyz, strategy | USE_HILB);
    gStack.insert(p2);
    break;
  default:
    gError << "Unknown algorithm for computing resolutions";
    break;
  }
}

void cmd_binomialGB_make(object &oR, object &owts, object &orevlex, object &ooptions)
{
  const Ring *R = oR->cast_to_Ring();
  intarray *awts = owts->intarray_of();
  int *wts = NULL;
  if (awts != NULL && awts->length() == R->n_vars()) wts = awts->raw();
  bool revlex = (orevlex->int_of() != 0);
  unsigned int options = ooptions->int_of();
  gStack.insert(new binomialGB_comp(R,wts,revlex,options));
}

void cmd_binomialGB_addgens(object &og, object &om)
{
  Matrix m = om->cast_to_Matrix();
  gb_comp *g = og->cast_to_gb_comp();
  binomialGB_comp *gb = g->cast_to_binomialGB_comp();
  if (gb == NULL)
    {
      gError << "expected binomialGB computation";
      return;
    }
  gb->add_generators(m);
}
void cmd_binomialGB_enlarge(object &og, object &oR, object &owts)
{
  const Ring *R = oR->cast_to_Ring();
  intarray *awts = owts->intarray_of();
  int *wts = NULL;
  if (awts != NULL) wts = awts->raw();
  gb_comp *g = og->cast_to_gb_comp();
  binomialGB_comp *gb = g->cast_to_binomialGB_comp();
  if (gb == NULL)
    {
      gError << "expected binomialGB computation";
      return;
    }
  gb->enlarge(R,wts);
}
void cmd_binomialGB_subring(object &og)
{
  gb_comp *g = og->cast_to_gb_comp();
  binomialGB_comp *gb = g->cast_to_binomialGB_comp();
  if (gb == NULL)
    {
      gError << "expected binomialGB computation";
      return;
    }
  gStack.insert(gb->subring());
}
void cmd_binomialGB_subringGB(object &og)
{
  gb_comp *g = og->cast_to_gb_comp();
  binomialGB_comp *gb = g->cast_to_binomialGB_comp();
  if (gb == NULL)
    {
      gError << "expected binomialGB computation";
      return;
    }
  gStack.insert(gb->subringGB());
}

//////////////////////////////////////////////////
// Sagbi routines (written with Harrison Tsai) ///
//////////////////////////////////////////////////

void cmd_sagbi_make(object &om)
{
  Matrix m = om->cast_to_Matrix();
  sagbi_comp *g = new sagbi_comp(m);
  gStack.insert(g);
}
void cmd_sagbi_subduction(object &om, object &oF, object &og)
{
  Matrix m = om->cast_to_Matrix();
  const RingMap *F = oF->cast_to_RingMap();
  gb_comp *g = og->cast_to_gb_comp();

  Matrix result = sagbi::subduct(m, F, g);
  gStack.insert(result);
}

void cmd_smith_make(object &om, object &o1, object &o2)
{
  Matrix m = om->cast_to_Matrix();
  bool do_rowchange = (o1->int_of() != 0);
  bool do_colchange = (o2->int_of() != 0);
  // Error check here: ring of 'm' must be a computatable PID
  if (!m.get_ring()->is_pid())
    {
      gError << "Smith normal form requires the base ring to be a computable P.I.D.";
      return;
    }
  MatrixComputation *sm = new MatrixComputation(m,do_rowchange,do_colchange);
  gStack.insert(sm);
}

void cmd_smith_calc(object &os, object &on)
{
  MatrixComputation *sm = os->cast_to_MatrixComputation();
  int n = on->int_of();
  int result = sm->calc(n);
  gStack.insert(make_object_int(result));
}

void cmd_smith_rowchange(object &os)
{
  MatrixComputation *sm = os->cast_to_MatrixComputation();
  gStack.insert(sm->getRowChangeOfBasisMatrix());
}

void cmd_smith_colchange(object &os)
{
  MatrixComputation *sm = os->cast_to_MatrixComputation();
  gStack.insert(sm->getColumnChangeOfBasisMatrix());
}

void cmd_smith_matrix(object &os)
{
  MatrixComputation *sm = os->cast_to_MatrixComputation();
  gStack.insert(sm->getResultMatrix());
}

void cmd_smith_status(object &os)
{
  MatrixComputation *sm = os->cast_to_MatrixComputation();
  int result = sm->getStatus();
  gStack.insert(make_object_int(result));
}

void cmd_LLL_init(object &o1, object &o2, object &o3)
{
  Matrix m = o1->cast_to_Matrix();
  RingElement threshold = o2->cast_to_RingElement();
  bool useChangeOfBasis = (o3->int_of() != 0);
  SparseMutableMatrix *A;
  SparseMutableMatrix *LLLstate;
  if (LLLoperations::initializeLLL(m,threshold,useChangeOfBasis,A,LLLstate))
    {
      gStack.insert(A);
      gStack.insert(LLLstate);
    }
}

void cmd_LLL_init0(object &o1, object &o2)
{
  SparseMutableMatrix *A = o1->cast_to_SparseMutableMatrix();
  RingElement threshold = o2->cast_to_RingElement();
  SparseMutableMatrix *LLLstate;
  if (LLLoperations::initializeLLL(A,threshold,LLLstate))
    gStack.insert(LLLstate);
}

void cmd_LLL_calc(object &o1, object &o2, object &o3)
{
  SparseMutableMatrix *A = o1->cast_to_SparseMutableMatrix();
  SparseMutableMatrix *LLLstate = o2->cast_to_SparseMutableMatrix();
  int nsteps = o3->int_of();
  int ret = LLLoperations::doLLL(A,LLLstate,nsteps);
  gStack.insert(make_object_int(ret));
}

static void cmd_kerGB_make(object &om)
{
  Matrix m = om->cast_to_Matrix();
  const PolynomialRing *R = m.get_ring()->cast_to_PolynomialRing();
  if (R == 0)
    {
      gError << "expected polynomial ring";
      return;
    }
  GBKernelComputation *g = new GBKernelComputation(m);
  gStack.insert(g);
}
static void cmd_kerGB_calc(object &o1)
{
  GBKernelComputation *g = o1->cast_to_GBKernelComputation();
  int ret = g->calc();
  gStack.insert(make_object_int(ret));
}
static void cmd_kerGB_getsyz(object &o1)
{
  GBKernelComputation *g = o1->cast_to_GBKernelComputation();
  gStack.insert(g->get_syzygies());
}

static void cmd_FFgausselim(object &o1)
{
  SparseMutableMatrix *A = o1->cast_to_SparseMutableMatrix();
  object_intarray * result = new object_intarray;
  FF_LUComputation::DO(A,*result->intarray_of());
  gStack.insert(result);
}

void i_NGB_cmds(void)
{
  // New NGB commands
  install(gggb, cmd_NGB_force, TY_MATRIX, TY_MATRIX, TY_MATRIX);
  install(gggb, cmd_NGB_force1, TY_MATRIX, TY_MATRIX, TY_MATRIX, TY_MATRIX);
  install(gggb, cmd_gb_make, TY_MATRIX, TY_INT, TY_INT, TY_INT);
  install(gggb, cmd_gb_make1, TY_MATRIX, TY_INT, TY_INT, TY_RING_ELEM, TY_INT);
//  install(gggb, cmd_NGB_make1, TY_MATRIX, TY_INT, TY_INT, TY_RING_ELEM, TY_INT);
  install(ggcalc, cmd_NGB_calc, TY_GB_COMP, TY_INTARRAY, TY_INTARRAY);

  install(ggstats, cmd_NGB_stats, TY_GB_COMP);
  install(gggetmingens, cmd_NGB_getmingens, TY_GB_COMP);
  install(gggetgb, cmd_NGB_getgb, TY_GB_COMP);
  install(gggetchange, cmd_NGB_getchange, TY_GB_COMP);
  install(gggetsyz, cmd_NGB_getsyz, TY_GB_COMP);
  install(gginitial, cmd_NGB_in, TY_GB_COMP, TY_INT);

  install(ggreduce, cmd_NGB_reduce, TY_GB_COMP, TY_MATRIX);
  install(ggreduce, cmd_NGB_vecreduce, TY_GB_COMP, TY_VECTOR);

  install(ggissubset, cmd_gb_issubset, TY_MATRIX, TY_GB_COMP);
  install(ggisequal, cmd_gb_isequal, TY_GB_COMP, TY_GB_COMP);

  // Computation display/statistics
  install(ggtracing, cmd_NGB_tracing, TY_INT);

  // binomial GB's
  install(ggbinomialGB, cmd_binomialGB_make, TY_RING, TY_INTARRAY, TY_INT, TY_INT);
  install(ggbinomialGBaddgens, cmd_binomialGB_addgens, TY_GB_COMP, TY_MATRIX);
  install(ggbinomialGBenlarge, cmd_binomialGB_enlarge, TY_GB_COMP, TY_RING, TY_INTARRAY);
  install(gggetsubring, cmd_binomialGB_subring, TY_GB_COMP);
  install(gggetsubringGB, cmd_binomialGB_subringGB, TY_GB_COMP);

  // GB of kernels of GB's: requires Schreyer free module as source.
  install(ggker, cmd_kerGB_make, TY_MATRIX);
  install(ggcalc, cmd_kerGB_calc, TY_GBKernelComputation);
  install(gggetsyz, cmd_kerGB_getsyz, TY_GBKernelComputation);

  // Performing the computation (for dets/pfaffians)
  install(ggcalc, cmd_comp_calc, TY_COMP, TY_INT);
  install(ggcalc, cmd_comp_calc2, TY_COMP, TY_INT, TY_INT, TY_INTARRAY, TY_INTARRAY);

  // Hilbert functions
  install(gghilb, cmd_hilb_make, TY_RING, TY_MATRIX);
  install(gghilb, cmd_hilb1_make, TY_RING, TY_MONIDEAL);
  install(ggcalc, cmd_hilb_calc, TY_HILB_COMP, TY_INT);
  install(ggstats, cmd_hilb_stats, TY_HILB_COMP);
  install(gggetvalue, cmd_hilb_value, TY_HILB_COMP);

  // Resolutions
  install(ggres, cmd_res, TY_MATRIX, TY_INT, TY_INT, TY_INTARRAY, TY_INT);

  // SAGBI basis routines
  install(ggsubduction, cmd_sagbi_subduction, TY_MATRIX, TY_RING_MAP, TY_GB_COMP);
  install(ggsagbi, cmd_sagbi_make, TY_MATRIX);

  // Smith normal form
  install(ggSmithNormalForm, cmd_smith_make, TY_MATRIX, TY_INT, TY_INT);
  install(ggcalc, cmd_smith_calc, TY_MatrixComputation, TY_INT);
  install(gggetRowChange, cmd_smith_rowchange, TY_MatrixComputation);
  install(gggetColChange, cmd_smith_colchange, TY_MatrixComputation);
  install(gggetgb, cmd_smith_matrix, TY_MatrixComputation);
  install(ggstatus, cmd_smith_status, TY_MatrixComputation);

  // LLL operations
  install(ggLLLinit, cmd_LLL_init, TY_MATRIX, TY_RING_ELEM, TY_INT);
  install(ggLLLinit, cmd_LLL_init0, TY_SparseMutableMatrix, TY_RING_ELEM);
  install(ggLLLcalc, cmd_LLL_calc, TY_SparseMutableMatrix, TY_SparseMutableMatrix, TY_INT);

  // Fraction free Gaussian elimination
  install(ggFFgausselim, cmd_FFgausselim, TY_SparseMutableMatrix);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
