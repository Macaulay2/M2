// Copyright 1996 Michael E. Stillman

#include "interp.hpp"

#include "gb.hpp"
#include "gbinhom.hpp"
#include "gbbinom.hpp"
#include "gbZZ.hpp"
#include "comp.hpp"
#include "hilb.hpp"
#include "hermite.hpp"
#include "gauss.hpp"

#include "gb2.hpp"
#include "res.hpp"
#include "res2.hpp"

#include "ringmap.hpp"
#include "sagbi.hpp"

extern int comp_printlevel;
extern Z *ZZ;

void cmd_NGB_tracing(object &on)
{
  int n = on->int_of();
  gStack.insert(make_object_int(comp_printlevel));
  comp_printlevel = n;
}

void cmd_gb_make(object &om, 
		 object &odosyz,
		 object &osyz,
		 object &ostrategy)
{
  Matrix m = om->cast_to_Matrix();
  const Ring *R = m.Ring_of();
  int dosyz = odosyz->int_of();
  int nsyz = osyz->int_of();
  int strategy = ostrategy->int_of();

  // Now we dispatch according to the kind of computation we are
  // asked to do.
  
  if (R->is_field())
    gStack.insert(new GaussElimComputation(m, dosyz, nsyz));
  else if (R->is_Z()) // MES later: || R->is_pid())
    gStack.insert(new HermiteComputation(m, dosyz, nsyz));
  else if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if (R->is_graded() && m.is_homogeneous())
	{
	  if ((strategy & 3) == 1)
	    {
	      //gStack.insert(new NGB_comp(m, dosyz, nsyz));
	      gStack.insert(new GB_comp(m, dosyz, nsyz, strategy));
	    }
	  else if ((strategy & 3) == 2)
	    gStack.insert(new GBinhom_comp(m, dosyz, nsyz, strategy));
	  else 
	    gStack.insert(new GB_comp(m, dosyz, nsyz, strategy));
	}
      else
	gStack.insert(new GBinhom_comp(m, dosyz, nsyz, strategy));
    }
  else if (R->is_poly_ring() && R->Ncoeffs()->is_Z())
    {
      gStack.insert(new GBZZ_comp(m, dosyz, nsyz, strategy));
    }
  else if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      gError << "GB for polynomial rings over PID's not yet implemented";
    }
  else 
    gError << "cannot compute Groebner bases or syzygies over this ring";
}

void cmd_gb_make1(object &om, 
		  object &odosyz,
		  object &osyz,
		  object &ohf,
		  object &ostrategy)
{
  int dosyz = odosyz->int_of();
  if (dosyz)
    {
      cmd_gb_make(om,odosyz,osyz,ostrategy);
      return;
    }
  Matrix m = om->cast_to_Matrix();
  const Ring *R = m.Ring_of();
  int nsyz = osyz->int_of();
  int strategy = ostrategy->int_of();
  RingElement hf = ohf->cast_to_RingElement();

  // Now we dispatch according to the kind of computation we are
  // asked to do.
  
  if (R->is_field())
    gError << "GB for ring = field together with Hilbert function is not yet implemented";
  else if (R->is_Z()) // MES later: || R->is_pid())
    gError << "GB for Z, using Hilbert function, is not yet implemented";
  else if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if (R->is_graded() && m.is_homogeneous())
	{
	  gStack.insert(new GB_comp(m, dosyz, nsyz, hf, strategy));
	}
      else
	gError << "cannot use Hilbert function for an inhomogeneous GB";
    }
  else if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      gError << "GB for polynomial rings over PID's not yet implemented";
    }
  else 
    gError << "cannot compute Groebner bases or syzygies over this ring";
}

#if 0
void cmd_NGB_make(object &om, 
		 object &odosyz,
		 object &osyz,
		 object &obits)
{
  Matrix m = om->cast_to_Matrix();
  const Ring *R = m.Ring_of();
  int dosyz = odosyz->int_of();
  int bits = obits->int_of();
  if (bits <= 0) bits = -1;
  int nsyz = osyz->int_of();

  if (R == (Ring *) ZZ)
    gStack.insert(new HermiteComputation(m, dosyz, nsyz));
  else
    gStack.insert(new NGB_comp(m, dosyz, nsyz));
}
#endif
void cmd_NGB_make1(object &om, 
		  object &odosyz,
		  object &osyz,
		  object &ohf,
		  object &obits)
{
  Matrix m = om->cast_to_Matrix();
  int dosyz = odosyz->int_of();
  int bits = obits->int_of();
  if (bits <= 0) bits = -1;
  int nsyz = osyz->int_of();
  RingElement hf = ohf->cast_to_RingElement();
  // MES: check that the ring of hf is 'correct'
  GB_comp *p = new GB_comp(m, dosyz, nsyz, hf.get_value());
  gStack.insert(p);
}

object_element *make_forceGB(Matrix gens, Matrix gb, Matrix change, Matrix syz)
{
  const Ring *R = gens.Ring_of();
  if (R->is_poly_ring())
    {
      if (R->Ncoeffs()->is_field())
	return new GB_comp(gens, gb, change, syz);
      else if (R->Ncoeffs()->is_Z())
	return new GBZZ_comp(gens, gb, change, syz);
    }
  gError << "Cannot create the desired forced Groebner basis";
  return NULL;
}

void cmd_NGB_force(object &ogens, object &ogb, object &ochange)
{
  Matrix gens = ogens->cast_to_Matrix();
  Matrix m    = ogb->cast_to_Matrix();
  Matrix change = ochange->cast_to_Matrix();
  Matrix syz(change.rows());
  object_element *p = make_forceGB(gens,m,change,syz);
  gStack.insert(p);
}

void cmd_NGB_force1(object &ogens, object &ogb, object &ochange, object &osyz)
{
  Matrix gens   = ogens->cast_to_Matrix();
  Matrix m      = ogb->cast_to_Matrix();
  Matrix change = ochange->cast_to_Matrix();
  Matrix syz    = osyz->cast_to_Matrix();
  object_element *p = make_forceGB(gens,m,change,syz);
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
  if (M.Ring_of()->degree_monoid() != P->Nmonoms())
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
  if (mi.Ring_of()->degree_monoid() != P->Nmonoms())
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
}
