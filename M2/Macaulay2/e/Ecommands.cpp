// Copyright 1998 by Michael E. Stillman

#include "Eio.hpp"
#include "EZZp.hpp"
#include "Emonorder.hpp"
#include "Emonoid.hpp"
#include "Ering.hpp"
#include "Efreemod.hpp"
#include "Evector.hpp"
#include "Ematrix.hpp"
#include "Eringmap.hpp"
#if 0
#include "Emonideal.hpp"
#endif
#include "EGB.hpp"

#include "interp.hpp"
#include "Ehashtab.hpp"


EHashTable *EUniqueObjects = 0;
const EZZ *EZ;

template class array<EVector>;
void appendMonomialToIntarray(const EMonoid *M, const monomial *m, intarray &result)
{
  const int *exp = M->to_exponents(m);
  for (int i=0; i<M->n_vars(); i++)
    result.append(exp[i]);
}

intarray monomialToIntarray(const EMonoid *M, const monomial *m)
{
  intarray result;
  appendMonomialToIntarray(M,m,result);
  return result;
}

const monomial *monomialFromIntarray(const EMonoid *D, const intarray &mapdeg)
{
  if (mapdeg.length() == 0)
    return D->clone(D->one());
  if (mapdeg.length() != D->n_vars())
    {
      gError << "monomial vector has wrong length";
      return 0;
    }
  return D->monomial_from_exponents(mapdeg.raw());
}

static void cmd_EHashTable_stats()
{
  EUniqueObjects->showshape();
}


/////////////////////////////
// Monomial Order Routines //
/////////////////////////////
static void cmd_EMO_init()
{
  EMonomialOrder *mo = EMonomialOrder::make();
  gStack.insert(mo);
}
static void cmd_EMO_clone(object &o1)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  gStack.insert(mo->clone());
}
static void cmd_EMO_revlex(object &o1, object &o2, object &o3)
{
  int nvars = o1->int_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->revlex(nvars, isgroup);
}
static void cmd_EMO_lex(object &o1, object &o2, object &o3)
{
  int nvars = o1->int_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->lex(nvars,isgroup);
}
static void cmd_EMO_revlexWeights(object &o1, object &o2, object &o3)
{
  intarray *a = o1->intarray_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->revlexWeights(a->length(),a->raw(),isgroup);
}

static void cmd_EMO_component(object &o1)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  mo->component();
}
static void cmd_EMO_weightFunction(object &o1, object &o2)
{
  intarray *a = o1->intarray_of();
  EMonomialOrder *mo = o2->cast_to_EMonomialOrder();
  mo->weightFunction(a->length(),a->raw());
}
static void cmd_EMO_product(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  EMonomialOrder *mo2 = o2->cast_to_EMonomialOrder();
  mo->product(mo2);
}
static void cmd_EMO_NClex(object &o1, object &o2)
{
  int nvars = o1->int_of();
  EMonomialOrder *mo = o2->cast_to_EMonomialOrder();
  mo->NClex(nvars);
}

/////////////////////////////
// Monoid Routines //////////
/////////////////////////////

static void cmd_EMonoid(object &o1, object &o2, object &o3)
{
  // Note: Monoids do not have a grading!  Also, monoids do not have elements
  // accessible via the front end... (except as arrays of integers).
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = mo->n_vars();
  intarray *printorder = o2->intarray_of();
  char **names = EMonoid::setNames(nvars,o3->string_of(),o3->length_of());

  if (printorder->length() != nvars)
    {
      gError << "expected " << nvars << " values, received " << printorder->length() << " instead";
      return;
    }
  EMonoid *M;
  if (mo->is_commutative())
    M = ECommMonoid::make(mo->clone(),
			  printorder->raw(),
			  (const char **) names);
  else
    M = ENCMonoid::make(mo->clone(),
			printorder->raw(),
			(const char **) names);

  // Now remove the array 'names'
  for (int i=0; i<nvars; i++)
    delete [] names[i];
  delete [] names;

  gStack.insert(M);
}

static void cmd_EMonoid_nvars(object &o1)
{
  EMonoid * M = o1->cast_to_EMonoid();
  gStack.insert(make_object_int(M->n_vars()));
}
static void cmd_EMonoid_monomialOrder(object &o1)
{
  EMonoid * M = o1->cast_to_EMonoid();
  // We clone the monomial order because otherwise the user could
  // possibly change the monomial order of an already constructed ring!
  gStack.insert(M->getMonomialOrder()->clone());
}

static void cmd_EMonoid_stats(object &o1)
{
  EMonoid * M = o1->cast_to_EMonoid();
  M->stats();
}

/////////////////////////////
// Polynomial Rings /////////
/////////////////////////////
static void cmd_EZZ()
{
  gStack.insert((object_element *)EZZ::make());
}

static void cmd_EZZp(object &r)
{
  // MES: check the size of p: If large, make a different coefficient ring.
  // This makes a polynomial ring with no variables...
  int p = r->int_of();
  if (p > 0)
    {
      ERing *R = EZZp::make(p);
      gStack.insert(R);
    }
  else
    gError << "ZZ/p: p must be positive";
}

static void cmd_EZZp_characteristic(object &o1)
{
  ERing *R = o1->cast_to_ERing();
  gStack.insert(make_object_int(R->characteristic()));
}

static bool grabDegreeInfo(int nvars, const EPolynomialRing *&ZD, int *&degrees)
{
  bool bad = false;
  intarray *degs;
  if (!gStack.in_bounds(0) || gStack[0]->type_id() != TY_INTARRAY)
    {
      gError << "ring construction: expected array of degrees";
      bad = true;
      degs = 0;
    }
  if (!gStack.in_bounds(1) || gStack[1]->type_id() != TY_ERing)
    {
      gError << "ring construction: expected degree ring";
      bad = true;
      ZD = 0;
    }
  if (bad)
    {
      gStack.poppem(2);
      return false;
    }
  
  degs = gStack[0]->intarray_of();
  const ERing *ZD1 = gStack[1]->cast_to_ERing();
  gStack.poppem(2);
  ZD = ZD1->toPolynomialRing();
  if (ZD == 0)
    {
      if (ZD1 == EZZ::make())
	ZD = ECommPolynomialRing::getTrivialRing();
      else
	{
	  gError << "expected ZZ or polynomial ring for degree ring";
	  return false;
	}
    }

  if (degs->length() != nvars * ZD->n_vars())
    {
      gError << "wrong length for degree vector";
      return false;
    }

  degrees = new int[degs->length()];
  for (int i=0; i<degs->length(); i++)
    degrees[i] = (*degs)[i];

  return true;
}
static void cmd_EPolynomialRing(object &o1, object &o2)
{
  ERing *K = o1->cast_to_ERing();
  EMonoid * M = o2->cast_to_EMonoid();
  EPolynomialRing *R;
  const EPolynomialRing *ZD;
  int * degrees;
  
  if (!grabDegreeInfo(M->n_vars(),ZD,degrees)) return;
  if (M->is_commutative())
    R = ECommPolynomialRing::make(K,M->toCommMonoid(),ZD,degrees);
  else
    R = ENCPolynomialRing::make(K,M->toNCMonoid(),ZD,degrees);

  gStack.insert(R);
}

static void cmd_EWeylAlgebra(object &o1, object &o2, object &o3, object &o4, object &o5)
{
  ERing *K = o1->cast_to_ERing();
  EMonoid * M = o2->cast_to_EMonoid();
  intarray *comms = o3->intarray_of();
  intarray *diffs = o4->intarray_of();
  if (diffs->length() != comms->length())
    {
      gError << "Weyl algebra: expected same length arrays";
      return;
    }
  int homog_var = o5->int_of();
  const EPolynomialRing *ZD;
  int * degrees;
  
  if (!grabDegreeInfo(M->n_vars(), ZD,degrees)) return;

  EWeylAlgebra *W;
  if (homog_var >= 0)
    W = EWeylAlgebra::make(K,M->toCommMonoid(),
			   ZD,
			   degrees,
			   diffs->length(),
			   diffs->raw(),
			   comms->raw(),homog_var);
  else
    W = EWeylAlgebra::make(K,M->toCommMonoid(),
			   ZD,
			   degrees,
			   diffs->length(),
			   diffs->raw(),
			   comms->raw());

  gStack.insert(W);
}

static void cmd_ESkewCommPolynomialRing(object &o1, object &o2, object &o3)
{
  ERing *K = o1->cast_to_ERing();
  EMonoid * M = o2->cast_to_EMonoid();
  intarray *skew = o3->intarray_of();

  const EPolynomialRing *ZD;
  int * degrees;
  
  if (!grabDegreeInfo(M->n_vars(),ZD,degrees)) return;

  ESkewCommPolynomialRing *R = ESkewCommPolynomialRing::make(
          K,M->toCommMonoid(),
	  ZD,
	  degrees,
          skew->length(),skew->raw());

  gStack.insert(R);
}

static void cmd_EPolyRing_getMonoid(object &o1)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  if (R == 0)
    {
      gError << "expected polynomial ring";
      return;
    }
  gStack.insert((object_element *)R->getMonoid());
}
static void cmd_EPolyRing_getCoefficientRing(object &o1)
{
  ERing *R = o1->cast_to_ERing();
  gStack.insert((object_element *)R->getCoefficientRing());
}

//////////////////////
// Free Modules //////
//////////////////////

static void cmd_EFreeModule(object &o1, object &o2)
{
  ERing *R = o1->cast_to_ERing();
  int r = o2->int_of();
  EFreeModule *F = R->makeFreeModule(r);
  gStack.insert(F);
}

static void cmd_EFreeModule2(object &o1, object &o2)
{
  ERing *R = o1->cast_to_ERing();
  intarray *degs = o2->intarray_of();
  const EMonoid *D = R->getDegreeMonoid();
  int ndegrees = R->n_degrees();
  int rank = degs->length() / ndegrees;
  if (degs->length() % ndegrees != 0)
    {
      gError << "wrong number of degrees";
      return;
    }
  const monomial **result_degs = new const monomial *[rank];
  for (int i=0; i<rank; i++)
    {
      result_degs[i] = D->monomial_from_exponents(degs->raw() + i*ndegrees);
      if (result_degs[i] == 0)
        {
          delete [] result_degs;
          return;
        }
    }
  EFreeModule *F = R->makeFreeModule(rank,result_degs);
  gStack.insert(F);
}

static void cmd_EFreeModule3(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  const ERing *R = m->getTarget()->getRing();
  const EPolynomialRing *A = R->toPolynomialRing();
  if (A == 0)
    {
      gError << "expected polynomial ring";
      return;
    }
  EFreeModule *F = A->makeSchreyerFreeModule(m);
  gStack.insert(F);
}

static void cmd_EFreeModule_getRing(object &o1)
{
  const EFreeModule *F = o1->cast_to_EFreeModule();
  gStack.insert((object_element *)F->getRing());
}

static void cmd_EFreeModule_rank(object &o1)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  gStack.insert(make_object_int(F->rank()));
}

static void cmd_EFreeModule_getDegrees(object &o1)
{
  const EFreeModule *F = o1->cast_to_EFreeModule();
  intarray degs;
  F->getDegrees(degs);
  gStack.insert(new object_intarray(degs));
}

static void cmd_EFreeModule_getInducedOrder(object &o1)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  gStack.insert(F->getInducedOrder());
}

void cmd_EFreeModule_directSum(object &oV, object &oW)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  const EFreeModule *W = oW->cast_to_EFreeModule();
  gStack.insert(V->directSum(W));
}

void cmd_EFreeModule_isEqual(object &oV, object &oW)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  const EFreeModule *W = oW->cast_to_EFreeModule();
  gStack.insert(make_object_int(V->isEqual(W)));
}

void cmd_EFreeModule_tensor(object &oV, object &oW)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  const EFreeModule *W = oW->cast_to_EFreeModule();
  gStack.insert(V->tensor(W));
}

void cmd_EFreeModule_dual(object &oV)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  gStack.insert(V->dual());
}

void cmd_EFreeModule_shift(object &oV, object &oa)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  intarray *a = oa->intarray_of();
  const EMonoid *D = V->getDegreeMonoid();
  const monomial *m = monomialFromIntarray(D,*a);
  if (m == 0) return;
  gStack.insert(V->shift(m));
}

void cmd_EFreeModule_subSpace(object &oV, object &oa)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  intarray *a = oa->intarray_of();
  gStack.insert(V->subSpace(*a));
}

void cmd_EFreeModule_symm(object &oV, object &on)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  int n = on->int_of();
  gStack.insert(V->symm(n));
}

void cmd_EFreeModule_exterior(object &oV, object &on)
{
  const EFreeModule *V = oV->cast_to_EFreeModule();
  int n = on->int_of();
  gStack.insert(V->exterior(n));
}

//////////////////////
// Vectors  //////////
//////////////////////

// Creation of new elements
object_ERingElement * make_object_ERingElement(const ERing *R, ERingElement a)
{
  object_ERingElement *result = new object_ERingElement(R,a);
  object_element *b = result;
  //  EUniqueObjects->insert(b);
  result = (object_ERingElement *)b;
  return result;
}  
object_EVector * make_object_EVector(EVector &v)
{
  // Consumes v.  Soon: The output will be UNIQUE:  two vectors
  // in the front end will be equal iff their handles are equal.
  // MES: WRITE THIS!!

  object_EVector *result = new object_EVector(v);
  object_element *a = result;
  EUniqueObjects->insert(a);
  result = (object_EVector *)a;
  return result;
}
static void cmd_EVector_getFreeModule(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  gStack.insert((object_element *)v->getFreeModule());
}

static void cmd_EVector_zero(object &o1)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  EVector v = F->zero();
  gStack.insert(make_object_EVector(v));
}

static void cmd_EVector_basisElement(object &o1,object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  int i = o2->int_of();
  EVector v = F->basisElement(i);
  gStack.insert(make_object_EVector(v));
}

static void cmd_EVector_makeTerm(object &o1, object &o2, object &o3)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  const EPolynomialRing *R = F->getRing()->toPolynomialRing();
  if (R == 0)
    {
      gError << "expected polynomial ring";
      return;
    }
  intarray *a = o2->intarray_of();
  int x = o3->int_of();
  const EMonoid *M = R->getMonoid();
  if (a->length() != M->n_vars())
    {
      gError << "expected exponent vector of length " << M->n_vars();
      return;
    }
  ERingElement one = F->getCoefficientRing()->one();
  const monomial *m = M->monomial_from_exponents(a->raw());
  if (m == 0) return;
  EVector v = R->vec_term(F,one,m,x);
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_makeTerm1(object &o1, object &o2, object &o3, object &o4)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  const EPolynomialRing *R = F->getRing()->toPolynomialRing();
  if (R == 0)
    {
      gError << "expected polynomial ring";
      return;
    }
  int c = o2->int_of();
  intarray *a = o3->intarray_of();
  int x = o4->int_of();
  const EMonoid *M = R->getMonoid();
  if (a->length() != M->n_vars())
    {
      gError << "expected exponent vector of length " << M->n_vars();
      return;
    }
  ERingElement c1 = F->getCoefficientRing()->from_int(c);
  const monomial *m = M->monomial_from_exponents(a->raw());
  if (m == 0) return;
  EVector v = R->vec_term(F,c1,m,x);
  gStack.insert(make_object_EVector(v));
}
static int check_elems(int num, int typ)
     // Return whether the top 'num' elements on the stack have type 'typ'.
{
  for (int i=num-1; i >= 0 ; i--) 
    if (!gStack.in_bounds(i) || gStack[i]->type_id() != typ)
      return 0;
  return 1;
}
static void cmd_EVector_make(object &o1)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  // Now the previous F->rank() elements on the stack should be vectors...
  if (!check_elems(F->rank(), TY_ERingElement))
    {
      gError << "expected ring elements";
      return;
    }
  // Now grab each element in term.
  ERingElement *elems = new ERingElement [F->rank()];
  for (int i=0; i<F->rank(); i++)
    elems[i] = gStack[F->rank() - 1 - i]->cast_to_ERingElement()->getValue();
  EVector v = F->makeVector(elems);
  delete [] elems;
  gStack.poppem(F->rank());
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_sparse(object &o1, object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  intarray *a = o2->intarray_of();
  int len = a->length();
  // Now the previous len elements on the stack should be vectors...
  if (!check_elems(len, TY_ERingElement))
    {
      gError << "expected ring elements";
      return;
    }
  // Now grab each element in term.
  ERingElement *elems = new ERingElement[len];
  for (int i=0; i<len; i++)
    elems[i] = gStack[len - 1 - i]->cast_to_ERingElement()->getValue();
  EVector v = F->makeSparseVector(elems, *a);
  delete [] elems;
  gStack.poppem(len);
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_isequal(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  int result = v->isEqual(*w);
  gStack.insert(make_object_int(result));
}
static void cmd_EVector_iszero(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  int result = v->isZero();
  gStack.insert(make_object_int(result));
}
// Parts
static void cmd_EVector_getComponent(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  int n = o2->int_of();
  ERingElement g = v->getComponent(n);
  const ERing *R = v->getRing();
  gStack.insert(make_object_ERingElement(R,g));
}
static void cmd_EVector_leadComponent(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  gStack.insert(make_object_int(v->leadComponent()));
}
static void cmd_EVector_leadCoefficient(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  ERingElement a;
  if (!v->leadCoefficient(a))
    {
      gError << "expected non-zero vector";
      return;
    }
  gStack.insert(make_object_ERingElement(v->getRing(),a));
}
static void cmd_EVector_leadMonomial(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  const EPolynomialRing *R = v->getRing()->toPolynomialRing();
  if (R == 0)
    {
      gError << "expected vector over polynomial ring";
      return;
    }
  intarray a;
  const monomial *m = v->leadMonomial();
  if (m == 0)
    {
      gError << "zero vector has no lead monomial";
      return;
    }
  R->getMonoid()->to_variable_exponent_pairs(m,a);
  gStack.insert(new object_intarray(a));
}
static void cmd_EVector_leadTerm(object &o1, object &o2, object &o3)
{
  EVector *v = o1->cast_to_EVector();
  int n = o2->int_of();
  int only_same_component = o3->int_of();
  EVector result = v->leadTerm(n,only_same_component);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_getterms(object &ov, object &om, object &on)
{
  EVector *v = ov->cast_to_EVector();
  int m = om->int_of();
  int n = on->int_of();
  EVector result = v->getTerms(m,n);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_subvector(object &ov, object &oF, object &oa)
{
  EVector *v = ov->cast_to_EVector();
  const EFreeModule *F = oF->cast_to_EFreeModule();
  intarray *a = oa->intarray_of();
  EVector result = v->subvector(F,*a);
  gStack.insert(make_object_EVector(result));
}

// Arithmetic
static void cmd_EVector_negate(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  EVector result = v->negate();
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_add(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector result = v->add(*w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_subtract(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector result = v->subtract(*w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_ZZmultiply(object &o1, object &o2)
{
  int a = o1->int_of();
  EVector *w = o2->cast_to_EVector();
  EVector result = w->multiply_by_ZZ(a);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_multiply(object &o1, object &o2)
{
  object_ERingElement *w = o1->cast_to_ERingElement();
  EVector *v = o2->cast_to_EVector();
  if (v->getRing() != w->getRing())
    {
      gError << "expected same ring";
      return;
    }
  EVector result = v->leftMultiply(*w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_rightMultiply(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  object_ERingElement *w = o2->cast_to_ERingElement();
  if (v->getRing() != w->getRing())
    {
      gError << "expected same ring";
      return;
    }
  EVector result = v->rightMultiply(*w);
  gStack.insert(make_object_EVector(result));
}
// Homogeneity
static void cmd_EVector_isgraded(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  const monomial *d;  // NOT USED
  gStack.insert(make_object_int(v->isGraded(d)));
}
static void cmd_EVector_degree(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  const monomial *d = v->degree();
  intarray degs = monomialToIntarray(v->getFreeModule()->getDegreeMonoid(), d);
  gStack.insert(new object_intarray(degs));
  
}
static void cmd_EVector_degreeWeightsLoHi(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  intarray *wts = o2->intarray_of();
  int lo,hi;
  v->degreeWeightsLoHi(wts->length(), wts->raw(),lo,hi);
  gStack.insert(make_object_int(lo));
  gStack.insert(make_object_int(hi));
}
static void cmd_EVector_homogenize(object &ov, object &ovar, object &owts)
{
  EVector *v = ov->cast_to_EVector();
  EVector result;
  int var = ovar->int_of();
  intarray *wts = owts->intarray_of();
  const ERing *R = v->getRing();
  if (var < 0 || var >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[var] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }
  if (!v->homogenize(var, wts->length(), wts->raw(), result))
    {
      return;
    }
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_homogenize1(object &ov, object &ovar, object &odeg, object &owts)
{
  EVector *v = ov->cast_to_EVector();
  EVector result;
  int var = ovar->int_of();
  int deg = odeg->int_of();
  intarray *wts = owts->intarray_of();
  const ERing *R = v->getFreeModule()->getRing();
  if (var < 0 || var >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[var] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }
  if (!v->homogenize(var, deg, wts->raw(), result))
    {
      return;
    }
  gStack.insert(make_object_EVector(result));
}

//////////////////////
// Ring Elements /////
//////////////////////
  
static void cmd_ERingElement_getRing(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  gStack.insert(r->getRing());
}
static void cmd_ERingElement_fromInteger(object &o1, object &o2)
{
  // WARNING!! THIS NEEDS TO CHANGE WITH ZZ...!!
  const ERing *R = o1->cast_to_ERing();
  int a = o2->int_of();
  ERingElement result = R->from_int(a);
  gStack.insert(make_object_ERingElement(R,result));
}

static void cmd_ERingElement_var(object &o1, object &o2, object &o3)
{
  int v = o1->int_of();
  int e = o2->int_of();
  const EPolynomialRing *R = o3->cast_to_EPolynomialRing();
  ERingElement result = R->make_ring_variable(v,e);
  gStack.insert(make_object_ERingElement(R,result));
}

static void cmd_ERingElement_term(object &o1, object &o2, object &o3)
{
  const EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  object_ERingElement *c = o2->cast_to_ERingElement();
  intarray *a = o3->intarray_of();

  // Now check that the coefficient ring of 'coeff' is the same as the coefficient ring.
  if (c->getRing() != R->getCoefficientRing())
    {
      gError << "expected coefficient in coefficient ring";
      return;
    }

  ERingElement c1 = c->getRing()->clone(*c);
  ERingElement result = R->make_term(c1,*a);
  gStack.insert(make_object_ERingElement(R,result));
}


static void cmd_ERingElement_isequal(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  object_ERingElement *r2 = o2->cast_to_ERingElement();
  const ERing *R = r->getRing();
  if (R != r2->getRing())
    {
      gError << "expected same ring";
      return;
    }
  int s = R->is_equal(*r, *r2);
  gStack.insert(make_object_int(s));
}
static void cmd_ERingElement_isequal2(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  int a = o2->int_of();  // MES: when arbitrary precision is back, change this!!
  const ERing *R = r->getRing();
  ERingElement b = R->from_int(a);
  int s = R->is_equal(*r, b);
  R->remove(b);
  gStack.insert(make_object_int(s));
}
static void cmd_ERingElement_iszero(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  int s = r->getRing()->is_zero(*r);
  gStack.insert(make_object_int(s));
}
static void cmd_ERingElement_negate(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  ERingElement s = r->getRing()->negate(*r);
  gStack.insert(make_object_ERingElement(r->getRing(), s));
}
static void cmd_ERingElement_add(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  object_ERingElement *r2 = o2->cast_to_ERingElement();
  const ERing *R = r->getRing();
  if (R != r2->getRing())
    {
      gError << "expected same ring";
      return;
    }
  ERingElement s = R->add(*r, *r2);
  gStack.insert(make_object_ERingElement(R, s));
}
static void cmd_ERingElement_subtract(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  object_ERingElement *r2 = o2->cast_to_ERingElement();
  const ERing *R = r->getRing();
  if (R != r2->getRing())
    {
      gError << "expected same ring";
      return;
    }
  ERingElement s = R->subtract(*r, *r2);
  gStack.insert(make_object_ERingElement(R, s));
}
static void cmd_ERingElement_ZZmultiply(object &o1, object &o2)
{
  int a = o1->int_of();
  object_ERingElement *r = o2->cast_to_ERingElement();
  ERingElement s = r->getRing()->mult_by_ZZ(*r,a);
  gStack.insert(make_object_ERingElement(r->getRing(), s));
}
static void cmd_ERingElement_multiply(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  object_ERingElement *r2 = o2->cast_to_ERingElement();
  const ERing *R = r->getRing();
  if (R != r2->getRing())
    {
      gError << "expected same ring";
      return;
    }
  ERingElement s = R->mult(*r, *r2);
  gStack.insert(make_object_ERingElement(R, s));
}
static void cmd_ERingElement_power(object &o1, object &o2)
{
  int a = o2->int_of();
  object_ERingElement *r = o1->cast_to_ERingElement();
  ERingElement s = r->getRing()->power(*r,a);
  gStack.insert(make_object_ERingElement(r->getRing(), s));
}

static void cmd_ERingElement_leadCoefficient(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  ERingElement s = R->leadCoefficient(*r);
  gStack.insert(make_object_ERingElement(R->getCoefficientRing(), s));
}
static void cmd_ERingElement_leadTerm(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  int n = o2->int_of();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  ERingElement s = R->leadTerm(*r, n);
  gStack.insert(make_object_ERingElement(R, s));
}
static void cmd_ERingElement_leadMonomial(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  const monomial *m = R->leadMonomial(*r);
  if (m == 0)
    {
      gError << "zero vector has no lead monomial";
      return;
    }
  intarray a;
  R->getMonoid()->to_variable_exponent_pairs(m,a);
  gStack.insert(new object_intarray(a));
}
#if 0
static void cmd_ERingElement_getCoefficient(object &oelem, object &om)
{
  object_ERingElement *r = oelem->cast_to_ERingElement();
  intarray *mon = om->intarray_of();
  XXX
  Monomial m = om->cast_to_Monomial();
  gStack.insert(r.get_coeff(m));
}
#endif
static void cmd_ERingElement_isgraded(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  const monomial *d;  // NOT USED
  gStack.insert(make_object_int(R->isGraded(*r,d)));
}
static void cmd_ERingElement_degree(object &o1)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const ERing *R = r->getRing();
  const monomial *result = R->degree(r->getValue());
  intarray degs = monomialToIntarray(R->getDegreeMonoid(), result);
  gStack.insert(new object_intarray(degs));
  
}
static void cmd_ERingElement_degreeWeightsLoHi(object &o1, object &o2)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  intarray *wts = o2->intarray_of();
  int lo,hi;
  R->degreeWeightsLoHi(*r, wts->length(), wts->raw(),lo,hi);
  gStack.insert(make_object_int(lo));
  gStack.insert(make_object_int(hi));
}
static void cmd_ERingElement_getterms(object &o1, object &om, object &on)
{
  object_ERingElement *r = o1->cast_to_ERingElement();
  const EPolynomialRing *R = r->getRing()->toPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial ring";
      return;
    }
  int m = om->int_of();
  int n = on->int_of();
  ERingElement result = R->getTerms(*r,m,n);
  gStack.insert(make_object_ERingElement(R,result));
}
/////////////////////////
// Ring map operations //
/////////////////////////

static void cmd_ERingMap(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  ERingMap *result = new ERingMap(m);
  gStack.insert(result);
}
static void cmd_ERingMap_getRing(object &o1)
{
  ERingMap *f = o1->cast_to_ERingMap();
  gStack.insert(f->getTarget());
}
static void cmd_ERingMap_eval_ERingElement(object &o1, object &o2)
{
  ERingMap *f = o1->cast_to_ERingMap();
  object_ERingElement *g = o2->cast_to_ERingElement();
  ERingElement result = f->evaluate(g->getRing(), g->getValue());
  const ERing *resultR = f->getTarget();
  gStack.insert(make_object_ERingElement(resultR,result));
}
static void cmd_ERingMap_eval_EVector(object &o1, object &o2, object &o3)
{
  ERingMap *f = o1->cast_to_ERingMap();
  const EFreeModule *resultF = o2->cast_to_EFreeModule();
  EVector *g = o3->cast_to_EVector();
  EVector result = f->evaluate(resultF, *g);
  gStack.insert(make_object_EVector(result));
}
static void cmd_ERingMap_eval_EMatrix(object &o1, object &o2, object &o3)
{
  ERingMap *f = o1->cast_to_ERingMap();
  const EFreeModule *resultF = o2->cast_to_EFreeModule();
  EMatrix *m = o3->cast_to_EMatrix();
  EMatrix *result = f->evaluate(resultF, m);
  gStack.insert(result);
}

static void cmd_ERingElement_promote(object &oR, object &of)
{
  const ERing *newR = oR->cast_to_ERing();
  object_ERingElement * f = of->cast_to_ERingElement();
  const ERing *Rf = f->getRing();
  ERingElement result;
  if (newR->promote(Rf, f->getValue(), result))
    gStack.insert(make_object_ERingElement(newR,result));
  else
    gError << "cannot promote given ring element";
}
static void cmd_ERingElement_lift(object &oR, object &of)
{
  const ERing *newR = oR->cast_to_ERing();
  object_ERingElement * f = of->cast_to_ERingElement();
  const ERing *Rf = f->getRing();
  ERingElement result;
  if (Rf->lift(newR, f->getValue(), result))
    gStack.insert(make_object_ERingElement(newR,result));
  else
    gError << "cannot lift given ring element";
}
static void cmd_EVector_promote(object &oF, object &of)
{
  const EFreeModule *newF = oF->cast_to_EFreeModule();
  EVector *v = of->cast_to_EVector();
  EVector result;
  if (v->promote(newF, result))
    gStack.insert(make_object_EVector(result));
  else
    gError << "cannot promote given vector";
}
static void cmd_EVector_lift(object &oF, object &of)
{
  const EFreeModule *newF = oF->cast_to_EFreeModule();
  EVector *v = of->cast_to_EVector();
  EVector result;
  if (v->lift(newF, result))
    gStack.insert(make_object_EVector(result));
  else
    gError << "cannot lift given vector";
}
static void cmd_EMatrix_promote(object &oF, object &of)
{
  const EFreeModule *newF = oF->cast_to_EFreeModule();
  EMatrix *m = of->cast_to_EMatrix();
  EMatrix *result = 0;
  if (m->promote(newF, result))
    gStack.insert(result);
  else
    gError << "cannot promote given matrix";
}
static void cmd_EMatrix_lift(object &oF, object &of)
{
  const EFreeModule *newF = oF->cast_to_EFreeModule();
  EMatrix *m = of->cast_to_EMatrix();
  EMatrix *result = 0;
  if (m->lift(newF, result))
    gStack.insert(result);
  else
    gError << "cannot promote given matrix";
}


///////////////////////
// Matrix Operations //
///////////////////////


// getVectorsFromStack: returns an array 0..ncols-1 of EVector *'s.
// If an error occurs, gError is set, and 0 is returned.

EVector *getVectorsFromStack(const EFreeModule *F, int ncols)
{
  int i;
  bool bad = false;
  EVector *result = EMatrix::allocate_columns(ncols);
    
  for (i=ncols-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_id() != TY_EVector)
	{
	  gError << "matrix: expected vector on the stack";
	  bad = true;
	  break;
	}
      EVector *v = gStack[i]->cast_to_EVector();
      if (v->getFreeModule()->getRing() != F->getRing())
	{
	  gError << "matrix: vector has incorrect base ring";
	  bad = true;
	  break;
	}
      result[ncols-1-i] = v->translate(F);
    }
  if (bad)
    {
      delete [] result;
      result = 0;
    }
  gStack.poppem(ncols);      
  return result;
}

EMatrix **getMatricesFromStack(int n)
{
  // Caller grabs the resulting array 0..n-1, if it is not 0.
  int i;
  bool bad = false;
  if (n <= 0) return 0;
  EMatrix **result = new EMatrix *[n];
  for (i=0; i<n; i++)
    result[i] = 0;
  for (i=0; i<n; i++)
    if (!gStack.in_bounds(i) || gStack[i]->type_id() != TY_EMatrix)
      {
	gError << "expected " << n << " matrices";
	bad = true;
	break;
      }
    else
      result[n-i-1] = gStack[i]->cast_to_EMatrix();
  for (i=1; i<n; i++)
    if (result[0]->getTarget()->getRing() != result[i]->getTarget()->getRing())
      {
        gError << "matrices have different base rings";
        bad = true;
        break;
      }
  gStack.poppem(n);
  if (bad)
    {
      delete [] result;
      return 0;
    }
  return result;
}

static void cmd_EMatrix1(object &orows, object &ocols, object &otype, object &omapdeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(ncols-1) rows:EFreeModule ncols:int mapdeg:intarray>> matrix
{
  int ncols = ocols->int_of();
  EFreeModule *F = orows->cast_to_EFreeModule();
  intarray *mapdeg = omapdeg->intarray_of();
  
  EVector *newcols = getVectorsFromStack(F,ncols);
  if (newcols == 0) return;
  int newtype = otype->int_of();
  if (newtype <= 0 || newtype > 3)
    {
      gError << "incorrect matrix type (left,right or both)";
      delete [] newcols;
      return;
    }
  const monomial *mapdegree = monomialFromIntarray(F->getDegreeMonoid(),*mapdeg);
  if (mapdegree != 0)
    gStack.insert(EMatrix::make(F,ncols,newcols,newtype,mapdegree));
  else
    delete [] newcols;
}

static void cmd_EMatrix2(object &orows, object &ocols, object &otype, object &omapdeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(ncols-1) rows:EFreeModule cols:EFreeModule mapdeg:intarray>> matrix
{
  EFreeModule *G = ocols->cast_to_EFreeModule();
  EFreeModule *F = orows->cast_to_EFreeModule();
  intarray *mapdeg = omapdeg->intarray_of();
  
  EVector *newcols = getVectorsFromStack(F,G->rank());
  if (newcols == 0) return;
  int newtype = otype->int_of();
  if (newtype <= 0 || newtype > 3)
    {
      gError << "incorrect matrix type (left,right or both)";
      delete [] newcols;
      return;
    }
  const monomial *mapdegree = monomialFromIntarray(F->getDegreeMonoid(),*mapdeg);
  if (mapdegree == 0)
    delete [] newcols;
  else
    gStack.insert(EMatrix::make(F,G,newcols,newtype,mapdegree));
}
static void cmd_EMatrix3(object &orows, object &ocols, object &om, object &otype, object &omapdeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(ncols-1) rows:EFreeModule cols:EFreeModule mapdeg:intarray>> matrix
{
  EFreeModule *G = ocols->cast_to_EFreeModule();
  EFreeModule *F = orows->cast_to_EFreeModule();
  EMatrix *m = om->cast_to_EMatrix();
  intarray *mapdeg = omapdeg->intarray_of();
  
  int newtype = otype->int_of();
  if (newtype <= 0 || newtype > 3)
    {
      gError << "incorrect matrix type (left,right or both)";
      return;
    }
  const monomial *mapdegree = monomialFromIntarray(F->getDegreeMonoid(),*mapdeg);
  if (mapdegree == 0)
    return;
  else
    gStack.insert(EMatrix::make(F,G,m,newtype,mapdegree));
}

static void cmd_EMatrix_getTarget(object &o1)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert((object_element *)m->getTarget());
}
static void cmd_EMatrix_getSource(object &o1)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert((object_element *)m->getSource());
}

static void cmd_EMatrix_getMapDegree(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  const monomial *mapdeg = m->getMapDegree();
  const EMonoid *D = m->getTarget()->getDegreeMonoid();
  gStack.insert(new object_intarray(monomialToIntarray(D,mapdeg)));
}
static void cmd_EMatrix_getMatrixType(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert(make_object_int(m->getMatrixType()));
}
static void cmd_EMatrix_column(object &oM, object &on)
{
  EMatrix *M = oM->cast_to_EMatrix();
  int n = on->int_of();
  if ((n < 0) || (n >= M->n_cols()))
    gError << "matrix index " << n << " out of range 0 .. " << M->n_cols()-1;
  else
    {
      EVector result = M->column(n).clone();
      gStack.insert(make_object_EVector(result));
    }
}
static void cmd_EMatrix_entry(object &oM, object &on, object &om)
{
  EMatrix *M = oM->cast_to_EMatrix();
  int n = on->int_of();
  int m = om->int_of();
  if ((n < 0) || (n >= M->n_rows()))
    gError << "matrix row index " << n << " out of range 0 .. " << M->n_rows()-1;
  else if ((m < 0) || (m >= M->n_cols()))
    gError << "matrix column index " << m << " out of range 0 .. " 
      << M->n_cols()-1;
  else
    {
      ERingElement elem = M->entry(n,m);
      gStack.insert(make_object_ERingElement(M->getTarget()->getRing(), elem));
    }
}
static void cmd_EMatrix_isEqual(object &o1, object &o2)
{
  EMatrix *m = o1->cast_to_EMatrix();
  EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(make_object_int(m->isEqual(n)));
}
static void cmd_EMatrix_isZero(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert(make_object_int(m->isZero()));
}
static void cmd_EMatrix_isGraded(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert(make_object_int(m->isGraded()));
}

static void cmd_EMatrix_negate(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert(m->negate());
}
static void cmd_EMatrix_add(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->add(n));
}
static void cmd_EMatrix_subtract(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->subtract(n));
}
static void cmd_EMatrix_mult(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->multiply(n));
}
static void cmd_EMatrix_multvec(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EVector *v = o2->cast_to_EVector();
  EVector result = m->vectorImage(*v);
  gStack.insert(make_object_EVector(result));
}
// Multi-linear algebra
static void cmd_EMatrix_reshape(object &o1, object &o2, object &o3)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  EFreeModule *F = o2->cast_to_EFreeModule();
  EFreeModule *G = o3->cast_to_EFreeModule();
  gStack.insert(m->reshape(F,G));
}
static void cmd_EMatrix_flip(object &o1, object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  EFreeModule *G = o2->cast_to_EFreeModule();
  gStack.insert(EMatrix::flip(F,G));
}
static void cmd_EMatrix_exteriorProduct(object &o1, object &o2, object &o3)
{
  int p = o1->int_of();
  int q = o2->int_of();
  EFreeModule *F = o3->cast_to_EFreeModule();
  gStack.insert(EMatrix::exteriorProduct(p,q,F));
}
static void cmd_EMatrix_transpose(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  gStack.insert(m->transpose());
}
static void cmd_EMatrix_submatrix(object &o1, object &o2, object &o3)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  intarray *rows = o2->intarray_of();
  intarray *cols = o3->intarray_of();
  gStack.insert(m->submatrix(*rows, *cols));
}
static void cmd_EMatrix_submatrix1(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  intarray *cols = o2->intarray_of();
  gStack.insert(m->submatrix(*cols));
}
static void cmd_EMatrix_koszul(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  int p = o2->int_of();
  gStack.insert(m->koszul(p));
}
static void cmd_EMatrix_koszul2(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(EMatrix::koszul(m,n));
}
static void cmd_EMatrix_leadTerm(object &o1, object &o2, object &o3)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  int n = o2->int_of();
  int same_comp_only = o3->int_of();
  gStack.insert(m->leadTerm(n,same_comp_only));
}
static void cmd_EMatrix_identity(object &o1)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  gStack.insert(EMatrix::identity(F));
}
static void cmd_EMatrix_zero(object &o1,object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  EFreeModule *G = o2->cast_to_EFreeModule();
  gStack.insert(EMatrix::zero(F,G));
}
static void cmd_EMatrix_random(object &o1,object &o2,object &o3)
{
  ERing *R = o1->cast_to_ERing();
  int r = o2->int_of();
  int c = o3->int_of();
  gStack.insert(EMatrix::random(R,r,c));
}
static void cmd_EMatrix_directSum(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->directSum(n));
}
static void cmd_EMatrix_directSumSeveral(object &o1)
{
  int n = o1->int_of();
  EMatrix **matrices = getMatricesFromStack(n);
  EMatrix *result;
  if (matrices == 0) return;
  if (n == 1) 
    result = matrices[0];
  else
    {
      result = matrices[0]->directSum(matrices[1]);
      for (int i=2; i<n; i++)
      {
        EMatrix *result1 = result->directSum(matrices[i]);
        delete result;
        result = result1;
      }
    }
  gStack.insert(result);
  delete [] matrices;
}
// The following is NOT static, since it is called from x_mat.cpp
void cmd_EMatrix_concatenate(object &o1)
{
  int i;
  int n = o1->int_of();
  EMatrix **matrices = getMatricesFromStack(n);
  EMatrix *result;
  if (matrices == 0) return;
  for (i=1; i<n; i++)
    if (matrices[i]->n_rows() != matrices[0]->n_rows())
      {
        gError << "concatenate: row sizes are not equal";
        delete matrices;
        return;
      }
  if (n == 1) 
    result = matrices[0];
  else
    {
      result = matrices[0]->concatenate(matrices[1]);
      for (i=2; i<n; i++)
      {
        EMatrix *result1 = result->concatenate(matrices[i]);
        delete result;
        result = result1;
      }
    }
  gStack.insert(result);
  delete [] matrices;
}
static void cmd_EMatrix_tensor(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->tensor(n));
}
static void cmd_EMatrix_moduleTensor(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  gStack.insert(m->moduleTensor(n));
}
static void cmd_EMatrix_homogenize(object &o1,object &o2,object &o3)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  int v = o2->int_of();
  intarray *wts = o3->intarray_of();
  if (wts->length() != m->getTarget()->getRing()->n_vars())
    {
      gError << "weight vector is of incorrect length";
      return;
    }
  gStack.insert(m->homogenize(v,wts->length(),wts->raw()));
}

static void cmd_EMatrix_smult(object &o1, object &o2)
{
  const object_ERingElement *a = o1->cast_to_ERingElement();
  const EMatrix *m = o2->cast_to_EMatrix();
  if (a->getRing() != m->getTarget()->getRing())
    {
      gError << "expected same ring";
      return;
    }
  gStack.insert(m->leftMultiply(a->getValue()));
}
static void cmd_EMatrix_srightmult(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const object_ERingElement *a = o2->cast_to_ERingElement();
  if (a->getRing() != m->getTarget()->getRing())
    {
      gError << "expected same ring";
      return;
    }
  gStack.insert(m->rightMultiply(a->getValue()));
}
static void cmd_EMatrix_diff(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  // rings must be the same: checked in 'diff'.
  gStack.insert(m->diff(n,true));
}
static void cmd_EMatrix_contract(object &o1, object &o2)
{
  const EMatrix *m = o1->cast_to_EMatrix();
  const EMatrix *n = o2->cast_to_EMatrix();
  // rings must be the same: checked in 'diff'.
  gStack.insert(m->diff(n,false));
}
static void cmd_EMatrix_sort(object &om, object &odegorder, object &omonorder)
{
  const EMatrix *M = om->cast_to_EMatrix();
  int degorder = odegorder->int_of();
  int monorder = omonorder->int_of();
  object_intarray *result = new object_intarray;
  int *perm = M->sort(degorder,monorder);
  for (int i=0; i<M->n_cols(); i++)
    result->intarray_of()->append(perm[i]);
  gStack.insert(result);
}
static void cmd_EMatrix_elim(object &oM, object &on)
{
  const EMatrix *M = oM->cast_to_EMatrix();
  int n = on->int_of();
  object_intarray *result = new object_intarray;
  M->selectInSubring(n, *result->intarray_of());
  gStack.insert(result);
}
static void cmd_EMatrix_coefficients(object &om, object &ovars)
{
  EMatrix *M = om->cast_to_EMatrix();
  intarray *vars = ovars->intarray_of();
  const EPolynomialRing *R = M->getTarget()->getRing()->toPolynomialRing();
  if (R == NULL)
    {
      gError << "need a polynomial ring";
      return;
    }
  int nvars = R->n_vars();
  bool *v = new bool[nvars];
  int i;
  for (i=0; i<nvars; i++) v[i] = false;
  for (i=0; i<vars->length(); i++)
    {
      int w = (*vars)[i];
      if (w < 0 || w >= nvars)
	{
	  gError << "'coeffs': variable index out of range";
	  delete [] v;
	  return;
	}
      v[w] = true;
    }
  EMatrix *result_monoms;
  EMatrix *result_coeffs = M->coefficients(v, result_monoms);
  delete [] v;
  gStack.insert(result_coeffs);
  gStack.insert(result_monoms);

}
static void cmd_EMatrix_sat(object &oM, object &on, object &omax)
{
#if 0
  const EMatrix *M = oM->cast_to_EMatrix();
  int n = on->int_of();
  int maxd = omax->int_of();
  int actual_divisor;
  EMatrix *result = M->divideByVariable(n,maxd,actual_divisor);
  gStack.insert(result);
  gStack.insert(make_object_int(actual_divisor));
#endif
}
static void cmd_EMatrix_divideByVariables(object &oM, object &on, object &omax)
{
#if 0
  // MES: implement divideByVariable
  const EMatrix *M = oM->cast_to_EMatrix();
  intarray  *n = on->intarray_of();
  intarray  *maxd = omax->intarray_of();  // entry of -1 means: divide by this variable as much as possible
				// entry of -2 means: divide by all but 1 of this variable.
  object_intarray *actual_divisor = new object_intarray;
  EMatrix *result = M->divideByVariables(*n,*maxd,actual_divisor->intarray_of());
  gStack.insert(result);
  gStack.insert(actual_divisor);
#endif
}

////////////////////
// EMonomialIdeal //
////////////////////
#if 0
static void cmd_EMI_toMatrix(object &o1)
{
  EMonomialIdeal *mi = o1->cast_to_EMonomialIdeal();
  EMatrix *m = mi->toMatrix();
  gStack.insert(m);
}
static void cmd_EMI_fromMatrix(object &o1,  object &o2)
{
  EMatrix *m = o1->cast_to_EMatrix();
  EMonomialIdeal *mi = new EMonomialIdeal(m,o2->int_of());
  gStack.insert(mi);
}
#endif
////////////////////
// EGB's ///////////
////////////////////
static void cmd_EGB_declareGB(object &ogens, object &ogb, object &ochange)
{
  EMatrix *gens = ogens->cast_to_EMatrix();
  EMatrix *m    = ogb->cast_to_EMatrix();
  EMatrix *change = ochange->cast_to_EMatrix();
  EMatrix *syz = EMatrix::zero(change->getTarget(), change->getRing()->makeFreeModule(0));
  EGroebnerComputation *p = EDeclaredGB::make(gens,m,change,syz);
  gStack.insert(p);
}

static void cmd_EGB_declareGB1(object &ogens, object &ogb, object &ochange, object &osyz)
{
  EMatrix *gens = ogens->cast_to_EMatrix();
  EMatrix *m    = ogb->cast_to_EMatrix();
  EMatrix *change = ochange->cast_to_EMatrix();
  EMatrix *syz = osyz->cast_to_EMatrix();
  EGroebnerComputation *p = EDeclaredGB::make(gens,m,change,syz);
  gStack.insert(p);
}
static void cmd_EGB_stats(object &op)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  p->stats();
}
static void cmd_EGB_getgb(object &op)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  gStack.insert(p->getGB());
}
static void cmd_EGB_getchange(object &op)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  gStack.insert(p->getChangeOfBasis());
}
static void cmd_EGB_getsyz(object &op)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  gStack.insert(p->getSyzygies());
}
static void cmd_EGB_getmingens(object &op)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  gStack.insert(p->getMinimalGenerators());
}
static void cmd_EGB_in(object &op, object &on)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  gStack.insert(p->getLeadTerms(on->int_of()));
}
static void cmd_EGB_reduceVector(object &op, object &ov, object &om)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  EVector *v = ov->cast_to_EVector();
  int m = om->int_of();
  if (m <= 0 || m >= 4)
    {
      gError << "expected left/right/2sided";
      return;
    }
  EVector lifted;
  EVector result = p->reduceVector(*v,m,lifted);
  gStack.insert(make_object_EVector(result));
  gStack.insert(make_object_EVector(lifted));
}
static void cmd_EGB_reduceMatrix(object &op, object &ov)
{
  EGroebnerComputation *p = op->cast_to_EGroebnerComputation();
  EMatrix *v = ov->cast_to_EMatrix();
  EMatrix *lifted;
  EMatrix *result = p->reduceMatrix(v,lifted);
  gStack.insert(result);
  gStack.insert(lifted);
}
void i_Ecommands(void)
{
#if 0
  EMonomialIdeal::mystash = new stash("EMonomialIdeal", sizeof(EMonomialIdeal));
  EMonomialLookupTable::mystash = new stash("EMonomialLookupTable",
					    sizeof(EMonomialLookupTable));
  EMInode::mystash = new stash("EMInode", sizeof(EMInode));
#endif
  EUniqueObjects = new EHashTable;
  EZ = EZZ::make();
  install(ggEZZ, cmd_EZZ);
  // Initialize stashes.
  
  install(ggtest, cmd_EHashTable_stats);
  
  // Commands for creation of objects
  // Coefficient rings


  //////////////////////
  // Monomial Orders ///
  //////////////////////

  install(ggMOinit, cmd_EMO_init);
  install(ggMOclone, cmd_EMO_clone, TY_EMonomialOrder);

  install(ggMOrevlex, cmd_EMO_revlex, TY_INT, TY_INT, TY_EMonomialOrder);
  install(ggMOrevlex, cmd_EMO_revlexWeights, TY_INTARRAY, TY_INT, TY_EMonomialOrder);
  install(ggMOlex, cmd_EMO_lex, TY_INT, TY_INT, TY_EMonomialOrder);

  install(ggMOcomponent, cmd_EMO_component, TY_EMonomialOrder);
  install(ggMOwtfcn, cmd_EMO_weightFunction, TY_INTARRAY, TY_EMonomialOrder);
  install(ggMOproduct, cmd_EMO_product, TY_EMonomialOrder, TY_EMonomialOrder);
  install(ggMONClex, cmd_EMO_NClex, TY_INT, TY_EMonomialOrder);

  // Monomial order query functions
  // MES: this needs to be done...

  //////////////////////
  // Monoids ///////////
  //////////////////////

  install(ggmonoid, cmd_EMonoid, TY_EMonomialOrder, TY_INTARRAY, TY_STRING);

  install(gggetMO, cmd_EMonoid_monomialOrder, TY_EMonoid);
  install(ggnvars, cmd_EMonoid_nvars, TY_EMonoid);

  install(ggstats, cmd_EMonoid_stats, TY_EMonoid);

  //////////////////////
  // Polynomial Rings //
  //////////////////////

  install(ggEZZ, cmd_EZZ);
  install(ggEcharp, cmd_EZZp, TY_INT);

  install(ggcharacteristic, cmd_EZZp_characteristic, TY_ERing);

  install(ggpolyring, cmd_EPolynomialRing, TY_ERing, TY_EMonoid);
  install(ggweylalgebra, cmd_EWeylAlgebra, TY_ERing, TY_EMonoid, 
	  TY_INTARRAY, TY_INTARRAY, TY_INT);
  install(ggskewpolyring, cmd_ESkewCommPolynomialRing, 
	  TY_ERing, TY_EMonoid, TY_INTARRAY);

  install(gggetCoefficientRing, cmd_EPolyRing_getCoefficientRing, TY_ERing);
  install(gggetMonoid, cmd_EPolyRing_getMonoid, TY_ERing);

  //////////////////////
  // Free Modules //////
  //////////////////////

  install(ggfree, cmd_EFreeModule, TY_ERing, TY_INT);
  install(ggfree, cmd_EFreeModule2, TY_ERing, TY_INTARRAY);
  install(ggfree, cmd_EFreeModule3, TY_EMatrix);

  install(gggetring, cmd_EFreeModule_getRing, TY_EFreeModule);
  install(ggrank, cmd_EFreeModule_rank, TY_EFreeModule);
  install(ggdegree, cmd_EFreeModule_getDegrees, TY_EFreeModule);
  install(gggetcols, cmd_EFreeModule_getInducedOrder, TY_EFreeModule);//CHANGE ggNAME

  install(ggisequal, cmd_EFreeModule_isEqual, TY_EFreeModule, TY_EFreeModule);
  install(ggadd, cmd_EFreeModule_directSum, TY_EFreeModule, TY_EFreeModule);
  install(ggmult, cmd_EFreeModule_tensor, TY_EFreeModule, TY_EFreeModule);
  install(ggtranspose, cmd_EFreeModule_dual, TY_EFreeModule);
  install(ggsubmodule, cmd_EFreeModule_subSpace, TY_EFreeModule, TY_INTARRAY);
  install(ggsymm, cmd_EFreeModule_symm, TY_EFreeModule, TY_INT);
  install(ggexterior, cmd_EFreeModule_exterior, TY_EFreeModule, TY_INT);

  install(ggshift, cmd_EFreeModule_shift, TY_EFreeModule, TY_INTARRAY);

  //////////////////////
  // Ring Elements /////
  //////////////////////
  
  install(gggetring, cmd_ERingElement_getRing, TY_ERingElement);

  install(ggfromint, cmd_ERingElement_fromInteger, TY_ERing, TY_INT);
  install(ggvar, cmd_ERingElement_var, TY_INT, TY_INT, TY_ERing);
  install(ggterm, cmd_ERingElement_term, TY_ERing, TY_ERingElement, TY_INTARRAY);

  install(ggisequal, cmd_ERingElement_isequal, TY_ERingElement, TY_ERingElement);
  install(ggisequal, cmd_ERingElement_isequal2, TY_ERingElement, TY_INT);
  install(ggiszero, cmd_ERingElement_iszero, TY_ERingElement);
  install(ggnegate, cmd_ERingElement_negate, TY_ERingElement);
  install(ggadd, cmd_ERingElement_add, TY_ERingElement, TY_ERingElement);
  install(ggsubtract, cmd_ERingElement_subtract, TY_ERingElement, TY_ERingElement);
  install(ggmult, cmd_ERingElement_ZZmultiply, TY_INT, TY_ERingElement);
  install(ggmult, cmd_ERingElement_multiply, TY_ERingElement, TY_ERingElement);
  install(ggpower, cmd_ERingElement_power, TY_ERingElement, TY_INT);

  install(ggleadcoeff, cmd_ERingElement_leadCoefficient, TY_ERingElement);
  install(ggleadterm, cmd_ERingElement_leadTerm, TY_ERingElement, TY_INT);
  install(ggleadmonom, cmd_ERingElement_leadMonomial, TY_ERingElement);

  install(ggdegree, cmd_ERingElement_degree, TY_ERingElement);
  install(ggdegree, cmd_ERingElement_degreeWeightsLoHi, TY_ERingElement, TY_INTARRAY);
  install(ggishomogeneous, cmd_ERingElement_isgraded, TY_ERingElement);

  install(gggetterms, cmd_ERingElement_getterms, TY_ERingElement, TY_INT, TY_INT);

  //////////////////////
  // Vectors ///////////
  //////////////////////

  install(gggetFreeModule, cmd_EVector_getFreeModule, TY_EVector);

  install(ggzero, cmd_EVector_zero, TY_EFreeModule);
  install(ggbasisElement, cmd_EVector_basisElement, TY_EFreeModule, TY_INT);
  install(ggterm, cmd_EVector_makeTerm, TY_EFreeModule, TY_INTARRAY, TY_INT);
  install(ggterm, cmd_EVector_makeTerm1, TY_EFreeModule, TY_INT, TY_INTARRAY, TY_INT);

  install(ggvector, cmd_EVector_make, TY_EFreeModule);
  install(ggsparsevector, cmd_EVector_sparse, TY_EFreeModule, TY_INTARRAY);

  install(ggelem, cmd_EVector_getComponent, TY_EVector, TY_INT);
  install(ggleadcomp, cmd_EVector_leadComponent, TY_EVector);
  install(ggleadcoeff, cmd_EVector_leadCoefficient, TY_EVector);
  install(ggleadterm, cmd_EVector_leadTerm, TY_EVector, TY_INT, TY_INT);
  install(ggleadmonom, cmd_EVector_leadMonomial, TY_EVector);

  install(ggisequal, cmd_EVector_isequal, TY_EVector, TY_EVector);
  install(ggiszero, cmd_EVector_iszero, TY_EVector);
  install(ggnegate, cmd_EVector_negate, TY_EVector);
  install(ggadd, cmd_EVector_add, TY_EVector, TY_EVector);
  install(ggsubtract, cmd_EVector_subtract, TY_EVector, TY_EVector);
  install(ggmult, cmd_EVector_ZZmultiply, TY_INT, TY_EVector);
  install(ggmult, cmd_EVector_multiply, TY_ERingElement, TY_EVector);
  install(ggrightmult, cmd_EVector_rightMultiply, TY_EVector, TY_ERingElement);

  install(ggdegree, cmd_EVector_degree, TY_EVector);
  install(ggdegree, cmd_EVector_degreeWeightsLoHi, TY_EVector, TY_INTARRAY);

  install(ggishomogeneous, cmd_EVector_isgraded, TY_EVector);
  install(gghomogenize, cmd_EVector_homogenize, TY_EVector, TY_INT, TY_INTARRAY);
  install(gghomogenize, cmd_EVector_homogenize1, TY_EVector, TY_INT, TY_INT, TY_INTARRAY);

  install(gggetterms, cmd_EVector_getterms, TY_EVector, TY_INT, TY_INT);
  install(ggselect, cmd_EVector_subvector, TY_EVector, 
	  TY_EFreeModule, TY_INTARRAY);

  //////////////////////
  // Matrices //////////
  //////////////////////

  install(ggmatrix, cmd_EMatrix1, TY_EFreeModule, TY_INT, TY_INT, TY_INTARRAY);
  install(ggmatrix, cmd_EMatrix2, TY_EFreeModule, TY_EFreeModule, TY_INT, TY_INTARRAY);
  install(ggmatrix, cmd_EMatrix3, TY_EFreeModule, TY_EFreeModule, TY_EMatrix, TY_INT, TY_INTARRAY);

  install(ggisequal, cmd_EMatrix_isEqual, TY_EMatrix, TY_EMatrix);
  install(ggiszero, cmd_EMatrix_isZero, TY_EMatrix);

  install(gggetrows, cmd_EMatrix_getTarget, TY_EMatrix);
  install(gggetcols, cmd_EMatrix_getSource, TY_EMatrix);
  install(gggetshift, cmd_EMatrix_getMapDegree, TY_EMatrix);
  install(ggsetshift, cmd_EMatrix_getMatrixType, TY_EMatrix);   // MES:change which gg command this is!!
  install(ggelem, cmd_EMatrix_column, TY_EMatrix, TY_INT);
  install(ggelem, cmd_EMatrix_entry, TY_EMatrix, TY_INT, TY_INT);

  install(ggnegate, cmd_EMatrix_negate, TY_EMatrix);
  install(ggishomogeneous, cmd_EMatrix_isGraded, TY_EMatrix);

  install(ggadd, cmd_EMatrix_add, TY_EMatrix, TY_EMatrix);
  install(ggsubtract, cmd_EMatrix_subtract, TY_EMatrix, TY_EMatrix);
  install(ggmult, cmd_EMatrix_mult, TY_EMatrix, TY_EMatrix);
  install(ggmult, cmd_EMatrix_multvec, TY_EMatrix, TY_EVector);
  install(ggreshape, cmd_EMatrix_reshape, TY_EMatrix, 
	  TY_EFreeModule, TY_EFreeModule);
  install(ggflip, cmd_EMatrix_flip, TY_EFreeModule, TY_EFreeModule);
  install(ggtranspose, cmd_EMatrix_transpose, TY_EMatrix);
  install(ggsubmatrix, cmd_EMatrix_submatrix, TY_EMatrix, 
	  TY_INTARRAY, TY_INTARRAY);
  install(ggsubmatrix, cmd_EMatrix_submatrix1, TY_EMatrix, TY_INTARRAY);
  install(ggkoszul, cmd_EMatrix_koszul, TY_EMatrix, TY_INT);
  install(ggkoszul, cmd_EMatrix_koszul2, TY_EMatrix, TY_EMatrix);
  install(ggleadterm, cmd_EMatrix_leadTerm, TY_EMatrix, TY_INT, TY_INT);
  install(ggiden, cmd_EMatrix_identity, TY_EFreeModule);
  install(ggzeromat, cmd_EMatrix_zero, TY_EFreeModule, TY_EFreeModule);
  install(ggrandom, cmd_EMatrix_random, TY_ERing, TY_INT, TY_INT);
  install(ggdirectsum, cmd_EMatrix_directSum, TY_EMatrix, TY_EMatrix);
  install(ggdirectsum, cmd_EMatrix_directSumSeveral, TY_INT);
  install(ggtensor, cmd_EMatrix_tensor, TY_EMatrix, TY_EMatrix);
  install(ggmodtensor, cmd_EMatrix_moduleTensor, TY_EMatrix, TY_EMatrix);
  install(ggexteriorproduct, cmd_EMatrix_exteriorProduct,
	  TY_INT, TY_INT, TY_EFreeModule);
  //install(ggconcat, cmd_EMatrix_concatenate, TY_INT);  : replace once x_mat.cpp is gone.
  install(gghomogenize, cmd_EMatrix_homogenize, TY_EMatrix, TY_INT, TY_INTARRAY);

  install(ggmult, cmd_EMatrix_smult, TY_ERingElement, TY_EMatrix);
  install(ggmult, cmd_EMatrix_srightmult, TY_EMatrix, TY_ERingElement);
  install(ggcontract, cmd_EMatrix_contract, TY_EMatrix, TY_EMatrix);
  install(ggdiff, cmd_EMatrix_diff, TY_EMatrix, TY_EMatrix);
  install(ggsortcolumns, cmd_EMatrix_sort, TY_EMatrix, TY_INT, TY_INT);
  install(ggelim, cmd_EMatrix_elim, TY_EMatrix, TY_INT);
  install(ggsat, cmd_EMatrix_sat, TY_EMatrix, TY_INT, TY_INT);
  install(ggsat, cmd_EMatrix_divideByVariables, TY_EMatrix, TY_INTARRAY, TY_INTARRAY);
  install(ggcoeffs, cmd_EMatrix_coefficients, TY_EMatrix, TY_INTARRAY);

#if 0
  install(ggsymm, cmd_EMatrix_symm, TY_EMatrix, TY_INT);
  install(ggkbasis, cmd_EMatrix_kbasis, TY_EMatrix, TY_EMatrix, TY_INTARRAY);
  install(ggtruncate, cmd_EMatrix_truncate, TY_EMatrix, TY_EMatrix, TY_INTARRAY);
  install(ggkbasis, cmd_EMatrix_kbasis, TY_EMatrix, TY_EMatrix);

  install(ggexterior, cmd_EMatrix_exterior, TY_EMatrix, TY_INT);

  install(ggcoeffs, cmd_EMatrix_var_coeffs, TY_EMatrix);

  install(ggminleadterms, cmd_EMatrix_minleadterms, TY_EMatrix);
  install(ggsimplify, cmd_EMatrix_simplify, TY_EMatrix, TY_INT);
  install(ggautoreduce, cmd_EMatrix_autoreduce, TY_EMatrix);
#endif

  //////////////////////
  // Ring Maps /////////
  //////////////////////
  install(ggringmap, cmd_ERingMap, TY_EMatrix);
  install(gggetring, cmd_ERingMap_getRing, TY_ERingMap);

  install(ggev, cmd_ERingMap_eval_ERingElement, TY_ERingMap, TY_ERingElement);
  install(ggev, cmd_ERingMap_eval_EVector, TY_ERingMap, TY_EFreeModule, TY_EVector);
  install(ggev, cmd_ERingMap_eval_EMatrix, TY_ERingMap, TY_EFreeModule, TY_EMatrix);

  install(ggpromote, cmd_ERingElement_promote, TY_ERing, TY_ERingElement);
  install(ggpromote, cmd_EMatrix_promote, TY_EFreeModule, TY_EMatrix);
  install(ggpromote, cmd_EVector_promote, TY_EFreeModule, TY_EVector);

  install(gglift, cmd_ERingElement_lift, TY_ERing, TY_ERingElement);
  install(gglift, cmd_EMatrix_lift, TY_EFreeModule, TY_EMatrix);
  install(gglift, cmd_EVector_lift, TY_EFreeModule, TY_EVector);

  //////////////////////
  // Monomial Ideals ///
  //////////////////////
#if 0
  install(ggmatrix, cmd_EMI_toMatrix, TY_EMonomialIdeal);
  install(ggmonideal, cmd_EMI_fromMatrix, TY_EMatrix, TY_INT);

  install(ggcopy, cmd_EMI_copy, TY_MONIDEAL);
  install(ggisequal, cmd_EMI_isequal, TY_MONIDEAL, TY_MONIDEAL);
  install(ggradical, cmd_EMI_radical, TY_MONIDEAL);
  install(ggadd, cmd_EMI_add, TY_MONIDEAL, TY_MONIDEAL);
  install(ggmult, cmd_EMI_product, TY_MONIDEAL, TY_MONIDEAL);
  install(ggintersect, cmd_EMI_intersect, TY_MONIDEAL, TY_MONIDEAL);
  install(ggdiv, cmd_EMI_quotient1, TY_MONIDEAL, TY_MONOMIAL);
  install(ggdiv, cmd_EMI_quotient, TY_MONIDEAL, TY_MONIDEAL);
  install(ggsat, cmd_EMI_sat1, TY_MONIDEAL, TY_MONOMIAL);
  install(ggsat, cmd_EMI_sat, TY_MONIDEAL, TY_MONIDEAL);

  install(ggremove, cmd_EMI_remove, TY_MONIDEAL);

  install(ggborel, cmd_EMI_borel, TY_MONIDEAL);
  install(ggisborel, cmd_EMI_isborel, TY_MONIDEAL);

  install(ggcodim, cmd_EMI_codim, TY_MONIDEAL);
  install(ggprimes, cmd_EMI_assprimes, TY_MONIDEAL);
#endif
  //////////////////////
  // Groebner bases ////
  //////////////////////
  install(gggb, cmd_EGB_declareGB, TY_EMatrix, TY_EMatrix, TY_EMatrix);
  install(gggb, cmd_EGB_declareGB1, TY_EMatrix, TY_EMatrix, TY_EMatrix, TY_EMatrix);

  install(gggetmingens, cmd_EGB_getmingens, TY_EGroebnerComputation);  
  install(gggetgb, cmd_EGB_getgb, TY_EGroebnerComputation);  
  install(gggetchange, cmd_EGB_getchange, TY_EGroebnerComputation);  
  install(gggetsyz, cmd_EGB_getsyz, TY_EGroebnerComputation);  
  install(gginitial, cmd_EGB_in, TY_EGroebnerComputation, TY_INT);  

  install(ggstats, cmd_EGB_stats, TY_EGroebnerComputation);
  install(ggreduce, cmd_EGB_reduceVector, TY_EGroebnerComputation, TY_EVector, TY_INT);
  install(ggreduce, cmd_EGB_reduceMatrix, TY_EGroebnerComputation, TY_EMatrix);
#if 0
  install(ggcalc, cmd_EGB_calc, TY_EGroebnerComputation, TY_INTARRAY, TY_INTARRAY);
#endif
}
