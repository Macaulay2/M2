// Copyright 1998 by Michael E. Stillman

#include "Eio.hpp"
#include "EZZp.hpp"
#include "Emonorder.hpp"
#include "Emonoid.hpp"
#include "Ering.hpp"
#include "Efreemod.hpp"
#include "Evector.hpp"
#include "Ematrix.hpp"

#include "interp.hpp"
#include "Ehashtab.hpp"

EHashTable EUniqueObjects;

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

monomial *monomialFromIntarray(const EMonoid *D, const intarray &mapdeg)
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
  EUniqueObjects.showshape();
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
static void cmd_EMO_revlex(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = o2->int_of();
  mo->revlex(nvars);
}
static void cmd_EMO_lex(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = o2->int_of();
  mo->lex(nvars);
}
static void cmd_EMO_revlexWeights(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  intarray *a = o2->intarray_of();
  mo->revlexWeights(a->length(),a->raw());
}
static void cmd_EMO_lexWeights(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  intarray *a = o2->intarray_of();
  mo->lexWeights(a->length(),a->raw());
}
static void cmd_EMO_revlex1(object &o1, object &o2, object &o3)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = o2->int_of();
  int isgroup = o3->int_of();
  mo->revlex(nvars, isgroup);
}
static void cmd_EMO_lex1(object &o1, object &o2, object &o3)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = o2->int_of();
  int isgroup = o3->int_of();
  mo->lex(nvars,isgroup);
}
static void cmd_EMO_revlexWeights1(object &o1, object &o2, object &o3)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  intarray *a = o2->intarray_of();
  int isgroup = o3->int_of();
  mo->revlexWeights(a->length(),a->raw(),isgroup);
}
static void cmd_EMO_lexWeights1(object &o1, object &o2, object &o3)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  intarray *a = o2->intarray_of();
  int isgroup = o3->int_of();
  mo->lexWeights(a->length(),a->raw(),isgroup);
}
static void cmd_EMO_component(object &o1)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  mo->component();
}
static void cmd_EMO_weightFunction(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  intarray *a = o2->intarray_of();
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
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  int nvars = o2->int_of();
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
    M = ECommMonoid::make(mo,
			  printorder->raw(),
			  (const char **) names);
  else
    M = ENCMonoid::make(mo,
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
  gStack.insert((object_element *)EPolynomialRing::getTrivialRing());
}

static void cmd_EZZp0(object &r)
{
  // MES: check the size of p: If large, make a different coefficient ring.
  int p = r->int_of();
  if (p > 0)
    gStack.insert(EZZp::make(p));
  else if (p == 0)
    gStack.insert((object_element *)EZZ::ZZ());
  else
    gError << "ZZ/p: p must be non-negative";
}

static void cmd_EZZp(object &r)
{
  // MES: check the size of p: If large, make a different coefficient ring.
  // This makes a polynomial ring with no variables...
  int p = r->int_of();
  if (p > 0)
    {
      ECoefficientRing *K = EZZp::make(p);
      EPolynomialRing *R = ECommPolynomialRing::make(K,
						     ECommMonoid::getTrivialMonoid(),
						     EPolynomialRing::getTrivialRing(),
						     0);
      gStack.insert(R);
    }
  else
    gError << "ZZ/p: p must be positive";
}

static void cmd_EZZp_characteristic(object &o1)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  gStack.insert(make_object_int(R->characteristic()));
}

static bool grabDegreeInfo(int nvars, EPolynomialRing *&ZD, int *&degrees)
{
  bool bad = false;
  intarray *degs;
  if (!gStack.in_bounds(0) || gStack[0]->type_id() != TY_INTARRAY)
    {
      gError << "ring construction: expected array of degrees";
      bad = true;
      degs = 0;
    }
  else
    degs = gStack[0]->intarray_of();

  if (!gStack.in_bounds(1) || gStack[1]->type_id() != TY_ERing)
    {
      gError << "ring construction: expected degree ring";
      bad = true;
      ZD = 0;
    }
  else
    ZD = gStack[1]->cast_to_EPolynomialRing();

  if (!bad && degs->length() != nvars * ZD->n_vars())
    {
      bad = true;
      gError << "wrong length for degree vector";
      degrees = 0;
    }
  else
    {
      degrees = new int[degs->length()];
      for (int i=0; i<degs->length(); i++)
        degrees[i] = (*degs)[i];
    }
  gStack.poppem(2);
  return !bad;
}
ECoefficientRing *getCoefficients(object &o1)
{
  // If o1 is a EPolynomialRing, check that the
  // number of variables is zero: if not return 0.
  // If ok, return the coefficient ring.
  // If o1 is a coefficient ring, return it.
  ECoefficientRing *result = o1->cast_to_ECoefficientRing();
  if (result != 0) return result;
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  if (R == 0) 
    {
      gError << "expected polynomial or coefficient ring";
      return 0;
    }
  if (R->n_vars() != 0)
    {
      gError << "expected a coefficient ring";
      return 0;
    }
  return (ECoefficientRing *) R->getCoefficientRing();
}
static void cmd_EPolynomialRing(object &o1, object &o2)
{
  ECoefficientRing *K = getCoefficients(o1);
  EMonoid * M = o2->cast_to_EMonoid();
  EPolynomialRing *R;
  EPolynomialRing *ZD;
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
  ECoefficientRing *K = getCoefficients(o1);
  EMonoid * M = o2->cast_to_EMonoid();
  intarray *diffs = o3->intarray_of();
  intarray *comms = o4->intarray_of();
  if (diffs->length() != comms->length())
    {
      gError << "Weyl algebra: expected same length arrays";
      return;
    }
  int homog_var = o5->int_of();
  EPolynomialRing *ZD;
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
  ECoefficientRing *K = getCoefficients(o1);
  EMonoid * M = o2->cast_to_EMonoid();
  intarray *skew = o3->intarray_of();

  EPolynomialRing *ZD;
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
  gStack.insert((object_element *)R->getMonoid());
}
static void cmd_EPolyRing_getCoefficientRing(object &o1)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  gStack.insert((object_element *)R->getCoefficientRing());
}
static void cmd_EPolyRing_getRingFreeModule(object &o1)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  gStack.insert((object_element *)R->getRingFreeModule());
}

//////////////////////
// Free Modules //////
//////////////////////

static void cmd_EFreeModule(object &o1, object &o2)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  int r = o2->int_of();
  EFreeModule *F = R->makeFreeModule(r);
  gStack.insert(F);
}

static void cmd_EFreeModule2(object &o1, object &o2)
{
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
  intarray *degs = o2->intarray_of();
  const EMonoid *D = R->getDegreeMonoid();
  int ndegrees = R->n_degrees();
  int rank = degs->length() / ndegrees;
  if (degs->length() % ndegrees != 0)
    {
      gError << "hi wrong number of degrees";
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
  buffer o;
  o << "free module ";
  for (int j=0; j<rank; j++)
    { D->elem_text_out(o,result_degs[j]); o << " "; }
  o << newline;
  emit(o.str());
  EFreeModule *F = R->makeFreeModule(rank,result_degs);
  gStack.insert(F);
}

static void cmd_EFreeModule3(object &o1)
{
  EMatrix *m = o1->cast_to_EMatrix();
  EFreeModule *F = m->getSource()->getRing()->makeFreeModule(m);
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
  monomial *m = monomialFromIntarray(D,*a);
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

object_EVector * make_object_EVector(EVector *&v)
{
  // Consumes v.  Soon: The output will be UNIQUE:  two vectors
  // in the front end will be equal iff their handles are equal.
  // MES: WRITE THIS!!

  if (v == 0) return 0;
  object_EVector *result = new object_EVector(v);
  object_element *a = result;
  EUniqueObjects.insert(a);
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
  EVector *v = F->zero();
  gStack.insert(make_object_EVector(v));
}

static void cmd_EVector_basisElement(object &o1,object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  int i = o2->int_of();
  EVector *v = F->basisElement(i);
  gStack.insert(make_object_EVector(v));
}

static void cmd_EVector_makeTerm(object &o1, object &o2, object &o3)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  intarray *a = o2->intarray_of();
  int x = o3->int_of();
  const EMonoid *M = F->getMonoid();
  if (a->length() != M->n_vars())
    {
      gError << "expected exponent vector of length " << M->n_vars();
      return;
    }
  field one = F->getCoefficientRing()->one();
  monomial *m = M->monomial_from_exponents(a->raw());
  if (m == 0) return;
  EVector *v = F->makeTerm(one,m,x);
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_makeTerm1(object &o1, object &o2, object &o3, object &o4)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  int c = o2->int_of();
  intarray *a = o3->intarray_of();
  int x = o4->int_of();
  const EMonoid *M = F->getMonoid();
  if (a->length() != M->n_vars())
    {
      gError << "expected exponent vector of length " << M->n_vars();
      return;
    }
  field c1 = F->getCoefficientRing()->from_int(c);
  monomial *m = M->monomial_from_exponents(a->raw());
  if (m == 0) return;
  EVector *v = F->makeTerm(c1,m,x);
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
  if (!check_elems(F->rank(), TY_EVector))
    {
      gError << "expected ring elements or vectors";
      return;
    }
  // Now grab each element in term.
  EVector **elems = new EVector *[F->rank()];
  for (int i=0; i<F->rank(); i++)
    elems[i] = gStack[F->rank() - 1 - i]->cast_to_EVector();
  EVector *v = F->makeVector(elems);
  delete [] elems;
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_sparse(object &o1, object &o2)
{
  EFreeModule *F = o1->cast_to_EFreeModule();
  intarray *a = o2->intarray_of();
  int len = a->length();
  // Now the previous len elements on the stack should be vectors...
  if (!check_elems(len, TY_EVector))
    {
      gError << "expected ring elements or vectors";
      return;
    }
  // Now grab each element in term.
  EVector **elems = new EVector *[len];
  for (int i=0; i<len; i++)
    elems[i] = gStack[len - 1 - i]->cast_to_EVector();
  EVector *v = F->makeSparseVector(elems, *a);
  delete [] elems;
  gStack.insert(make_object_EVector(v));
}
static void cmd_EVector_isequal(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  int result = v->isEqual(w);
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
  EVector *result = v->getComponent(n);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_leadComponent(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  gStack.insert(make_object_int(v->leadComponent()));
}
static void cmd_EVector_leadCoefficient(object &o1)
{
  // This will CHANGE when better coefficients are ready!!
  EVector *v = o1->cast_to_EVector();
  gStack.insert(make_object_int(v->leadCoefficient()));
}
static void cmd_EVector_leadTerm(object &o1, object &o2, object &o3)
{
  EVector *v = o1->cast_to_EVector();
  int n = o2->int_of();
  int only_same_component = o3->int_of();
  EVector *result = v->leadTerm(n,only_same_component);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_getterms(object &ov, object &om, object &on)
{
  EVector *v = ov->cast_to_EVector();
  int m = om->int_of();
  int n = on->int_of();
  EVector *result = v->getTerms(m,n);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_subvector(object &ov, object &oF, object &oa)
{
  EVector *v = ov->cast_to_EVector();
  const EFreeModule *F = oF->cast_to_EFreeModule();
  intarray *a = oa->intarray_of();
  EVector *result = F->subvector(v,*a);
  gStack.insert(make_object_EVector(result));
}

// Arithmetic
static void cmd_EVector_negate(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  EVector *result = v->negate();
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_add(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector *result = v->add(w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_subtract(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector *result = v->subtract(w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_ZZmultiply(object &o1, object &o2)
{
  int a = o1->int_of();
  EVector *w = o2->cast_to_EVector();
  EVector *result = w->multiply_by_ZZ(a);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_multiply(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector *result = v->multiply(w);
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_rightMultiply(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  EVector *w = o2->cast_to_EVector();
  EVector *result = v->rightMultiply(w);
  gStack.insert(make_object_EVector(result));
}
// Homogeneity
static void cmd_EVector_isgraded(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  monomial *d;  // NOT USED
  gStack.insert(make_object_int(v->isGraded(d)));
}
static void cmd_EVector_degree(object &o1)
{
  EVector *v = o1->cast_to_EVector();
  monomial *d = v->degree();
  intarray degs = monomialToIntarray(v->getFreeModule()->getDegreeMonoid(), d);
  gStack.insert(new object_intarray(degs));
  
}
static void cmd_EVector_degreeWeightsLoHi(object &o1, object &o2)
{
  EVector *v = o1->cast_to_EVector();
  intarray *wts = o2->intarray_of();
  int lo,hi;
  v->degreeWeightsLoHi(wts->raw(),lo,hi);
  gStack.insert(make_object_int(lo));
  gStack.insert(make_object_int(hi));
}
static void cmd_EVector_homogenize(object &ov, object &ovar, object &owts)
{
  EVector *v = ov->cast_to_EVector();
  int var = ovar->int_of();
  intarray *wts = owts->intarray_of();
  const EPolynomialRing *R = v->getFreeModule()->getRing();
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
  EVector *result = v->homogenize(var,wts->raw());
  gStack.insert(make_object_EVector(result));
}
static void cmd_EVector_homogenize1(object &ov, object &ovar, object &odeg, object &owts)
{
  EVector *v = ov->cast_to_EVector();
  int var = ovar->int_of();
  int deg = odeg->int_of();
  intarray *wts = owts->intarray_of();
  const EPolynomialRing *R = v->getFreeModule()->getRing();
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
  EVector *result = v->homogenize(var,deg,wts->raw());
  gStack.insert(make_object_EVector(result));
}
///////////////////////
// Matrix Operations //
///////////////////////


// getVectorsFromStack: returns an array 0..ncols-1 of EVector *'s.
// If an error occurs, gError is set, and 0 is returned.

EVector **getVectorsFromStack(const EFreeModule *F, int ncols)
{
  int i;
  bool bad = false;
  EVector **result = new EVector *[ncols];
  for (i=0; i<ncols; i++)
    result[i] = 0;
    
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
      result[ncols-1-i] = F->translate(v);
    }
  if (bad)
    {
      for (i=0; i<ncols; i++) 
        delete result[i];
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

EFreeModule *makeFreeModuleFromDegrees(const EPolynomialRing *R, int ncols, EVector **cols)
{
  monomial **degs = new monomial *[ncols];
  for (int i=0; i<ncols; i++)
    degs[i] = cols[i]->degree();
  return R->makeFreeModule(ncols,degs);
}

static void cmd_EMatrix1(object &orows, object &ocols, object &omapdeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(ncols-1) rows:EFreeModule ncols:int mapdeg:intarray>> matrix
{
  int ncols = ocols->int_of();
  EFreeModule *F = orows->cast_to_EFreeModule();
  intarray *mapdeg = omapdeg->intarray_of();
  
  EVector **newcols = getVectorsFromStack(F,ncols);
  if (newcols == 0) return;
  monomial *mapdegree = monomialFromIntarray(F->getDegreeMonoid(),*mapdeg);
  if (mapdegree == 0) return;
  EFreeModule *G = makeFreeModuleFromDegrees(F->getRing(),ncols,newcols);
  gStack.insert(EMatrix::make(F,G,newcols,mapdegree));
}

static void cmd_EMatrix2(object &orows, object &ocols, object &omapdeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(ncols-1) rows:EFreeModule cols:EFreeModule mapdeg:intarray>> matrix
{
  EFreeModule *G = ocols->cast_to_EFreeModule();
  EFreeModule *F = orows->cast_to_EFreeModule();
  intarray *mapdeg = omapdeg->intarray_of();
  
  EVector **newcols = getVectorsFromStack(F,G->rank());
  if (newcols == 0) return;
  monomial *mapdegree = monomialFromIntarray(F->getDegreeMonoid(),*mapdeg);
  if (mapdegree == 0) return;
  gStack.insert(EMatrix::make(F,G,newcols,mapdegree));
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
static void cmd_EMatrix_column(object &oM, object &on)
{
  EMatrix *M = oM->cast_to_EMatrix();
  int n = on->int_of();
  if ((n < 0) || (n >= M->n_cols()))
    gError << "matrix index " << n << " out of range 0 .. " << M->n_cols()-1;
  else
    {
      EVector *result = M->column(n)->clone();
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
      EVector *result = M->entry(n,m);
      gStack.insert(make_object_EVector(result));
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
  EVector *result = m->rightMultiply(v);
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
  EPolynomialRing *R = o1->cast_to_EPolynomialRing();
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
static void cmd_EMatrix_concatenate(object &o1)
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
  gStack.insert(m->homogenize(v,wts->raw()));
}

void i_Ecommands(void)
{
  // Initialize stashes.
  
  install(ggtest, cmd_EHashTable_stats);
  
  // Commands for creation of objects
  // Coefficient rings


  //////////////////////
  // Monomial Orders ///
  //////////////////////

  install(ggMOinit, cmd_EMO_init);
  install(ggMOclone, cmd_EMO_clone, TY_EMonomialOrder);
  install(ggMOrevlex, cmd_EMO_revlex, TY_EMonomialOrder, TY_INT);
  install(ggMOrevlex, cmd_EMO_revlexWeights, TY_EMonomialOrder, TY_INTARRAY);
  install(ggMOlex, cmd_EMO_lex, TY_EMonomialOrder, TY_INT);
  install(ggMOlex, cmd_EMO_lexWeights, TY_EMonomialOrder, TY_INTARRAY);
  install(ggMOrevlex, cmd_EMO_revlex1, TY_EMonomialOrder, TY_INT, TY_INT);
  install(ggMOrevlex, cmd_EMO_revlexWeights1, TY_EMonomialOrder, TY_INTARRAY, TY_INT);
  install(ggMOlex, cmd_EMO_lex1, TY_EMonomialOrder, TY_INT, TY_INT);
  install(ggMOlex, cmd_EMO_lexWeights1, TY_EMonomialOrder, TY_INTARRAY, TY_INT);
  install(ggMOcomponent, cmd_EMO_component, TY_EMonomialOrder);
  install(ggMOwtfcn, cmd_EMO_weightFunction, TY_EMonomialOrder, TY_INTARRAY);
  install(ggMOproduct, cmd_EMO_product, TY_EMonomialOrder, TY_EMonomialOrder);
  install(ggMONClex, cmd_EMO_NClex, TY_EMonomialOrder, TY_INT);

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

  install(ggpolyring, cmd_EPolynomialRing, TY_ECoefficientRing, TY_EMonoid);
  install(ggweylalgebra, cmd_EWeylAlgebra, TY_ECoefficientRing, TY_EMonoid, 
	  TY_INTARRAY, TY_INTARRAY, TY_INT);
  install(ggskewpolyring, cmd_ESkewCommPolynomialRing, 
	  TY_ECoefficientRing, TY_EMonoid, TY_INTARRAY);

  install(ggpolyring, cmd_EPolynomialRing, TY_ERing, TY_EMonoid);
  install(ggweylalgebra, cmd_EWeylAlgebra, TY_ERing, TY_EMonoid, 
	  TY_INTARRAY, TY_INTARRAY, TY_INT);
  install(ggskewpolyring, cmd_ESkewCommPolynomialRing, 
	  TY_ERing, TY_EMonoid, TY_INTARRAY);

  install(gggetCoefficientRing, cmd_EPolyRing_getCoefficientRing, TY_ERing);
  install(gggetMonoid, cmd_EPolyRing_getMonoid, TY_ERing);
  install(gggetRingFreeModule, cmd_EPolyRing_getRingFreeModule, TY_ERing);

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
  
  install(ggisequal, cmd_EVector_isequal, TY_EVector, TY_EVector);
  install(ggiszero, cmd_EVector_iszero, TY_EVector);
  install(ggnegate, cmd_EVector_negate, TY_EVector);
  install(ggadd, cmd_EVector_add, TY_EVector, TY_EVector);
  install(ggsubtract, cmd_EVector_subtract, TY_EVector, TY_EVector);
  install(ggmult, cmd_EVector_ZZmultiply, TY_INT, TY_EVector);
  install(ggmult, cmd_EVector_multiply, TY_EVector, TY_EVector);
  install(ggrightmult, cmd_EVector_rightMultiply, TY_EVector, TY_EVector);
  
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

  install(ggmatrix, cmd_EMatrix1, TY_EFreeModule, TY_INT, TY_INTARRAY);
  install(ggmatrix, cmd_EMatrix2, TY_EFreeModule, TY_EFreeModule, TY_INTARRAY);

  install(ggisequal, cmd_EMatrix_isEqual, TY_EMatrix, TY_EMatrix);
  install(ggiszero, cmd_EMatrix_isZero, TY_EMatrix);

  install(gggetrows, cmd_EMatrix_getTarget, TY_EMatrix);
  install(gggetcols, cmd_EMatrix_getSource, TY_EMatrix);
  install(gggetshift, cmd_EMatrix_getMapDegree, TY_EMatrix);
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
  install(ggconcat, cmd_EMatrix_concatenate, TY_INT);
  install(gghomogenize, cmd_EMatrix_homogenize, TY_EMatrix, TY_INT, TY_INTARRAY);

#if 0
  install(ggmult, cmd_EMatrix_smult, TY_RING_ELEM, TY_EMatrix);

  install(ggelim, cmd_EMatrix_elim, TY_EMatrix, TY_INT);
  install(ggsat, cmd_EMatrix_sat, TY_EMatrix, TY_INT, TY_INT);

  install(ggcontract, cmd_EMatrix_contract, TY_EMatrix, TY_EMatrix);
  install(ggdiff, cmd_EMatrix_diff, TY_EMatrix, TY_EMatrix);
  install(ggsymm, cmd_EMatrix_symm, TY_EMatrix, TY_INT);
  install(ggkbasis, cmd_EMatrix_kbasis, TY_EMatrix, TY_EMatrix, TY_INTARRAY);
  install(ggtruncate, cmd_EMatrix_truncate, TY_EMatrix, TY_EMatrix, TY_INTARRAY);
  install(ggkbasis, cmd_EMatrix_kbasis, TY_EMatrix, TY_EMatrix);

  install(ggexterior, cmd_EMatrix_exterior, TY_EMatrix, TY_INT);

  install(ggcoeffs, cmd_EMatrix_coeffs, TY_EMatrix, TY_INTARRAY);
  install(ggcoeffs, cmd_EMatrix_var_coeffs, TY_EMatrix);

  install(ggminleadterms, cmd_EMatrix_minleadterms, TY_EMatrix);
  install(ggsimplify, cmd_EMatrix_simplify, TY_EMatrix, TY_INT);
  install(ggsortcolumns, cmd_EMatrix_sort, TY_EMatrix, TY_INT, TY_INT);
  install(ggautoreduce, cmd_EMatrix_autoreduce, TY_EMatrix);
#endif

  //////////////////////
  // Ring Maps /////////
  //////////////////////
#if 0
  install(ggringmap, cmd_ERingMap, TY_EMatrix);

  install(ggev, cmd_ERingMap_eval_ringelem, TY_ERingMap, TY_RING_ELEM);
  install(ggev, cmd_ERingMap_eval_vector, TY_ERingMap, TY_EFreeModule, TY_EVector);
  install(ggev, cmd_ERingMap_eval_matrix, TY_ERingMap, TY_EFreeModule, TY_EMatrix);

  install(ggpromote, cmd_ERingElement_promote, TY_RING, TY_RING_ELEM);
  install(ggpromote, cmd_EMatrix_promote, TY_EFreeModule, TY_EMatrix);
  install(ggpromote, cmd_EVector_promote, TY_EFreeModule, TY_EVector);

  install(gglift, cmd_ERingElement_lift, TY_RING, TY_RING_ELEM);
  install(gglift, cmd_EMatrix_lift, TY_EFreeModule, TY_EMatrix);
  install(gglift, cmd_EVector_lift, TY_EFreeModule, TY_EVector);
#endif
}
