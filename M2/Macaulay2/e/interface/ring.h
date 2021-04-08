#ifndef _ring_h_
#  define _ring_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Computation;
class Matrix;
class Monoid;
class Ring;
class RingElement;
#  else
typedef struct Computation Computation;
typedef struct Matrix Matrix;
typedef struct Monoid Monoid;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   Ring interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

unsigned int rawRingHash(const Ring *R); /* drg: connected hash */
/* assigned sequentially */

M2_string IM2_Ring_to_string(const Ring *M); /* drg: connected */

long rawRingCharacteristic(const Ring *R); /* connected: rawCharacteristic */

///////////////////
// Ring creation //
///////////////////

const Ring *IM2_Ring_ZZ(void); /* drg: connected rawZZ*/
/* always returns the same object */

const Ring *IM2_Ring_QQ(void); /* drg: connected rawQQ */
/* always returns the same object */

const Ring /* or null */ *IM2_Ring_ZZp(int p); /* drg: connected rawZZp*/
/* Expects a prime number p in range 2 <= p <= 32749 */

const Ring /* or null */ *rawGaloisField(const RingElement *f);
/* drg: connected to rawGaloisField */
/* f should be a primitive element in a ring
   R = ZZ/p[x]/(g(x)), some p, some variable x, g irreducible,
   monic, deg >= 2.
   However, currently, not all of this is checked...
*/

const Ring /* or null */ *IM2_Ring_RRR(unsigned long prec);
/* drg: connected rawRRR */

const Ring /* or null */ *IM2_Ring_CCC(unsigned long prec);
/* drg: connected rawCCC */

const Ring /* or null */ *IM2_Ring_polyring(const Ring *K, const Monoid *M);
/* drg: connected rawPolynomialRing(,) */
/* K can be either commutative or not. If K is a quotient ring, the relations
 are ignored.  Use rawQuotientRing to then make the desired quotient ring. */

const Ring *IM2_Ring_trivial_polyring();
/* drg: connected rawPolynomialRing() */
/* This returns the polynomial ring ZZ[], whose degree ring is itself */

const Ring /* or null */ *IM2_Ring_skew_polyring(const Ring *R,
                                                 M2_arrayint skewvars);
/* drg: reconnected rawSkewPolynomialRing */
/* R should be a polynomial ring K[M], where K is commutative.
   skewvars should be a list of variable indices (for the monoid M) indicating
   which ones are skew-commutative with each other. Further quotients (using
   IM2_Ring_quotient) are allowed */

const Ring /* or null */ *IM2_Ring_weyl_algebra(const Ring *R,
                                                M2_arrayint comm_vars,
                                                M2_arrayint diff_vars,
                                                int homog_var);
/* drg: reconnected rawWeylAlgebra*/
/* R should be a polynomial ring K[M], where K is commutative.
   comm_vars and diff_vars should be arrays of the same length.  diff_vars[i] is
   the differential operator corresponding to comm_vars[i].  If homog_var is
   non-negative, then the multiplication is taken to be the homogeneous Weyl
   algebra (that is Dx*x = x*Dx + h^2, where h is the variable of R with index
   'homog_var'). Further quotients (using IM2_Ring_quotient) are allowed,
   however, one must make sure that the quotient is only by elements in the
   center of the ring.
*/

const Ring /* or null */ *IM2_Ring_solvable_algebra(const Ring *R,
                                                    const Matrix *Q);
/* drg: connected rawSolvableAlgebra */
/* R should be a polynomial ring K[M], where K is commutative.
   Q is a square matrix of size #gens(M).  If the variables are X_1, ..., X_r,
   then multiplication is defined to be X_j X_i = Q_(j,i), for j > i.
   The initial term of Q_(j,i) MUST be c * X_i * X_j, for some constant
   c (depending on i and j), or at least less than that in the monomial
   ordering.
*/

const Ring * /* or null */ rawRingM2FreeAlgebra(const Ring *coefficientRing,
                                                M2_ArrayString names,
                                                const Ring *degreeRing,
                                                M2_arrayint degrees,
                                                M2_arrayint wtvecs,
                                                M2_arrayint heftVector);
/* coefficientRing will be the allowed coefficients of our non-comm polynomials.
   wtvecs is a flattened array of all of the weight vectors for the monomial
   order, where contains the first weightvector, then second weightvector, etc.
*/

/* WIP
const M2FreeMonoid* rawM2FreeMonoid(M2_ArrayString names,
                                    const Ring* degreeRing,
                                    M2_arrayint degrees,
                                    M2_arrayint wtvecs,
                                    M2_arrayint heftVector)
*/

const Ring * /* or null */ rawRingM2FreeAlgebraQuotient(const Matrix *GB,
                                                        int maxdeg);
/* Given a non-commutative Groebner basis GB, create the quotient ring of it.
   maxdeg is either -1 (the GB is complete), or the maximum degree that the GB
   has been computed to */

const Ring /* or null */ *IM2_Ring_frac(const Ring *R);
/* drg: connected rawFractionRing*/

const Ring /* or null */ *IM2_Ring_localization(const Ring *R, Computation *P);
/* drg: connected rawLocalRing */
/* Create the localization of R.
   R should be a COMMUTATIVE ring.  P should be a one row matrix
   whose entries generate a prime ideal of R.
*/

const Ring /* or null */ *IM2_Ring_quotient(const Ring *R, const Matrix *I);
/*drg: connected rawQuotientRing */
/* Given a quotient of an ambient poly ring R = A/J, and a GB I in R, form
   the quotient ring A/(I+J). */

const Ring /* or null */ *IM2_Ring_quotient1(const Ring *R, const Ring *B);
/*drg: connected rawQuotientRing */
/* if R is a polynomial ring of the form A[x], and B = A/I (where A is a poly
   ring) then form the quotient ring B[x]. */

const Ring /* or null */ *IM2_Ring_schur(const Ring *R);
/* drg: reconnected rawSchurRing */

const Ring *rawSchurRing1(const Ring *A);

const Ring *rawSchurRing2(const Ring *A, int n);

const Ring *rawSchurSnRing(const Ring *A, int n);

const Ring /* or null */ *rawTowerRing1(long charac, M2_ArrayString names);
/* Create a tower ring with the given variable names and characteristic */

const Ring /* or null */ *rawTowerRing2(const Ring *R1,
                                        M2_ArrayString new_names);
const Ring /* or null */ *rawTowerRing3(const Ring *R1,
                                        engine_RawRingElementArray eqns);

// FIXME: these three aren't defined in ring.cpp
const Ring /* or null */ *rawARingTower1(const Ring *R1, M2_ArrayString names);
/* Create a tower ring with the given variable names and base ring */

const Ring /* or null */ *rawARingTower2(const Ring *R1,
                                         M2_ArrayString new_names);
const Ring /* or null */ *rawARingTower3(const Ring *R1,
                                         engine_RawRingElementArray eqns);

M2_bool IM2_Ring_is_field(const Ring *K); /* drg: connected rawIsField*/
/* Returns true if K is a field, or has been declared to be one.
   In the latter case, if an operation shows that K cannot be a field,
   then this function will thereafter return false, and
   rawGetNonUnit(K) can be used to obtain a non-unit, if one
   has been found. */

M2_bool IM2_Ring_declare_field(const Ring *K);
/* drg: connected rawDeclareField*/
/* Declare that K is a field.  The ring K can then be used as the coefficient
   ring for computing Groebner bases,etc.  If false is returned, then
   the ring K has known non-units, and an error has been issued */

const RingElement *rawGetNonUnit(const Ring *K);
/* drg: connected rawGetNonUnit */
/* Return a non-unit for the ring K, if one has been found, or the zero
   element, if not. Perhaps we should name this 'get_non_unit'.  This
   function currently never seems to return a non-zero value, but I plan
   on fixing that (MES, June 2002). */

const Ring /* or null */ *rawAmbientRing(const Ring *R);
/* drg: connected rawAmbientRing */
/* If R is a quotient of a polynomial ring, or is a fraction ring, return the
   polynomial ring over a basic ring of which this is a quotient (or fraction
   ring) of. For example, if R = frac(ZZ[s,t]/(s^2-1))[x,y,z]/(s*x+t*y+z^2),
   then the returned ring is ZZ[s,t][x,y,z]. This routine is provided only for
   debugging the engine. */

const Ring /* or null */ *rawDenominatorRing(const Ring *R);
/* drg: connected rawDenominatorRing */
/* If elements of R may have denominators, then this routine returns true, and
   the ambient ring for denominators returned. Otherwise, NULL
   is returned, which is not to be considered an error.  This routine is
   provided only for debugging the engine. */

/////////////////////////////
// GaloisField routines /////
/////////////////////////////

// FIXME: should these be moved elsewhere?

/* FIXME
bool findConwayPolynomial(long charac,
                             long deg,
                             bool find_random_if_no_conway_poly_available,
                             std::vector<long> &result_poly);
*/

/* returns an array of non-negative integers, which represents the given Conway
 polynomial If there is none, a list of length 0 is returned. if the boolean
 argument is set to true, returns a random poly that flint finds
*/
M2_arrayintOrNull rawConwayPolynomial(
    long charac,
    long deg,
    M2_bool find_random_if_no_conway_poly_available);

#  if defined(__cplusplus)
}
#  endif

#endif /* _ring_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
