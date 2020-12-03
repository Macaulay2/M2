/* Copyright 2002 by Michael E. Stillman */

#ifndef _engine_h_
#define _engine_h_

/**
   \mainpage Hi, this is my main documentation page.
 */

#include "engine-includes.hpp"
#include "rand.h" // engine interface for random numbers

#if defined(__cplusplus)
class Monomial;
class Monoid;
class Ring;
class FreeModule;
class MonomialIdeal;
class Matrix;
class MutableMatrix;
class RingElement;
class RingMap;
class Computation;
class EngineComputation;
class MutableComplex;
// NAG begin
class M2SLEvaluator;
class M2Homotopy;
class M2SLProgram;
class StraightLineProgram;
class PathTracker;
class M2PointArray;
// NAG end

typedef struct MonomialOrdering MonomialOrdering;
#else
/* Define the externally visible types here */
typedef struct Monomial Monomial;
typedef struct Monoid Monoid;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct RingMap RingMap;
typedef struct Computation Computation;
typedef struct EngineComputation EngineComputation;
typedef struct MonomialOrdering MonomialOrdering;
typedef struct MonomialIdeal MonomialIdeal;
typedef struct MutableComplex MutableComplex;
// NAG begin
typedef struct M2SLEvaluator M2SLEvaluator;
typedef struct M2Homotopy M2Homotopy;
typedef struct M2SLProgram M2SLProgram;
typedef struct StraightLineProgram StraightLineProgram;
typedef struct PathTracker PathTracker;
typedef struct M2PointArray M2PointArray;
// NAG end
#endif

#if defined(NO_CONST)
  /* we must do this after including system *.h files above */
# define const
#endif

typedef EngineComputation EngineComputationOrNull;

#if defined(__cplusplus)
extern "C" {
#endif
  void IM2_initialize(void); /* drg: connected */
  M2_string IM2_last_error_message(void); /* drg: connected */

  M2_string engineMemory(); /* connected MES to engineMemory */

  /*****************************************************/
  /**** Integer primality and factorization (via flint)*/
  /*****************************************************/

  M2_bool rawZZisPrime(gmp_ZZ a);

  M2_bool rawZZisProbablePrime(gmp_ZZ a);

  gmp_arrayZZ rawZZfactor(gmp_ZZ a);
  
  /**************************************************/
  /**** Monomial routines ***************************/
  /**************************************************/
  /* Monomials in the engine: are not associated with a monoid, and may have negative
   * exponents.  Monomials are immutable objects.
   */

  /**************************************************/
  /**** MonomialOrdering routines *******************/
  /**************************************************/
  MonomialOrdering *rawLexMonomialOrdering(int nvars, int packing); /* drg: connected rawMonomialOrdering*/
    /* Lex, LexSmall, LexTiny */

  MonomialOrdering /* or null */ *rawGRevLexMonomialOrdering(M2_arrayint degs, int packing); /* drg: connected rawMonomialOrdering*/
    /* GRevLex, GrevLexSmall, GRevLexTiny */

  MonomialOrdering *rawRevLexMonomialOrdering(int nvars); /* drg: connected rawMonomialOrdering*/
    /* RevLex => n */

  MonomialOrdering *rawWeightsMonomialOrdering(M2_arrayint wts); /* drg: connected rawMonomialOrdering*/
    /* Weights => {...} */

  MonomialOrdering *rawGroupLexMonomialOrdering(int nvars); /* drg: connected rawMonomialOrdering*/
    /* GroupLex => n */

  MonomialOrdering *rawGroupRevLexMonomialOrdering(int nvars);
    /* GroupRevLex => n */

  MonomialOrdering *rawNClexMonomialOrdering(int nvars); /* drg: connected rawMonomialOrdering*/
    /* NCLex => n */

  MonomialOrdering *rawPositionMonomialOrdering(M2_bool up_or_down); /* drg: connected rawMonomialOrdering */
    /* argument of true:  Position => Up, (should be the default)
     * argument of false: Position => Down
     */

  MonomialOrdering *rawProductMonomialOrdering(engine_RawMonomialOrderingArray mo); /* drg: connected rawMonomialOrdering*/
    /* for tensor products */

  MonomialOrdering *rawJoinMonomialOrdering(engine_RawMonomialOrderingArray mo); /* drg: connected rawMonomialOrdering*/
    /* default, when making monoids and polynomial rings */

  int rawNumberOfVariables(const MonomialOrdering *mo); /* drg: connected rawNumberOfVariables*/

  int rawNumberOfInvertibleVariables(const MonomialOrdering *mo); /* drg: connected rawNumberOfInvertibleVariables*/

  M2_arrayint rawNonTermOrderVariables(const MonomialOrdering *mo); /* Dan: PLEASE CONNECT */
  /* Returns an array of the indices of those variables which are less than 1 in the
     monomial ordering.  If this number is > 0, then the monomial ordering is not a term
     order, and local (tangent cone) algorithms must be used for GB's */

  M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering *mo); /* drg: connected */

  unsigned int rawMonomialOrderingHash(const MonomialOrdering *mo); /* drg: connected hash */
    /* Assigned sequentially */

  int moIsGRevLex(const struct MonomialOrdering *mo);

  int moIsLex(const struct MonomialOrdering *mo);

  M2_arrayint moGetWeightValues(const struct MonomialOrdering *mo);

  M2_arrayintOrNull rawMonomialOrderingToMatrix(const struct MonomialOrdering *mo);
  /* return a (flattened) matrix corresponding to the monomial ordering 'mo'.
     Appended to this sequence of integers is 3 further numbers:
     (1) If the tie-breaker is revlex, one further value of "1" is added, else if it is lex, 
     one further value of "0" is added.
     (2) If the module order is Position=>Up, then 0 else 1 in the next spot.
     (3) If the modules part of the order is considered right before the ith row of this matrix
         then "i" is in the next spot. (i=#rows, f the module component is considered last).
     The returned value represents a matrix with #vars columns, and #gradings weights, in row-major order
     (each row is contiguous in memory), plus the three extra entries.
     NULL is returned if 'mo' corresponds to a non-commutative monoid, or has "Inverses=>true" set.
  */

  /**************************************************/
  /**** Monoid routines *****************************/
  /**************************************************/
  Monoid *IM2_Monoid_trivial();  /* drg: connected rawMonoid*/
    /* Always returns the same object */

  engine_RawMonoidOrNull IM2_Monoid_make(const MonomialOrdering *mo,
                                M2_ArrayString names,
                                const Ring *DegreeRing,
                                M2_arrayint degs,
                                M2_arrayint hefts); /* drg: connected rawMonoid*/
    /* This function will return NULL if the monomial order cannot be handled
       currently, if the first components for each degree are not all
       positive, or under various other "internal" error conditions */

  unsigned int rawMonoidHash(const Monoid *M);  /* drg: connected hash */
    /* Assigned sequentially */

  M2_string IM2_Monoid_to_string(const Monoid *M);  /* drg: connected */
    /* For debugging purposes */

  int rawMonoidNumberOfBlocks(const Monoid *M); /* connected rawMonoidNumberOfBlocks */
  
  /**************************************************/
  /**** ARing routines ******************************/
  /**************************************************/

  const Ring /* or null */ *rawARingZZp(unsigned long p); /* connected */
    /* Expects a prime number p in range 2 <= p <= 32749 */

  const Ring /* or null */ *rawARingGaloisField1(const RingElement *prim); /* connected */
  /* same interface as rawGaloisField, but uses different internal code */

  const Ring /* or null */ *rawARingGaloisFieldFlintBig(const RingElement *prim); /* connected */
  /* same interface as rawGaloisField, but uses Flint GF code designed for wordsize p, but too big 
     for lookup tables */

  const Ring /* or null */ *rawARingGaloisFieldFlintZech(const RingElement *prim); /* connected */
  /* same interface as rawGaloisField, but uses Flint GF code designed for wordsize p, 
     and uses lookup tables */

  const Ring /* or null */ *rawARingGaloisFieldFromQuotient(const RingElement *prim); /* connected */
  /* same interface as rawGaloisField, but uses Givaro */

  const Ring /* or null */ *rawARingGaloisField(int p, int n); /* connected */
  /* creates a ring GF(p^n).  Constraints on p, n? */
  /* returns null if the values p,n are too large  */


  M2_arrayintOrNull rawARingGFPolynomial(const Ring *R);
  /* given an ARingGF, return the coefficient array of the quotient polynoials.
     So, if R = kk[a]/(f(a)), where kk = ZZ/p, then the (integer) coefficients
     {f0, f1, f2, ..., f_(degree f)} is returned.
   */

  M2_arrayintOrNull rawARingGFCoefficients(const RingElement *f);
  /* f can be written as a polynomial in the generator a, e.g.
     f = f0 + a*f1 + ... + a^(d-1) * f_(d-1), where d = deg of the
     ring over ZZ/p.  This function returns {f0, f1, ..., f_(d-1)},
     where each entry is an integer */

  const RingElement*  rawMultiplicativeGenerator(const Ring *R);
  /* given an ARingGF, return the  the generator of the multiplicative group.
    */

  /**************************************************/
  /**** ARing flint routines ************************/
  /**************************************************/
  const Ring* /* or null */ rawARingZZFlint(); /* connected */

  const Ring* /* or null */ rawARingQQFlint(); /* connected */

  const Ring /* or null */ *rawARingZZpFlint(unsigned long p); /* connected */
    /* Expects a prime number p in range 2 <= p <= 2^64-1 */

  /* returns an array of non-negative integers, which represents the given Conway polynomial
     If there is none, a list of length 0 is returned.
     if the boolean argument is set to true, returns a random poly that flint finds 
  */
  M2_arrayintOrNull rawConwayPolynomial(long charac, 
                                        long deg, 
                                        M2_bool find_random_if_no_conway_poly_available);

  /**************************************************/
  /**** Ring routines *******************************/
  /**************************************************/
  unsigned int rawRingHash(const Ring *R);  /* drg: connected hash */
    /* assigned sequentially */

  M2_string IM2_Ring_to_string(const Ring *M); /* drg: connected */

  long rawRingCharacteristic(const Ring* R); /* connected: rawCharacteristic */

  const Ring *IM2_Ring_ZZ(void);  /* drg: connected rawZZ*/
    /* always returns the same object */

  const Ring *IM2_Ring_QQ(void);  /* drg: connected rawQQ */
    /* always returns the same object */

  const Ring /* or null */ *IM2_Ring_ZZp(int p); /* drg: connected rawZZp*/
    /* Expects a prime number p in range 2 <= p <= 32749 */

  const Ring /* or null */ *rawGaloisField(const RingElement *f); /* drg: connected to rawGaloisField */
    /* f should be a primitive element in a ring
       R = ZZ/p[x]/(g(x)), some p, some variable x, g irreducible,
       monic, deg >= 2.
       However, currently, not all of this is checked...
    */

  const Ring /* or null */ *IM2_Ring_RRR(unsigned long prec); /* drg: connected rawRRR */

  const Ring /* or null */ *IM2_Ring_CCC(unsigned long prec); /* drg: connected rawCCC */

  const Ring /* or null */ *IM2_Ring_polyring(const Ring *K,
                                      const Monoid *M); /* drg: connected rawPolynomialRing(,) */
  /* K can be either commutative or not. If K is a quotient ring, the relations are
   ignored.  Use rawQuotientRing to then make the desired quotient ring. */

  const Ring *IM2_Ring_trivial_polyring(); /* drg: connected rawPolynomialRing() */
  /* This returns the polynomial ring ZZ[], whose degree ring is itself */

  const Ring /* or null */ *IM2_Ring_skew_polyring(const Ring *R,
                                           M2_arrayint skewvars); /* drg: reconnected rawSkewPolynomialRing */
  /* R should be a polynomial ring K[M], where K is commutative.
     skewvars should be a list of variable indices (for the monoid M) indicating which ones
     are skew-commutative with each other. Further quotients (using IM2_Ring_quotient) are allowed */

  const Ring /* or null */ *IM2_Ring_weyl_algebra(const Ring *R,
                                          M2_arrayint comm_vars,
                                          M2_arrayint diff_vars,
                                          int homog_var); /* drg: reconnected rawWeylAlgebra*/
  /* R should be a polynomial ring K[M], where K is commutative.
     comm_vars and diff_vars should be arrays of the same length.  diff_vars[i] is the differential
     operator corresponding to comm_vars[i].  If homog_var is non-negative, then the multiplication
     is taken to be the homogeneous Weyl algebra (that is Dx*x = x*Dx + h^2, where h is the
     variable of R with index 'homog_var').
     Further quotients (using IM2_Ring_quotient) are allowed, however, one must make sure that
     the quotient is only by elements in the center of the ring.
  */

  const Ring /* or null */ *IM2_Ring_solvable_algebra(const Ring *R,
                                              const Matrix *Q); /* drg: connected rawSolvableAlgebra */
  /* R should be a polynomial ring K[M], where K is commutative.
     Q is a square matrix of size #gens(M).  If the variables are X_1, ..., X_r,
     then multiplication is defined to be X_j X_i = Q_(j,i), for j > i.
     The initial term of Q_(j,i) MUST be c * X_i * X_j, for some constant
     c (depending on i and j), or at least less than that in the monomial ordering.
  */

  const Ring /* or null */ *IM2_Ring_frac(const Ring *R); /* drg: connected rawFractionRing*/

  const Ring /* or null */ *IM2_Ring_localization(const Ring *R,
                                          Computation *P); /* drg: connected rawLocalRing */
  /* Create the localization of R.
     R should be a COMMUTATIVE ring.  P should be a one row matrix
     whose entries generate a prime ideal of R.
  */

  const Ring /* or null */ * IM2_Ring_quotient(const Ring *R,
                                       const Matrix *I); /*drg: connected rawQuotientRing */
  /* Given a quotient of an ambient poly ring R = A/J, and a GB I in R, form
     the quotient ring A/(I+J). */

  const Ring /* or null */ * IM2_Ring_quotient1(const Ring *R,
                                        const Ring *B); /*drg: connected rawQuotientRing */
  /* if R is a polynomial ring of the form A[x], and B = A/I (where A is a poly ring)
     then form the quotient ring B[x]. */

  const Ring /* or null */ *rawTowerRing1(long charac, M2_ArrayString names);
  /* Create a tower ring with the given variable names and characteristic */

  const Ring /* or null */ *rawTowerRing2(const Ring *R1, M2_ArrayString new_names);
  const Ring /* or null */ *rawTowerRing3(const Ring *R1, engine_RawRingElementArray eqns);

  const Ring /* or null */ *rawARingTower1(const Ring *R1, M2_ArrayString names);
  /* Create a tower ring with the given variable names and base ring */

  const Ring /* or null */ *rawARingTower2(const Ring *R1, M2_ArrayString new_names);
  const Ring /* or null */ *rawARingTower3(const Ring *R1, engine_RawRingElementArray eqns);


  const Ring /* or null */ *IM2_Ring_schur(const Ring *R); /* drg: reconnected rawSchurRing */

  const Ring *rawSchurRing2(const Ring *A, int n);

  const Ring *rawSchurRing1(const Ring *A);

  const Ring *rawSchurSnRing(const Ring *A, int n);

  M2_bool IM2_Ring_is_field(const Ring *K); /* drg: connected rawIsField*/
    /* Returns true if K is a field, or has been declared to be one.
       In the latter case, if an operation shows that K cannot be a field,
       then this function will thereafter return false, and
       rawGetNonUnit(K) can be used to obtain a non-unit, if one
       has been found. */

  M2_bool IM2_Ring_declare_field(const Ring *K); /* drg: connected rawDeclareField*/
    /* Declare that K is a field.  The ring K can then be used as the coefficient
       ring for computing Groebner bases,etc.  If false is returned, then
       the ring K has known non-units, and an error has been issued */

  const RingElement * rawGetNonUnit(const Ring *K); /* drg: connected rawGetNonUnit */
    /* Return a non-unit for the ring K, if one has been found, or the zero
       element, if not. Perhaps we should name this 'get_non_unit'.  This
       function currently never seems to return a non-zero value, but I plan
       on fixing that (MES, June 2002). */

  const Ring /* or null */ *rawAmbientRing(const Ring *R); /* drg: connected rawAmbientRing */
  /* If R is a quotient of a polynomial ring, or is a fraction ring, return the
     polynomial ring over a basic ring of which this is a quotient (or fraction ring) of.
     For example, if R = frac(ZZ[s,t]/(s^2-1))[x,y,z]/(s*x+t*y+z^2), then the returned
     ring is ZZ[s,t][x,y,z]. This routine is provided only for debugging the engine. */

  const Ring /* or null */ *rawDenominatorRing(const Ring *R); /* drg: connected rawDenominatorRing */
  /* If elements of R may have denominators, then this routine returns true, and
     the ambient ring for denominators returned. Otherwise, NULL
     is returned, which is not to be considered an error.  This routine is provided only for debugging the engine. */

  /**************************************************/
  /**** Ring element routines ***********************/
  /**************************************************/
  const RingElement *IM2_RingElement_from_Integer(const Ring *R,
                                                  gmp_ZZ d);  /* drg: connected rawFromNumber*/

  const RingElement /* or null */ *IM2_RingElement_from_rational(const Ring *R,
                                                         mpq_srcptr r); /* rawFromNumber*/

  gmp_ZZ /* or null */ IM2_RingElement_to_Integer(const RingElement *a); /* drg: connected rawToInteger*/
    /* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
       Otherwise, NULL is returned, and an error is given */

  gmp_QQ IM2_RingElement_to_rational(const RingElement *a); /* connected: rawToRational */
    /* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
       Otherwise, NULL is returned, and an error is given */

  const RingElement /* or null */ *IM2_RingElement_from_BigReal(const Ring *R,
                                                        gmp_RR d); /* drg: waiting, rawFromNumber*/

  const RingElement /* or null */ *IM2_RingElement_from_BigComplex(const Ring *R,
                                                        gmp_CC d); /* drg: waiting, rawFromNumber*/

  gmp_RRorNull IM2_RingElement_to_BigReal(const RingElement *a); /* drg: implemented, connected to rawToRR */
    /* If the ring of a is RR, this returns the underlying representation of 'a'.
       Otherwise NULL is returned. */

  gmp_CCorNull IM2_RingElement_to_BigComplex(const RingElement *a); /* drg: implemented, connected to rawToRR */


  const RingElement /* or null */ *IM2_RingElement_make_var(const Ring *R, int v); /* drg: connected rawRingVar*/

  M2_bool IM2_RingElement_is_zero(const RingElement *a); /* drg: connected rawIsZero*/

  M2_bool IM2_RingElement_is_equal(const RingElement *a,
                                   const RingElement *b); /* drg: connected === */

  const RingElement /* or null */ *IM2_RingElement_invert(const RingElement *a);/* TODO */

  engine_RawRingElementPair IM2_RingElement_divmod(const RingElement *a,
                                                 const RingElement *b); /* drg: connected rawDivMod*/


  int rawRingElementCompare(const RingElement *a,
                            const RingElement *b);
  /* Superficially compares two ring elements a,b from the same ring.  If the ring is
     a polynomial ring, then the lead flat monomials are compared.
     -1 means that a < b
     0 means that a == b
     1 means that a > b
     If the two rings are different, then 0 is returned (without error).  The front end never will call
     this function in that case though, as methods are installed for comparison on a ring by ring basis.
  */

  M2_string IM2_RingElement_to_string(const RingElement *a); /* drg: connected */

  unsigned int rawRingElementHash(const RingElement *a);/* connected */

  const Ring * IM2_RingElement_ring(const RingElement *a); /* drg: connected rawRing*/

  /**************************************************/
  /**** polynomial ring element routines ************/
  /**************************************************/

  M2_bool IM2_RingElement_is_graded(const RingElement *a); /* drg: connected rawIsHomogeneous*/

  M2_arrayintOrNull IM2_RingElement_multidegree(const RingElement *a); /* drg: connected rawMultiDegree*/

  gmp_ZZpairOrNull rawWeightRange(M2_arrayint wts, const RingElement *a); /* drg: connected rawWeightRange*/
    /* The first component of the degree is used, unless the degree monoid is trivial,
       in which case the degree of each variable is taken to be 1.
       Returns lo,hi degree.  If the ring is not a graded ring or a polynomial ring
       then (0,0) is returned.
    */

  const RingElement* /* or null */ rawRingElementAntipode(const RingElement* f);
  /* If the ring is not a skew commuting poly ring, this is the identity map.  Otherwise
     this returns a poly, with the signs of the coefficients possibly changed, 
     this implements the (anti-)isomorphism of the ring and its opposite ring.
  */

  const Matrix* /* or null */ rawHomogenizeMatrix(const Matrix* a, const Matrix* b, const Matrix* c);
  /* TEST dummy function!! */
  
  const RingElement /* or null */ *IM2_RingElement_homogenize_to_degree(
            const RingElement *a,
            int v,
            int deg,
            M2_arrayint wts); /* drg: connected rawHomogenize*/

  const RingElement /* or null */ *IM2_RingElement_homogenize(
            const RingElement *a,
            int v,
            M2_arrayint wts); /* drg: connected rawHomogenize*/

  const RingElement /* or null */ *IM2_RingElement_term(
            const Ring *R,
            const RingElement *a,
            const Monomial *m); /* drg: connected rawTerm*/
    /* R must be a polynomial ring, and 'a' an element of the
       coefficient ring of R.  Returns a*m, if this is a valid
       element of R.  Returns NULL if not (with an error message).
    */

  const RingElement /* or null */ *IM2_RingElement_get_terms(
            int nvars, /* n variables in an outermost monoid */
            const RingElement *a,
            int lo, int hi); /* drg: connected rawGetTerms*/
    /* Returns the sum of some monomials of 'a', starting at 'lo',
       going up to 'hi'.  If either of these are negative, they are indices
       from the back of the polynomial.
       'a' should be an element of a polynomial ring.
    */

  const RingElement /* or null */ *IM2_RingElement_get_coeff(
            const Ring * coeffRing, /* ring of the result */
            const RingElement *a,
            const Monomial *m); /* drg: connected rawCoefficient*/
    /* Return (as an element of the coefficient ring) the coeff
       of the monomial 'm'.
    */

  const RingElement /* or null */ *IM2_RingElement_lead_coeff(
            const Ring * coeffRing, /* ring of the result */
            const RingElement *a); /* drg: connected rawLeadCoefficient*/

  const Monomial /* or null */ *IM2_RingElement_lead_monomial(
            int nvars, /* number of variables in an outermost monoid */
            const RingElement *a); /* drg: connected rawLeadMonomial*/

  int IM2_RingElement_n_terms(
            int nvars, /* number of variables in an outermost monoid */
            const RingElement *a); /* drg: connected rawTermCount*/

  engine_RawArrayPairOrNull IM2_RingElement_list_form(
            const Ring * coeffRing, /* ring of the result coefficients */
            const RingElement *f); /* drg: connected rawPairs */

  engine_RawRingElementArrayOrNull rawConvolve(engine_RawRingElementArray H,
                                               int convolve_type);
  /* assumes: H[0]..H[n] are in a ring.
     returns the array determined by convolving H
     (see def in x-relem.cpp for more info)
  */

  engine_RawRingElementArray rawGetParts(const M2_arrayint wts,
                                const RingElement *f);
  /* Return an array of RingElement's, each having pure weight, and sorted by
     strictly increasing weight value.  The wt vector values must fit into
     a word length integer.  */

  const RingElement /* or null */ * rawGetPart(const M2_arrayint wts,
                                     const RingElement *f,
                                     M2_bool lobound_given,
                                     M2_bool hibound_given,
                                     long lobound,
                                     long hibound);
  /* Return the sum of all of the terms t of f, which satisfy: lobound <= wt.t <= hibound,
     where, if lobound_given is false, then lobound is -infinity, and if hibound_given
     is false, then hibound is infinity. */

  int IM2_RingElement_index_if_var(const RingElement *f); /* drg: connected rawIndexIfVariable */
  /* if f is a variable of its ring, then the index of that variable is returned.
     If f is not a variable, then -1 is returned. */

  M2_arrayint rawRingElementIndices(const RingElement *f); /* drg: connected rawIndices */
  /* The list of indices of variables which occur in f is returned. */

  const RingElement /* or null */ * rawAssociateDivisor(const RingElement *f);
  /* A unit 'a' in the base coefficient ring, such that a*f is the preferred associate of f.
     For example, if f = -9x+6 in QQ[x], then -3 is returned.
     If the (ultimate) base ring of f is QQ, then a*f has no denominators.
     If the base ring of f is frac(K[x]), K=ZZ,QQ, or another field, x is a set of vars
     then a*f is in ZZ[x], or K[x].
     If the base coefficient ring is the fraction ring of a quotient poly ring, then
     an error is flagged, and NULL is returned.
   */

  ////  Content ////////////////////////////////////////
  // The content of a ring element, polynomial or vector is defined to be an
  // element of the base (either a finite field, ZZ, QQ, or a fraction)
  // If base is ZZ: the result is the gcd of all of the terms appearing in the polynomial or vector
  //   The sign is chosen so that after division by the content the resulting polynomial is monic
  // If base is QQ or frac(A): the numerator is the gcd of all of the numerators,
  //                the denominator is the lcm of all of the denominators
  // If base is kk: the result is the lead coefficient.  For vectors, what should this mean?
  //////////////////////////////////////////////////////

  const RingElement /* or null */ *rawRingElementContent(const RingElement *f); /* connect to rawContent */
  // returns the content of f (as a matrix over the base coefficient ring)

  const RingElement /* or null */ *rawRingElementRemoveContent(const RingElement *f); /* connect to rawRemoveContent */
  // returns the polynomial which results after division by the content

  const RingElement /* or null */ *rawRingElementSplitContent(const RingElement *f, const RingElement /* or null */ **result); /* connect to rawSplitContent */
  // returns the content of f (as a matrix over the base coefficient ring)
  // result is set to the polynomial which results after division by the content

  /**************************************************/
  /**** fraction field ring element routines ********/
  /**************************************************/

  /** The numerator of a fraction.
   * \param a An element of a fraction ring frac(R).
   * \return The numerator of a, as an element of the ring R.
   *
   * Connected as rawNumerator.
   *
   */
  const RingElement *IM2_RingElement_numerator(const RingElement *a); /* drg: connected rawNumerator*/

  /** The denominator of a fraction.
   * \param a An element of a fraction ring frac(R).
   * \return The denominator of a, as an element of the ring R.
   *
   * Connected as rawDenominator.
   *
   */
  const RingElement *IM2_RingElement_denominator(const RingElement *a); /* drg: connected rawDenominator*/

  const RingElement /* or null */ *IM2_RingElement_fraction(const Ring *R,
                                                    const RingElement *a,
                                                    const RingElement *b); /* drg: connected rawFraction*/

  gmp_ZZ /* or null */ rawSchurDimension(const RingElement *f); /* connected rawSchurDimension */
  /* f should be a polynomial whose base ring was created using rawSchurRing
     (otherwise NULL is returned).  If so, the dimension of the corresponding
     (virtual) GL(n) representation is returned. */

  const RingElement /* or null */ *rawSchurFromPartition(const Ring *R, M2_arrayint part);
  /* if R is a SchurRing2, then return the element corresponding to the given partition */

  const RingElement /* or null */ *rawSchurSnTensorMult(const RingElement *f, const RingElement *g);
  /* the tensor multiplication function in SchurSnRing */

  long rawDiscreteLog(const RingElement *h); /* drg: connected */
  /* returns -1 if h is 0, or if elements of the ring of h
     are not represented as powers of a primitive element.
     Otherwise returns an integer in the range 0..q-1 */

  int rawDegree(int v, const RingElement *f); /* connected to rawDegree */
  /* Returns -1 if 0 or not implemented for a given ring.  For now, valid only for tower rings */

  int rawExtensionDegree(int v, const Ring *R); /* connected to rawExtensionDegree */
  /* Currently only valid for tower rings.  Others return 0.  */

  const RingElement /* or null */ *rawDiff(int v, const RingElement *f);  /* connected */
  /* Currently only valid for tower rings */

  const RingElement /* or null */ *rawLowerP(const RingElement *f);  /* connected */
  /* Currently only valid for tower rings */

  const RingElement *rawTowerTranslatePoly(const Ring *newRing, const RingElement *F);
  /* 2 cases: ring of F is a polynomial ring, and newRing is a Tower.
     second case: ring of F is a tower, and newRing is a polynomial ring.
     In both cases, the two rings should have the same characteristic, and the same number of variables.
     This then translates F, returning the translated poly in the ring newRing.
  */

  const RingElement /* or null */ *rawPowerMod(const RingElement *f, mpz_srcptr n, const RingElement *g);  /* connected */
  /* Currently only valid for tower rings */

  /**************************************************/
  /**** FreeModule routines *************************/
  /**************************************************/
  /* A FreeModule in the engine is always over a specific
     ring, and is graded using the degree monoid of the ring.
     Monomials in a free module are ordered, either in a way
     determined by the ordering in the ring, or using an induced
     (Schreyer) monomial ordering (in the case when the ring
     is a polynomial ring of some sort) */
  /* General notes: these are immutable obects, at least once
     they are returned by the engine */
  /* BUGS/TODO: the Schreyer orders produced by sum,tensor,
     symm,exterior, and submodule, ignore the current tie
     breaker values.
     Also: I might keep all freemodules for a ring unique.
     This is not currently done. */

  const Ring *
  IM2_FreeModule_ring(
          const FreeModule *F); /* drg: connected rawRing*/

  int
  IM2_FreeModule_rank(
          const FreeModule *F); /* drg: connected rawRank*/

  M2_string
  IM2_FreeModule_to_string(
          const FreeModule *F); /* drg: connected */

  unsigned int
  rawFreeModuleHash(
          const FreeModule *F); /* not quite connected */

  const FreeModule /* or null */ *
  IM2_FreeModule_make(
          const Ring *R,
          int rank); /* drg: connected rawFreeModule*/

  const FreeModule /* or null */ *
  IM2_FreeModule_make_degs(
          const Ring *R,
          M2_arrayint degs); /* drg: connected rawFreeModule*/
    /* Make a graded free module over R.  'degs' should be of length
     * divisible by the length 'd' of a degree vector, and the
     * i th degree will be degs[i*d]..degs[i*d+d-1], starting at
     * i = 0.
     */

  const FreeModule /* or null */ *
  IM2_FreeModule_make_schreyer(
          const Matrix *m); /* drg: connected rawSchreyerSource */
    /* Returns G, (a copy of) the source free module of 'm', modified to
     * use the induced order via m: compare two monomials of G via
     * x^A e_i > x^B e_j iff either
     * leadmonomial((in m)(x^A e_i)) > leadmonomial((in m)(x^B e_j))
     * or these are the same monomial, and i > j.
     * The case where the target of 'm' has a Schreyer order is
     * handled efficiently.
     */

  M2_arrayint
  IM2_FreeModule_get_degrees(
          const FreeModule *F); /* drg: connected rawMultiDegree*/

  const Matrix *
  IM2_FreeModule_get_schreyer(
          const FreeModule *F); /* drg: connected rawGetSchreyer*/

  M2_bool
  IM2_FreeModule_is_equal(
          const FreeModule *F,
          const FreeModule *G); /* drg: connected === */
    /* Determines if F and G are the same graded module.  If one has a
     * Schreyer order and one does not, but their ranks and degrees are the
     * same, then they are considered equal by this routine.
     */

  const FreeModule /* or null */ *
  IM2_FreeModule_sum(
          const FreeModule *F,
          const FreeModule *G); /* drg: connected rawDirectSum */
    /* The direct sum of two free modules over the same ring, or NULL.
     * If F or G has a Schreyer order, then so does their direct sum
     */

  const FreeModule /* or null */ *
  IM2_FreeModule_tensor(
          const FreeModule *F,
          const FreeModule *G); /* drg: connected rawTensor*/
    /* The tensor product of two free modules over the same ring, or NULL.
     * If F has (ordered basis {f_1,...,f_r}, and
     * G has (ordered) basis {g_1, ..., g_s}, then
     * the result has (ordered) basis
     *    {f_1 ** g_1, f_1 ** g_2, ..., f_1 ** g_s,
     *     f_2 ** g_1, f_2 ** g_2, ..., f_2 ** g_s,
     *     ...
     *     f_r ** g_1, ...              f_r ** g_s}.
     *  If F or G has a Schreyer order, what about their tensor product?
     *  At the moment, the answer is almost yes...
     */

  const FreeModule /* or null */ *IM2_FreeModule_dual(
          const FreeModule *F); /* drg: connected rawDual*/
    /* Returns the graded dual F^* of F: if F has basis {f_1,...,f_r},
     * with degrees {d_1, ..., d_r}, then F^* has rank r, with
     * degrees {-d_1, ..., -d_r}.  The result does not have a
     * Schreyer order (even if F does).
     */

  const FreeModule *
  IM2_FreeModule_symm(
          int n,
          const FreeModule *F); /* drg: connected rawSymmetricPower*/
    /* Returns the n th symmetric power G of F.
     * If F has basis {f_1,...,f_r}, then G has basis
     * the monomials of f_1, ..., f_r of degree exactly n, in
     * descending lexicographic order.
     * If F has a Schreyer order, then G is set to have one as well.
     */


  const FreeModule *
  IM2_FreeModule_exterior(
          int n,
          const FreeModule *F); /* drg: connected rawExteriorPower*/
    /* Returns the n th exterior power G of F.
     * If F has basis {f_1,...,f_r}, then G has basis
     * the squarefree monomials of f_1, ..., f_r of degree exactly n, in
     * descending reverse lexicographic order.
     * If F has a Schreyer order, then G is set to have one as well.
     */

  const FreeModule /* or null */ *
  IM2_FreeModule_submodule(
          const FreeModule *F,
          M2_arrayint selection); /* drg: connected rawSubmodule*/
    /* Returns a free module obtained by choosing basis elements of F:
     * if F has basis {f_0, ..., f_(r-1)} with degrees {d_0, ..,d_(r-1)},
     * and selection = [i_1, ..., i_s], where 0 <= i_j < r for all j,
     * then return a free module of rank s, having degrees
     * d_(i_1), ..., d_(i_s).  'selection' may include duplicate values.
     * If F has a Schreyer order, the result has one as well.
     */

  M2_arrayintOrNull rawFreeModuleSelectByDegrees(const FreeModule* F,
                                                 M2_arrayint lo,
                                                 M2_arrayint hi); 
  /* If F_i has multi-degree >= lo, AND <= hi, then add i to the result
     IF: lo has length 0, then treat that as -infinity in each component.
     Same with hi.
  */

  /**************************************************/
  /**** Matrix routines *****************************/
  /**************************************************/

  const FreeModule * IM2_Matrix_get_target(const Matrix *M); /* drg: connected rawTarget*/

  const FreeModule * IM2_Matrix_get_source(const Matrix *M); /* drg: connected rawSource, used in rawMatrixColumns*/

  int IM2_Matrix_n_rows(const Matrix *M); /* drg: connected rawNumberOfRows*/

  int IM2_Matrix_n_cols(const Matrix *M); /* drg: connected rawNumberOfColumns*/

  M2_arrayint IM2_Matrix_get_degree(const Matrix *M); /* drg: connected rawMultiDegree*/

  M2_string IM2_Matrix_to_string(const Matrix *M); /* drg: connected */

  unsigned int rawMatrixHash(const Matrix *M); /* drg: connected to "hash"  */

  const RingElement /* or null */ * IM2_Matrix_get_entry(const Matrix *M, int r, int c); /* drg: connected rawMatrixEntry, OK*/

  /*******************************************************************************/
  const Matrix * IM2_Matrix_identity(const FreeModule *F,
                                     int preference
                                     ); /* drg: connected rawIdentity, OK*/

  const Matrix /* or null */ * IM2_Matrix_zero(const FreeModule *F,
                                       const FreeModule *G,
                                       int preference
                                       ); /* drg: connected rawZero, OK */

  const Matrix /* or null */ * IM2_Matrix_make1(const FreeModule *target,
                                        int ncols,
                                        const engine_RawRingElementArray M,
                                        int preference); /* drg: connected rawMatrix1, OK */

  const Matrix /* or null */ * IM2_Matrix_make2(const FreeModule *target,
                                        const FreeModule *source,
                                        M2_arrayint deg,
                                        const engine_RawRingElementArray M,
                                        int preference); /* drg: connected rawMatrix2, OK */

  const Matrix /* or null */ * IM2_Matrix_make_sparse1(const FreeModule *target,
                                               int ncols,
                                               M2_arrayint rows,
                                               M2_arrayint cols,
                                               const engine_RawRingElementArray entries,
                                               int preference); /* drg: connected rawSparseMatrix1, OK */

  const Matrix /* or null */ * IM2_Matrix_make_sparse2(const FreeModule *target,
                                               const FreeModule *source,
                                               M2_arrayint deg,
                                               M2_arrayint rows,
                                               M2_arrayint cols,
                                               const engine_RawRingElementArray entries,
                                               int preference); /* drg: connected rawSparseMatrix2, OK */

  M2_bool IM2_Matrix_is_implemented_as_dense(const Matrix *M); /* connected to rawIsDense */
  /* Is the matrix M implemented in the engine as a dense matrix? */

  const Matrix /* or null */ * IM2_Matrix_remake1(const FreeModule *target,
                                          const Matrix *M,
                                          int preference
                                          ); /* drg: connected rawMatrixRemake1, OK  */
  /* Create a new matrix (mutable or immutable), from M, with new target,
     and/or mutable-ness. The target free module must have the expected rank.
     The source free module is computed heuristically from the the target and the
     columns of the matrix.
  */

  const Matrix /* or null */ * IM2_Matrix_remake2(const FreeModule *target,
                                          const FreeModule *source,
                                          M2_arrayint deg,
                                          const Matrix *M,
                                          int preference
                                          ); /* drg: connected rawMatrixRemake2, OK */
  /* Create a new matrix (mutable or immutable), from M, with new target,
     source, deg and/or mutable-ness. The new free modules must have
     the expected rank.
  */

  const Matrix /* or null */ *IM2_Matrix_random(const Ring *R,
                                  int r, int c,
                                  double fraction_non_zero,
                                  int special_type, /* 0: general, 1:upper triangular, others? */
                                  int preference); /* connected to rawRandomConstantMatrix, OK */

  /**********************************************************************************/

  M2_bool IM2_Matrix_is_zero(const Matrix *M); /* drg: connected rawIsZero*/

  int IM2_Matrix_is_equal(const Matrix *M, const Matrix *N); /* drg: connected === and to rawIsEqual for use with == */
        // 1 = true, 0 = false, -1 = error
    /* This checks that the entries of M,N are the same, as well as
       that the source and target are the same (as graded free modules).
       Therefore, it can happen that M-N == 0, but M != N.
    */

  M2_bool IM2_Matrix_is_graded(const Matrix *M); /* drg: connected rawIsHomogeneous*/

  const Matrix /* or null */ * IM2_Matrix_concat(const engine_RawMatrixArray Ms); /* drg: connected rawConcat*/

  const Matrix /* or null */ * IM2_Matrix_direct_sum(const engine_RawMatrixArray Ms); /* drg: connected rawDirectSum*/

  const Matrix /* or null */ * IM2_Matrix_tensor(const Matrix *M,
                                         const Matrix *N); /* drg: connected rawTensor*/

  const Matrix /* or null */ * IM2_Matrix_transpose(const Matrix *M); /* drg: connected rawDual*/

  const Matrix /* or null */ * IM2_Matrix_reshape(const Matrix *M,
                                          const FreeModule *F,
                                          const FreeModule *G); /* drg: connected rawReshape*/

  const Matrix /* or null */ * IM2_Matrix_flip(const FreeModule *F,
                                       const FreeModule *G); /* drg: connected rawFlip*/

  const Matrix /* or null */ * rawWedgeProduct(int p,
                                       int q,
                                       const FreeModule *F); /* drg: connected rawWedgeProduct */
  /* Constructs the map
     exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
  */

  const Matrix /* or null */ * IM2_Matrix_submatrix(const Matrix *M,
                                            M2_arrayint rows,
                                            M2_arrayint cols); /* drg: connected rawSubmatrix*/

  const Matrix /* or null */ * IM2_Matrix_submatrix1(const Matrix *M,
                                             M2_arrayint cols); /* drg: connected rawSubmatrix*/

  const Matrix /* or null */ * IM2_Matrix_koszul(int p, const Matrix *M); /* drg: connected rawKoszul*/

  const Matrix /* or null */ *
  rawKoszulMonomials(int nskew,
                     const Matrix *M,
                     const Matrix *N); /* drg: connected rawKoszulMonomials */
  /* M and N should each have one row, and the base ring should be a
     polynomial ring.  The (i,j) th entry of the resulting matrix is
     1 or -1 times N_j/M_i (if M_i divides N_j). The sign is determined only from
     the first nskew variables.  The sign is the sign of M_i * (N_j/M_i) in
     exterior algebra (on this set of variables).  The actual commutativity of the
     common ring of M and N is ignored. */

  const Matrix /* or null */ * IM2_Matrix_symm(int p, const Matrix *M); /* drg: connected rawSymmetricPower*/

  const Matrix /* or null */ * IM2_Matrix_exterior(int p, const Matrix *M, int strategy); /* drg: connected rawExteriorPower*/

  M2_arrayint IM2_Matrix_sort_columns(const Matrix *M,
                                            int deg_order,
                                            int mon_order); /* drg: connected rawSortColumns*/


  const Matrix /* or null */ * IM2_Matrix_minors(int p, const Matrix *M, int strategy); /* drg: unconnected*/

  const Matrix /* or null */ * rawMinors(int p,
                           const Matrix *M,
                           int strategy,
                           int n_minors_to_compute, /* -1 means all */
                           M2_arrayintOrNull first_row_set,
                           M2_arrayintOrNull first_col_set
                           ); /* connected to rawMinors */
  /* If first_row_set or first_col_set is not NULL, they should both be non-NULL,
     and both have length p.  If not, NULL is returned.
     Compute n_minors_to_compute minors, starting at (first_row_set,first_col_set) if given,
     otherwise starting at the first (0..p-1,0..p-1).
  */

  const Matrix /* or null */ * IM2_Matrix_pfaffians(int p, const Matrix *M); /* drg: connected rawPfaffians*/

  const Matrix * rawMatrixCompress(const Matrix *M); /* connected rawMatrixCompress */

  const Matrix /* or null */ * IM2_Matrix_uniquify(const Matrix *M); /* TODO */
  /* if any two columns are the same up to a scalar multiple, then keep only
     one of the columns.  Remove any zero columns too.
     The definition of "same up to a scalar" is this:
     if K is the base field or ring (i.e. QQ in QQ(x,y,z)[s,t]),
     and if c and d are the lead scalar coeff's of vecs v,w, resp, then
     v and w are scalar multiplies iff d*v == c*w.
     Warning: Over non-domains, this might not be the intended effect.
  */

  const Matrix * rawRemoveScalarMultiples(const Matrix *m); /* connected */

  const Matrix * rawRemoveMonomialFactors(const Matrix *m, M2_bool make_squarefree_only);
  /* connected to rawRemoveMonomialFactors */

  ////  Content ////////////////////////////////////////
  // The content of a ring element, polynomial or vector is defined to be an
  // element of the base (either a finite field, ZZ, QQ, or a fraction)
  // If base is ZZ: the result is the gcd of all of the terms appearing in the polynomial or vector
  //   The sign is chosen so that after division by the content the resulting polynomial is monic
  // If base is QQ or frac(A): the numerator is the gcd of all of the numerators,
  //                the denominator is the lcm of all of the denominators
  // If base is kk: the result is the lead coefficient.  For vectors, what should this mean?
  //////////////////////////////////////////////////////

  const Matrix /* or null */ *rawMatrixContent(const Matrix *M); /* connect to rawContent */
  // returns the matrix of the content of each column of M

  const Matrix /* or null */ *rawMatrixRemoveContent(const Matrix *M); /* connect to rawRemoveContent */
  // returns the matrix with the content (as defined above) removed

  const Matrix /* or null */ *rawMatrixSplitContent(const Matrix *M, const Matrix /* or null */ **result); /* connect to rawSplitContent */
  // returns the matrix of the content of each column of M,
  // and result is set to the result of rawMatrixRemoveContent.

  const Matrix /* or null */ * IM2_Matrix_remove_content(const Matrix *M);      /* connected rawRemoveContent*/

  /* Routines for use when the base ring is a polynomial ring of some sort */

  const Matrix /* or null */ * IM2_Matrix_diff(const Matrix *M,
                                       const Matrix *N); /* drg: connected rawMatrixDiff*/

  const Matrix /* or null */ * IM2_Matrix_contract(const Matrix *M,
                                           const Matrix *N); /* drg: connected rawMatrixContract*/

  const Matrix /* or null */ * IM2_Matrix_homogenize(const Matrix *M,
                                             int var,
                                             M2_arrayint wts); /* drg: connected rawHomogenize*/

  const struct engine_RawMatrixPair_struct /* or null */ *IM2_Matrix_coeffs(const Matrix *M, M2_arrayint vars) ;/* TODO */


  const Matrix /* or null */ * rawCoefficients(M2_arrayint vars,
                                       const Matrix *monoms,
                                       const Matrix *M); /* drg: connected as rawCoefficients*/
  /* Given:
   *  vars : a list of variable indices in the (common) ring R of monoms and M
   *  monoms : a map R^b --> R^a such that each column has exactly one monomial
   *      which is only in the variables in 'vars'.
   *  M : a map R^c --> R^a such that every (module) monomial of each column of M
   *      matches one of the columns of 'monoms', in the variables 'vars'.
   *
   * Returns: a matrix C : R^c --> R^b such that
   *      (i) the entries of C do not involve the variables in 'vars', and
   *      (ii) monoms * C == M
   *
   * Assumptions on rings: if R is non-commutative, then the variables in 'vars'
   *      should commute with the variables outside of 'vars'.
   *
   * If each column of monoms has more than one monomial, or if variables other than
   *      those in 'vars' occur, then only the first monomial is used, and the other
   *      variables are ignored.
   * If a monomial occurs twice, then one of them will be used (which one is left undefined)
   */

  const Matrix /* or null */ * IM2_Matrix_monomials(M2_arrayint vars, const Matrix *M); /* drg: connected rawMonomials*/

  const Matrix * IM2_Matrix_initial(int nparts, const Matrix *M); /* drg: connected rawInitial*/

  M2_arrayint IM2_Matrix_elim_vars(int nparts, const Matrix *M); /* drg: connected rawEliminateVariables*/

  M2_arrayint IM2_Matrix_keep_vars(int nparts, const Matrix *M); /* drg: connected rawKeepVariables*/

  engine_RawMatrixAndInt IM2_Matrix_divide_by_var(const Matrix *M, int var, int maxdegree); /* drg: connected rawDivideByVariable*/
  /* If M = [v1, ..., vn], and x = 'var'th variable in the ring,
     return the matrix [w1,...,wn], where wi * x^(ai) = vi,
     and wi is not divisible by x, or ai = maxdegree,
     and the integer which is the maximum of the ai's.
     QUESTION: what rings should this work over?
  */

  engine_RawMatrixPairOrNull rawTopCoefficients(const Matrix *M); /* connected to rawTopCoefficients */
  /* Returns a pair of matrices: the first is a list of monomials (of form var^exp),
     and the second has the same row space as M.  For each column, find the smallest
     index variable, var,  which occurs, and exp, the largest degree to which it occurs
     in that column.  Place var^exp in the first matrix.
     Place the coeff of var^exp (a vector) into the second matrix.
     If the ring is not a polynomial ring, an error is given, and Null is returned.
  */

  M2_arrayintOrNull rawMatrixIndices(const Matrix *f); /* connected to rawIndices */

  M2_arrayint IM2_Matrix_min_leadterms(const Matrix *M, M2_arrayint vars); /* TODO */

  const Matrix /* or null */ * IM2_Matrix_auto_reduce(const Matrix *M); /* TODO */

  const Matrix /* or null */ * IM2_Matrix_reduce(const Matrix *M, const Matrix *N); /* TODO */

  const Matrix /* or null */ * IM2_Matrix_reduce_by_ideal(const Matrix *M, const Matrix *N); /* TODO */

  /* Routines when considering matrices as modules of some sort */

  const Matrix /* or null */ * rawModuleTensor(const Matrix *M,
                                       const Matrix *N); /* connected rawModuleTensor */

  const Matrix /* or null */ * rawBasis(const Matrix *M,
                                M2_arrayint lo_degree, /* possibly length 0 */
                                M2_arrayint hi_degree,
                                M2_arrayint wt,
                                M2_arrayint vars,
                                M2_bool do_truncation,
                                int limit); /* connected to rawBasis */
  /* Yields a monomial basis of part of the graded R-module cokernel(M).
   * Returns a matrix of monomials which maps to the target of M, such that
   *  (i) The image spans the sum of M_i, for lo_degree <= i <= hi_degree
   *       where M_i is the degree i piece of M.
   * Notes:
   *  -- 'vars' is a list of variables.  The entries of the result will only involve
   *     these variables.
   *  -- 'wt' should be a list of integers of length <= number of degrees,
   *     with the property that each variable in 'vars' has (its degree) dot wt > 0.
   *  -- if lo_degree has length 0, then it is assumed to be -infinity
   *  -- if hi_degree has length 0, then it is assumed to be infinity
   *  -- in either of these cases, or if lo_degree is not equal to hi_degree, then
   *     the degree ring of R must have one variable.
   *  -- if limit >= 0, then only the first 'limit' monomials are placed into the result.
   *  -- if do_truncation is set, then monomials of degree higher than hi_degree will be
   *     placed into .
   *
   * If R is a quotient ring, then the monomial order had better be a product order
   * such that the first block (or blocks) consists of the variables in 'vars'.
   *
   */

  int IM2_Matrix_dimension(const Matrix *M); /* TODO */

  const RingElement /* or null */ * IM2_Matrix_Hilbert(const Matrix *M); /* drg: connected rawHilbert*/
  /* This routine computes the numerator of the Hilbert series
     for coker leadterms(M), using the degrees of the rows of M.
     NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */

  const Matrix * IM2_kernel_of_GB(const Matrix *G); /* connected rawKernelOfGB */
  /* Assuming that the columns of G form a GB, this computes
     a Groebner basis of the kernel of these elements, using an appropriate Schreyer order on the
     source of G. */


  /**************************************************/
  /**** RingMap routines ****************************/
  /**************************************************/
  /* My plan, Dan, is to make changes to how ring maps are
     constructed, in the case when we have rings over polynomials
     rings (including galois field bases) */

  const Ring * IM2_RingMap_target(const RingMap *F); /* drg: connected rawTarget*/

  M2_string IM2_RingMap_to_string(const RingMap *F); /* drg: connected */

  unsigned int rawRingMapHash(const RingMap *F); /* TODO */ 

  M2_bool IM2_RingMap_is_equal(const RingMap*, const RingMap*); /* drg: connected === */

  const RingMap * IM2_RingMap_make(const Matrix *M, const Ring *base); /* TODO */

  const RingMap * IM2_RingMap_make1(const Matrix *M); /* drg: connected rawRingMap */
  /* WARNING: I want to change the interface to this routine */

  const RingElement /* or null */ * IM2_RingMap_eval_ringelem(const RingMap *F,
                                                      const RingElement *a); /* drg: connected rawRingMapEval*/

  const Matrix /* or null */ * IM2_RingMap_eval_matrix(const RingMap *F,
                                               const FreeModule *newTarget,
                                               const Matrix *M); /* drg: connected rawRingMapEval*/

  MutableMatrix /* or null */ * rawRingMapEvalMutableMatrix(const RingMap* F,
                                                            const MutableMatrix* M);
  /* drg: connected rawRingMapEval*/

  const RingElement *IM2_RingElement_promote(const Ring *S, const RingElement *f); /* drg: connected rawPromote*/

  const RingElement /* or null */ *IM2_RingElement_lift(int *success_return, const Ring *S, const RingElement *f); /* drg: connected rawLift*/
  // returns null if lifting not possible

    /* Is this documentation correct for promote and lift?
       We have several ways of moving from one ring to the next:
       R ---> R[x1..xn]
       R ---> R/I
       R ---> frac R
       Z/p[x]/F(x) ---> GF(p,n)
       R ---> local(R,I)    (much later...)

       Both of the following routines assume that S ---> 'this'
       is one of these construction steps.  Promote takes an element of
       S, and maps it into 'this', while lift goes the other way.
    */

  const Matrix /* or null */ *IM2_Matrix_promote(const FreeModule *newTarget, const Matrix *f); /* connected to rawPromote*/

  const Matrix /* or null */ *IM2_Matrix_lift(int *success_return, const FreeModule *newTarget, const Matrix *f); /* connected to rawLift */
  // returns null if lifting not possible

  /**************************************************/
  /**** MutableMatrix routines **********************/
  /**************************************************/

  MutableMatrix * IM2_MutableMatrix_identity(const Ring *R,
                                             int nrows,
                                             M2_bool prefer_dense); /* drg: connected rawMutableIdentity, OK*/

  MutableMatrix * IM2_MutableMatrix_make(const Ring *R,
                                         int nrows,
                                         int ncols,
                                         M2_bool prefer_dense); /* drg: connected rawMutableMatrix, OK */

  MutableMatrix * IM2_MutableMatrix_from_matrix(const Matrix *M, M2_bool prefer_dense); /* drg: connected rawMutableMatrix, OK*/


  const Matrix * IM2_MutableMatrix_to_matrix(const MutableMatrix *M); /* drg: connected rawMatrix, OK*/

  M2_string IM2_MutableMatrix_to_string(const MutableMatrix *M); /* drg: connected toString, OK */

  unsigned int  rawMutableMatrixHash(const MutableMatrix *M); /* drg: connected to "hash" */

  int IM2_MutableMatrix_n_rows(const MutableMatrix *M); /* drg: connected rawNumberOfRows, OK */

  int IM2_MutableMatrix_n_cols(const MutableMatrix *M); /* drg: connected rawNumberOfColumns, OK */

  void rawMutableMatrixFillRandomDensity(MutableMatrix *M, double density, int special_type);
  /* drg: connected rawMutableMatrixFillRandom */
  /* special_type: 0 is general, 1 is (strictly) upper triangular. */

  void rawMutableMatrixFillRandom(MutableMatrix *M, long nelems);
  /* drg: connected rawMutableMatrixFillRandom */

  MutableMatrix /* or null */ *rawMutableMatrixPromote(const Ring *R, const MutableMatrix *f); /* connected to rawPromote*/

  MutableMatrix /* or null */ *rawMutableMatrixLift(int *success_return, const Ring* R, const MutableMatrix *f); /* connected to rawLift */
  // returns null if lifting not possible

  const RingElement /* or null */ * IM2_MutableMatrix_get_entry(const MutableMatrix *M,
                                                        int r, int c); /* drg: connected rawMatrixEntry, OK*/

  /* Each of these routines returns false if there was an error. */

  M2_bool IM2_MutableMatrix_set_entry(MutableMatrix *M, int r, int c, const RingElement *a); /* drg: connected rawSetMatrixEntry, OK */

  M2_bool IM2_MutableMatrix_row_swap(MutableMatrix *M, int i, int j); /* drg: connected rawMatrixRowSwap, OK */

  M2_bool IM2_MutableMatrix_column_swap(MutableMatrix *M, int i, int j); /* drg: connected rawMatrixColSwap, OK*/

  M2_bool IM2_MutableMatrix_row_operation(MutableMatrix *M,
                                          int i,
                                          const RingElement *r,
                                          int j,
                                          M2_bool opposite_mult); /* drg: connected rawMatrixRowChange, OK */
  /* row(i) <- row(i) + r * row(j), returns false if matrix is
     immutable, or rows are out of bounds */

  M2_bool IM2_MutableMatrix_column_operation(MutableMatrix *M,
                                             int i,
                                             const RingElement *r,
                                             int j,
                                             M2_bool opposite_mult); /* drg: connected rawMatrixColChange, OK*/
  /* column(i) <- column(i) + r * column(j), returns false if matrix is
     immutable, or columns are out of bounds */

  M2_bool IM2_MutableMatrix_row_scale(MutableMatrix *M,
                                      const RingElement *r,
                                      int i,
                                      M2_bool opposite_mult); /* drg: connected rawMatrixRowScale, OK*/
  /* row(i) <- r * row(i), returns false if matrix is immutable
     or row is out of bounds */

  M2_bool IM2_MutableMatrix_column_scale(MutableMatrix *M,
                                         const RingElement *r,
                                         int i,
                                         M2_bool opposite_mult); /* drg: connected rawMatrixColumnScale, OK */
  /* column(i) <- r * column(i), returns false if matrix is immutable
     or row is out of bounds */

  M2_bool IM2_MutableMatrix_insert_columns(MutableMatrix *M, int i, int n_to_add); /* connected to rawInsertColumns, OK */
  /* Insert n_to_add columns directly BEFORE column i. */

  M2_bool IM2_MutableMatrix_insert_rows(MutableMatrix *M, int i, int n_to_add); /* connected to rawInsertRows, OK */
  /* Insert n_to_add rows directly BEFORE row i. */

  M2_bool IM2_MutableMatrix_delete_columns(MutableMatrix *M, int i, int j); /* connected to rawDeleteColumns, OK */
  /* Delete columns i .. j from M */

  M2_bool IM2_MutableMatrix_delete_rows(MutableMatrix *M, int i, int j); /* connected to rawDeleteRows, OK  */
  /* Delete rows i .. j from M */


  M2_bool IM2_MutableMatrix_column_2by2(MutableMatrix *M,
                                        int c1, int c2,
                                        const RingElement *a1, const RingElement *a2,
                                        const RingElement *b1, const RingElement *b2,
                                        M2_bool opposite_mult); /* connected to rawMatrixColumnOperation2, OK */
  /* column(c1) <- a1 * column(c1) + a2 * column(c2)
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */

  M2_bool IM2_MutableMatrix_row_2by2(MutableMatrix *M,
                                     int r1, int r2,
                                     const RingElement *a1, const RingElement *a2,
                                     const RingElement *b1, const RingElement *b2,
                                     M2_bool opposite_mult); /* connected to rawMatrixRowOperation2, OK */
  /* row(r1) <- a1 * row(r1) + a2 * row(r2)
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */

  M2_bool IM2_MutableMatrix_sort_columns(MutableMatrix *M, int lo, int hi); /* connected to rawSortColumns2, OK */
  /* Returns false if M is not mutable, or lo, or hi are out of range */

  M2_bool IM2_MutableMatrix_row_permute(MutableMatrix *M,
                                        int start,
                                        M2_arrayint perm); /* connected to rawPermuteRows, OK */
  /* if perm = [p0 .. pr], then row(start + i) --> row(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */

  M2_bool IM2_MutableMatrix_column_permute(MutableMatrix *M,
                                           int start,
                                           M2_arrayint perm); /* connected to rawPermuteColumns, OK */
  /* if perm = [p0 .. pr], then column(start + i) --> column(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */

  const RingElement * IM2_Matrix_dot_product(const MutableMatrix *M, int c1, int c2); /* connected to rawColumnDotProduct */
  /* Return the dot product of columns c1 and c2 of the matrix M.  If either c1 or c2 is
     out of range, 0 is returned. */

  /**
     Is the matrix implemented as a contiguous array of elements?
   */
  M2_bool rawMutableMatrixIsDense(const MutableMatrix *M);

  M2_bool IM2_MutableMatrix_is_zero(const MutableMatrix *M); /* drg: connected rawIsZero, OK */

  M2_bool IM2_MutableMatrix_is_equal(const MutableMatrix *M,
                                           const MutableMatrix *N); /* drg: connected to rawIsEqual for use with ==, not connected to '===', OK */
  /* This checks that the entries of M,N are the same */

  MutableMatrix * IM2_MutableMatrix_copy(MutableMatrix *M, M2_bool prefer_dense); /* connected to rawMutableMatrix, OK */

  M2_bool IM2_MutableMatrix_set_values(MutableMatrix *M,
                                       M2_arrayint rows,
                                       M2_arrayint cols,
                                       engine_RawRingElementArray values); /* connected to rawSetMatrixValues, OK */
  /* Given three arrays of the same length, 'rows', 'cols', 'values', set the
     corresponding values of M.  If any elements are out of range, ignore those
     triples.  If the type of ring element is not valid, or the sizes of the
     matrices do not match, return false. */

  MutableMatrix /* or null */ * IM2_MutableMatrix_submatrix(const MutableMatrix *M,
                                                    M2_arrayint rows,
                                                    M2_arrayint cols); /* drg: connected rawSubmatrix, OK */

  MutableMatrix /* or null */ * IM2_MutableMatrix_submatrix1(const MutableMatrix *M,
                                                     M2_arrayint cols); /* drg: connected rawSubmatrix, OK */


  M2_bool IM2_MutableMatrix_reduce_by_pivots(MutableMatrix *M); /* connected rawReduceByPivots */
  /* Using row and column operations, use unit pivots to reduce the matrix */
  /* A return value of false means that the computation was interrupted */

  /** return the transpose of A */
  MutableMatrix* rawMutableMatrixTranspose(MutableMatrix* A);

  /**
     returns the rank of the matrix M.  If 'rank' is not defined on this type of matrix,
     then returns -1 (and an error message is given).
   */
  long rawLinAlgRank(MutableMatrix* M);

  /** requires: M should be a square matrix.  
      If not, or if the ring has not implemented this routine,
      then null is returned (and an error message is given).
   */
  const RingElement* rawLinAlgDeterminant(MutableMatrix* A);

  MutableMatrix* rawLinAlgInverse(MutableMatrix* A);

  /** compute the row reduced echelon form of the matrix A.
      This is a matrix of the same shape as A.
      NULL, and an error, is returned if the ring is not
      equipped to compute this, or if it has not been
      implemented for that ring type yet
  */
  MutableMatrix* rawLinAlgRREF(MutableMatrix* A);

  M2_arrayintOrNull rawLinAlgRankProfile(MutableMatrix* A, M2_bool row_profile);

  MutableMatrix* rawLinAlgNullSpace(MutableMatrix* A);

  /** Returns X s.t. AX = B.  Assumptions: 
      A has the same number of rows as B. A doesn't have to be invertible or square.
      If a usage error occurs, NULL is returned and 'success' is set to 0.
      In all other cases, 'success' is set to 1.
      If AX=B has no solutions, then NULL is returned,
      otherwise a matrix X solving AX=B is returned.
  */
  MutableMatrix* rawLinAlgSolve(const MutableMatrix* A, 
                                const MutableMatrix* B,
                                int* success);

  /** Returns X s.t. AX = B.  Assumptions: 
      A is a square matrix, with the same number of rows as B.
      If a usage error occurs, NULL is returned and 'success' is set to 0.
      In all other cases, 'success' is set to 1.
      If A turns out to be not invertible, NULL is returned,
      otherwise the unique matrix X solving AX=B is returned.
  */
  MutableMatrix* rawLinAlgSolveInvertible(const MutableMatrix* A, 
                                          const MutableMatrix* B,
                                          int* success);

  /** A,B,C should be mutable matrices over the same ring, and a,b
     elements of this ring.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */ 

  /** set C += A*B.  If not implemented, or sizes/rings are not compatible
      then false is returned.  Otherwise true is returned.
  */
  M2_bool rawLinAlgAddMult(MutableMatrix* C,
                        const MutableMatrix* A,
                        const MutableMatrix* B);

  /** set C -= A*B.  If not implemented, or sizes/rings are not compatible
      then false is returned.  Otherwise true is returned.
  */
  M2_bool rawLinAlgSubMult(MutableMatrix* C,
                        const MutableMatrix* A,
                        const MutableMatrix* B);

  /* return A*B, where A,B are mutable matrices, over same ring, same density type.
   */
  MutableMatrix* /* or null */ rawLinAlgMult(const MutableMatrix* A,
                                             const MutableMatrix* B);


  engine_RawRingElementArrayOrNull rawLinAlgCharPoly(MutableMatrix* A);
  // returns an array whose coefficients give the characteristic polynomial of the square matrix A

  engine_RawRingElementArrayOrNull rawLinAlgMinPoly(MutableMatrix* A);
  // returns an array whose coefficients give the minimal polynomial of the square matrix A



#if 0
  RingElement *rawFFPackDeterminant(MutableMatrix *M);
  /* connected to rawFFPackDeterminant, MES */
  /* requires: M should be a square matrix over a prime finite field */

  size_t rawFFPackRank(MutableMatrix *M);
  /* connected to rawFFPackRank, MES */
  /* requires: M should be a matrix over a prime finite field */

  MutableMatrix /* Or Null */ * rawFFPackNullSpace(MutableMatrix *M, M2_bool right_side);
  /* connected to rawFFPackNullSpace, MES */
  /* requires: M should be a matrix over a prime finite field */
  /* computes either left or right nullspace */

  MutableMatrix /* or null */ * rawFFPackSolve(MutableMatrix *A, MutableMatrix *B, M2_bool right_side);
  /* connected to rawFFPackSolve, MES */
  /* requires: M should be a matrix over a prime finite field */
  /* returns solution of AX=B or XA=B, depending on right_side */

  MutableMatrix /* or null */ *rawFFPackInvert(MutableMatrix *M);
  /* connected to rawFFPackInvert, MES */
  /* requires: M should be a square matrix over a prime finite field */

  MutableMatrix /* or null */ *rawFFPackAddMultipleTo(MutableMatrix *C,
                                                      const MutableMatrix *A,
                                                      const MutableMatrix *B,
                                                      M2_bool transposeA,
                                                      M2_bool transposeB,
                                                      const RingElement *a,
                                                      const RingElement *b);
  /* A,B,C should be mutable matrices over a finite prime field, and a,b
     elements of this field.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
     connected to rawFFPackAddMultipleTo, MES
  */

  M2_arrayintOrNull rawFFPackRowRankProfile(MutableMatrix *A);
  /* connected, MES */

  M2_arrayintOrNull rawFFPackColumnRankProfile(MutableMatrix *A);
  /* connected, MES */
#endif

  engine_RawArrayIntPairOrNull rawLQUPFactorization(MutableMatrix *A);

  /***************************************************
   ***** Lapack routines for dense mutable matrices **
   ***************************************************/

  /* Each of the following routines accepts honest MutableMatrix arguments,
     and returns false if there is an error.  The return values are placed into
     some of the (already existing) parameters of the routine */

  M2_arrayintOrNull rawLU(const MutableMatrix *A,
                           MutableMatrix *L,
                           MutableMatrix *U); /* connected */
  /* Returns the permutation array: we need to be more precise which one.
     A encodes both the L and the U part, as in lapack.
   */

  /**
     Hi, this is rawLUincremental.

     Returns the permutation array: we need to be more precise which one.
     Given (first m columns of PLU = first m columns of A (not given), and
     given the (m+1)-st column of A, then returns a Q, modified from P,
     and changes LU (which encodes L and U)
     s.t.  (first (m+1) columns of QLU = first (m+1) columns of A (not given)
     Note: LU encodes L and U in the usual manner: lower triangular part is L,
     diagonal of L is all ones, and U is the upper triangular part.
  */
  M2_arrayintOrNull rawLUincremental(M2_arrayintOrNull P, /* constant */
                                     MutableMatrix *LU, /* modified in routine */
                                     const MutableMatrix *v, /* constant */
                                     int m);

  void rawTriangularSolve(MutableMatrix *Lv, /* modified in routine */
                          MutableMatrix *x, /* modified in routine */
                          int m,
                          int strategy);

  M2_bool rawEigenvalues(MutableMatrix *A,
                         MutableMatrix *eigenvalues,
                         M2_bool isHermitian); /* connected */
  /*
   */

  M2_bool rawEigenvectors(MutableMatrix *A,
                          MutableMatrix *eigenvalues,
                          MutableMatrix *eigenvectors,
                          M2_bool isHermitian); /* connected */
  /*
   */

  M2_bool rawSVD(MutableMatrix *A,
                 MutableMatrix *Sigma,
                 MutableMatrix *U,
                 MutableMatrix *VT,
                 M2_bool use_divide_and_conquer); /* connected */
  /*
   */

  M2_bool rawLeastSquares(MutableMatrix *A,
                          MutableMatrix *b,
                          MutableMatrix *x, /* return value: argument modified */
                          M2_bool assume_full_rank); /* connected */
  /* Case 1: A is a dense matrix over RR.  Then so are b,x.
     Case 2: A is a dense matrix over CC.  Then so are b,x. */

  M2_bool rawQR(const MutableMatrix* A, /* input m x n matrix */
                MutableMatrix* Q, /* output m x n orthonormal columns matrix */
                MutableMatrix* R, /* output R matrix: upper triangular, nonsingular if A has ker A = 0 */
                M2_bool return_QR); /* if false, the output is instead the lapack encoded Householder transformations */
  /* if return_QR is false, then Q will contain the encoded Householder reflections
     and the multipliers tau_i will appear in R.
     MES TODO: be more specific here, once we know the exact format!
  */

  /**************************************************/
  /**** Mutable Complex routines ********************/
  /**************************************************/

  M2_string rawMutableComplexToString(const MutableComplex *M);

  unsigned int  rawMutableComplexHash(const MutableComplex *M);

  MutableComplex* rawMutableComplex(const engine_RawMutableMatrixArray M);

  M2_arrayint rawPruneBetti(MutableComplex* C, int n, int f);

  MutableComplex* rawPruneComplex(MutableComplex* C, int n, int f);

  engine_RawMutableMatrixArray rawPruningMorphism(MutableComplex* C, int n, int f);

  /**************************************************/
  /**** Local Ring routines *************************/
  /**************************************************/

  Matrix * rawLiftLocalMatrix(const Ring * R, const Matrix *m);
  M2_bool  rawIsLocalUnit(const RingElement *f);

  /**************************************************/
  /**** Monomial ideal routines *********************/
  /**************************************************/

  /* A MonomialIdeal is an immutable object, having a base ring.
     The base ring should be a polynomial ring or quotient of one.
     In case a quotient is given, the monomial ideal is considered
     to be in the commutative quotient ring whose quotient elements
     are the lead terms of the quotient polynomials.
     Each monomial ideal is represented by its minimal generators only */

  engine_RawMonomialIdealOrNull IM2_MonomialIdeal_make(const Matrix *m, int n); /* drg: connected rawMonomialIdeal*/
  /* Given a matrix 'm' over an allowed base ring (as above), create the
     monomial ideal consisting of all of the lead monomials of the columns
     of 'm' which have their lead term in row 'n'.  If 'n' is out of range,
     or the ring is not allowed, NULL is returned. */

  const Matrix /* or null */ *IM2_MonomialIdeal_to_matrix(const MonomialIdeal *I); /* drg: connected rawMonomialIdealToMatrix */
  /* Return a one row matrix over the base ring of I consisting
     of the monomials in I */

  M2_string IM2_MonomialIdeal_to_string(const MonomialIdeal *I); /* TODO */

  unsigned int rawMonomialIdealHash(const MonomialIdeal *I); 
  /* connected to 'hash', sequential, as it is mutable */

  int IM2_MonomialIdeal_is_equal(const MonomialIdeal *I1,
                                     const MonomialIdeal *I2); /* drg: connected === */
        // 1 = true, 0 = false, -1 = error

  int IM2_MonomialIdeal_n_gens(const MonomialIdeal *I); /* drg: connected rawNumgens*/
  /* Returns the number of minimal generators of I */


  const MonomialIdeal /* or null */ *rawRadicalMonomialIdeal(const MonomialIdeal *I); /* drg: connected rawRadicalMonomialIdeal*/
  /* The radical of the monomial ideal, that is, the monomial ideal
     generated by the square-free parts of the each monomial of I. */

  const MonomialIdeal /* or null */ *IM2_MonomialIdeal_intersect(const MonomialIdeal *I,
                                                         const MonomialIdeal *J); /* drg: connected rawIntersect*/

  const MonomialIdeal /* or null */ *rawColonMonomialIdeal1(const MonomialIdeal *I,
                                                   const Monomial *a); /* drg: connected rawColon*/
  /* If I = (m1, ..., mr),
     Form the monomial ideal (I : a) = (m1:a, ..., mr:a) */

  const MonomialIdeal /* or null */ *rawColonMonomialIdeal2(const MonomialIdeal *I,
                                                      const MonomialIdeal *J); /* drg: connected rawColon*/
  /* Form the monomial ideal (I : J) = intersect(I:m1, ..., I:mr),
     where J = (m1,...,mr) */

  const MonomialIdeal /* or null */ *rawSaturateMonomialIdeal1(const MonomialIdeal *I,
                                              const Monomial *a); /* drg: connected rawSaturateMonomialIdeal*/
  /* Form I:a^\infty.  IE, set every variable which occurs in 'a' to '1' in
     every generator of I. */

  const MonomialIdeal /* or null */ *rawSaturateMonomialIdeal2(const MonomialIdeal *I,
                                                   const MonomialIdeal *J); /* drg: connected rawSaturateMonomialIdeal*/
  /* Form (I : J^\infty) = intersect(I:m1^\infty, ..., I:mr^\infty),
     where J = (m1,...,mr). */

  const MonomialIdeal /* or null */ *IM2_MonomialIdeal_borel(const MonomialIdeal *I); /* drg: connected rawStronglyStableClosure*/
  /* This should really be named: ..._strongly_stable.
     Form the smallest monomial ideal J containing I which is strongly stable,
     that is that:
     If m is in J, then p_ij(m) is in J,
     where p_ij(m) = x_j * (m/x_i), for j <= i, s.t. x_i | m. (Here the
     variables in the ring are x1, ..., xn */

  M2_bool IM2_MonomialIdeal_is_borel(const MonomialIdeal *I); /* drg: connected rawIsStronglyStable*/
  /* This should really be named: ..._is_strongly_stable.
     Determine if I is strongly stable (see IM2_MonomialIdeal_borel for the
     definition of strongly stable */

  int IM2_MonomialIdeal_codim(const MonomialIdeal *I); /* drg: connected rawCodimension*/
  /* Return the codimension of I IN THE AMBIENT POLYNOMIAL RING. */

  const MonomialIdeal /* or null */ *rawMonomialMinimalPrimes(const MonomialIdeal *I,
                                                int codim_limit,
                                                int count); /* drg: connected */
  /* RENAME THIS ROUTINE */
  /* Return a monomial ideal whose generators correspond to the
     minimal primes of I of codim <= codim_limit.  If a minimal prime
     of I has the form (x_i1, ..., x_ir), then the corresponding monomial
     is x_i1 ... x_ir, i.e. the support of
     the monomial generates the monomial minimal prime.
     If 'count' is positive, only collect this number.
  */

  const MonomialIdeal /* or null */ *rawMaximalIndependentSets(const MonomialIdeal *I,
                                                 int count);
  /* drg: connected rawMaximalIndependentSets */
  /* Returns a monomial ideal where each generator encodes a maximal independent set
     of variables of I.  If 'count' is positive, only collect this number.
     A maximal independent set is encoded as a squarefree monomial of the product
     of all of the independent variables in the set. */

  const RingElement /* or null */ * IM2_MonomialIdeal_Hilbert(const MonomialIdeal *I); /* connected to rawHilbert */
  /* This routine computes the numerator of the Hilbert series
     for coker I.  NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */

  M2_arrayint rawMonomialIdealLCM(const MonomialIdeal *I); /* connected, same name */

  const MonomialIdeal /* or null */ *rawAlexanderDual(const MonomialIdeal *I, const M2_arrayint top, int strategy); /* connected, same name */
  /* 0 is the default, 1 is an alternate strategy */

  /**************************************************/
  /**** Groebner basis and resolution routines ******/
  /**************************************************/

  enum ComputationStatusCode {
    /* Keep this enum in sync with RawStatusCodes in Macaulay2/m2/gb.m2 */
    COMP_NEED_RESIZE = 1,         /* need resize */
    COMP_ERROR = 2,               /* error */
    COMP_INTERRUPTED = 3,         /* interrupted */
    COMP_NOT_STARTED = 4,         /* not started */
    COMP_INITIAL_STOP = 5,        /* StopBeforeComputation */
    COMP_DONE = 6,                /* done */
    COMP_DONE_DEGREE_LIMIT = 7,   /* DegreeLimit */
    COMP_DONE_LENGTH_LIMIT = 8,   /* LengthLimit */
    COMP_DONE_SYZYGY_LIMIT = 9,   /* SyzygyLimit */
    COMP_DONE_PAIR_LIMIT = 10,    /* PairLimit */
    COMP_DONE_GB_LIMIT = 11,      /* BasisElementLimit */
    COMP_DONE_SYZ_LIMIT = 12,     /* SyzygyLimit */
    COMP_DONE_CODIM = 13,         /* CodimensionLimit */
    COMP_DONE_MIN_GENS = 14,      /* StopWithMinimalGenerators */
    COMP_DONE_STEPS = 15,         /* StepLimit */
    COMP_DONE_SUBRING_LIMIT = 16, /* SubringLimit */
    COMP_COMPUTING = 17,          /* computing */
    COMP_OVERFLOWED = 18,         /* overflowed */
  };

  enum StrategyValues {
    STRATEGY_LONGPOLYNOMIALS = 1,
    STRATEGY_SORT = 2,
    STRATEGY_USE_HILB = 4,
    STRATEGY_USE_SYZ = 8
  };

  enum Algorithms {
    GB_polyring_field = 1, /* The main GB algorithm to use */
    GB_polyring_field_homog = 2
  };

  enum gbTraceValues {
    /* The printlevel flags */
    PRINT_SPAIR_TRACKING = 1024
  };

  Computation /* or null */* IM2_Computation_set_stop(Computation *G,
                                     M2_bool always_stop,       /* 1 */
                                     M2_arrayint degree_limit,  /* 2*/
                                     int basis_element_limit,   /* 3 */
                                     int syzygy_limit,          /* 4 */
                                     int pair_limit,            /* 5 */
                                     int codim_limit,           /* 6 */
                                     int subring_limit,         /* 7 */
                                     M2_bool just_min_gens,     /* 8 */
                                     M2_arrayint length_limit   /* 9 */  /* not for GB */
                                     ); /* drg: connected rawGBSetStop */

  /* Each of these routines can return NULL, because of errors */

  Computation /* or null */ *rawStartComputation(Computation *G);
  /* start or continue the computation */

  enum ComputationStatusCode rawStatus1(Computation *C);

  int rawStatus2(Computation *C);
  /* The computation is complete up to and including this degree.
     The exact meaning of 'degree' is computation specific */

  M2_string IM2_GB_to_string(Computation *C); /* drg: connected, in actors4.d */

  unsigned int rawComputationHash(const Computation *C); /* drg: connected, in basic.d */

  void rawShowComputation(const Computation *C); /* Dan: connected to rawShowComputation */

  /*******************************************
   * Computation routines for Groebner bases *
   *******************************************/

  /* 
     routine to compute a Groebner basis of an ideal in a polynomial ring
     over a finite prime field.  Interfaces to mathicgb.
     reducer: 0 is ClassicReducer, 1 is MatrixReducer
   */
  const Matrix* /* or null */ rawMGB(const Matrix* input, 
                                     int reducer,
                                     int spairGroupSize,
                                     int nthreads,
                                     const M2_string logging
                                     ); /* connected: rawMGB */

  Computation /* or null */ *IM2_GB_make(const Matrix *m,
                                 M2_bool collect_syz,
                                 int n_rows_to_keep,
                                 M2_arrayint gb_weights,
                                 M2_bool use_max_degree,
                                 int max_degree,
                                 int algorithm,
                                 int strategy,
                                 int max_reduction_count); /* drg: connected rawGB */

  Computation /* or null */ *IM2_GB_force(const Matrix *m,
                                  const Matrix *gb,
                                  const Matrix *change,
                                  const Matrix *syz); /* drg: connected rawGBForce */

  Computation /* or null */ *rawMarkedGB(const Matrix *leadterms,
                                 const Matrix *m,
                                 const Matrix *gb,
                                 const Matrix *change,
                                 const Matrix *syz); /* mes: connected rawMarkedGB */

  Computation /* or null */ *rawGroebnerWalk(const Matrix *gb,
                                     const MonomialOrdering *order1);
  /* Create a GB algorithm which will compute using the generic Groebner walk algorithm
     Input: gb: a matrix which, under order1, would be a Groebner basis, except that
                'gb' is a matrix over a polynomial ring whose order is 'order2'.
            order1: a monomial ordering
     Output: a Groebner basis computation object which will compute a GB of gb wrt
            order2, using the Geneeric Groebner Walk algorithm of ...
     Assumptions: the base ring is a polynomial ring over a field, with NO quotient elements
  */

  Computation /* or null */ *IM2_GB_set_hilbert_function(Computation *G,
                                                 const RingElement *h); /* drg: connected rawGBSetHilbertFunction */


  const Matrix /* or null */ *rawGBGetMatrix(Computation *C);
  /* Get the minimal, auto-reduced GB of a GB computation.
     Each call to this may produce a different raw matrix */

  const Matrix /* or null */ *rawGBGetLeadTerms(Computation *G, int nparts);

  const Matrix /* or null */ *rawGBGetParallelLeadTerms(Computation *C, M2_arrayint w);

  const Matrix /* or null */ *rawGBMinimalGenerators(Computation *C);
  /* Yields a matrix whose columns form a minimal generating set
     for the ideal or submodule, as computed so far.  In the
     inhomogeneous case, this yields a generating set which is
     sometimes smaller than the entire Groebner basis. */

  const Matrix /* or null */ *rawGBChangeOfBasis(Computation *C);
  /* Yields the change of basis matrix from the Groebner basis to
     the original generators, at least if n_rows_to_keep was set
     when creating the GB computation.  This matrix, after the
     computation has run to completion, should satisfy:
     (original matrix) = (GB matrix) * (change of basis matrix). */

  const Matrix /* or null */ *rawGBSyzygies(Computation *C);
  /* Yields a matrix containing the syzygies computed so far
     via the GB computation C, assuming that 'collect_syz' was
     set when the computation was created.  If 'n_rows_to_keep' was
     set to a non-negative integer, then only that many rows of each
     syzygy are kept. */

  const Matrix /* or null */ *rawGBMatrixRemainder(Computation *G,
                                           const Matrix *m); /* drg: connected rawGBMatrixRemainder */

  M2_bool IM2_GB_matrix_lift(Computation *G,
                          const Matrix *m,
                          const Matrix /* or null */ **result_remainder,
                          const Matrix /* or null */ **result_quotient
                          ); /* drg: connected rawGBMatrixLift */
  /* false is returned if there is an error or if the remainder is NON-zero */

  int IM2_GB_contains(Computation *G,
                      const Matrix *m); /* drg: connected rawGBContains */


  /*******************************************
   * Computation routines for Resolutions ****
   *******************************************/

  /* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
  /* Res: SortStrategy, 0, 1, 2, 3 ?? */

  Computation /* or null */ *IM2_res_make(const Matrix *m,
                                  M2_bool resolve_cokernel,
                                  int max_level,
                                  M2_bool use_max_slanted_degree,
                                  int max_slanted_degree,
                                  int algorithm,
                                  int strategy /* drg: connected rawResolution */
                                  );

  const Matrix /* or null */ *rawResolutionGetMatrix(Computation *G,int level);
  /* rawResolutionGetMatrix */

  MutableMatrix /* or null */ *rawResolutionGetMatrix2(Computation *G,int level,int degree);
  /* rawResolutionGetMatrix2 */

  // This might be temporary!
  MutableMatrix /* or null */ *
  rawResolutionGetMutableMatrixB(Computation *C,
                                 const Ring* R, // A polynomial ring with coeffs = RR, or a finite field used in C, same monoid as C's ring.
                                 int level);

  // This might be temporary!
  MutableMatrix /* or null */ *
  rawResolutionGetMutableMatrix2B(Computation *C,
                           const Ring* KK, // should be RR, or a finite field used in C.
                           int level,
                           int degree);

  const FreeModule /* or null */ *rawResolutionGetFree(Computation *G, int level);
    /*drg: connected rawResolutionGetFree*/

  M2_arrayint rawResolutionBetti(Computation *G,
                                 int type); /* drg: connected rawGBBetti */
  /* type:
         0: minimal betti numbers, (for FastNonminimal=>true, the ACTUAL betti numbers)
         1: non-minimal betti numbers (skeleton size, or size of GB's).
           (for FastNonminimal=>true, same as "0" case)
         2: number of S-pairs remaining to consider
         3: number of monomials in polynomials at this slot
         4: for FastNonminimal=>true resolutions, the minimal betti numbers
            other cases, this is an error.
     Not all of these may be accessible with all algorithms.  If not available,
     A betti diagram with all -1's is displayed.
  */

  /* I don't know what this is supposed to do (mike) */
  int IM2_Resolution_status(Computation *G,
                    int * complete_up_through_this_degree,
                    int * complete_up_through_this_level); /* drg: TODO */
  /* -1: error condition, and the error message is set.
     0: not made, and in fact it won't ever be done...
     1: not started,
     2: started,
     3: stopped because of a stopping condition
     4: finished the computation completely
  */

  enum ComputationStatusCode IM2_Resolution_status_level(Computation *G,
                                                         int level,
                                                         M2_bool minimize,
                                                         int * complete_up_through_this_degree);
  /* WARNING: 'minimize' is completely ignored, and should be removed from the interface */
  /* drg: connected rawResolutionStatusLevel */

  M2_arrayint rawMinimalBetti(Computation *G,
                              M2_arrayint slanted_degree_limit,
                              M2_arrayint length_limit); /* connectd: rawMinimialBetti */
  
  /****************************************************/
  /**** Chinese remainder and rational reconstruction */
  /****************************************************/

  const RingElement * rawRingElementCRA(const RingElement *f,
                                        const RingElement *g,
                                        mpz_srcptr m,
                                        mpz_srcptr n);

  const Matrix * rawMatrixCRA(const Matrix *f,
                              const Matrix *g,
                              mpz_srcptr m,
                              mpz_srcptr n);

  const RingElement * rawRingElementRatConversion(const RingElement *f,
                                  mpz_srcptr m,
                                  const Ring *RQ);

  // f should be an element in the polynomial ring R (over ZZ).
  // RQ should be the same ring as R, but with rational coefficients

  const Matrix * rawMatrixRatConversion(const Matrix *f,
                                        mpz_srcptr m,
                                        const Ring *RQ);
  // f should be a matrix in the polynomial ring R (over ZZ).
  // RQ should be the same ring as R, but with rational coefficients



  /**************************************************/
  /**** Fraction free LU decomposition **************/
  /**************************************************/

  M2_arrayintOrNull IM2_FF_LU(MutableMatrix *M); /* connected to rawFFLU */
  /* Replace M by a column echelon form.  No fractions are generated, but the
     base ring should be a domain.
     If M has a column change of basis matrix attached, it will be modified accordingly.
  */

  /**************************************************/
  /**** LLL bases ***********************************/
  /**************************************************/

  M2_bool rawLLL(MutableMatrix *M,
                 MutableMatrix /* or null */ *U,
                 gmp_QQ threshold,
                 int strategy); /* DAN: connected to rawLLL */
  /* Given a mutable matrix M over ZZ, and a rational number threshold, 1/4 < threshold <= 1,
     modify M so that the columns form a Lenstra-Lenstra-Lovasz
     basis of the image of (the original) M.  ASSUMPTION: (strategy=0 case)
     the columns of M are already a a basis for the
     lattice.  The algorithm used is that in Cohen's book on computational algebraic number
     theory, BUT: beware of the typos in the algorithm!
     If there is any error (interupted, M or threshold not the correct kind), then false
     is returned, and LLL is set to 0.
     If M has a column change of basis matrix attached, it will be modified accordingly.

     strategy: 0 means use original Macaulay2 engine routine.
               2 means use NTL LLL
               (strategy%3) == 3 means use one of the real number variants:
               GramSchmidt or Givens: 0 or 4 (respectively)
               LLL or BKZ: 0 or 8 (respectively)
               FP, QP1, QP, XD, RR (respectively: 0, 16, 2*16, 3*16, 4*16
               Thus: strategy 3+4+8+16 means use NTL's GBKZ_QP1.

     For the RR variants, the suggested value of the threshold is 99/100.

     If U is not NULL, then it should be an m by m matrix over ZZ, where m is the number
     of columns of the matrix M.  In this case, it is set to be the invertible transform
     matrix such that Mold * U = Mnew.
  */

  M2_bool IM2_SmithNormalForm(MutableMatrix *M); /* connected rawSmithNormalForm */
  /* Given a mutable matrix over ZZ, compute the Smith normal form for M. (replaces
     M with this normal form.
     Currently the algorithm used makes computing the change of basis matrices
     difficult (we use mod D arithmetic, where D is the determinant).
     If there is an error, then an error is flagged and false is returned.
  */

  M2_bool IM2_HermiteNormalForm(MutableMatrix *M); /* connect rawHermiteNormalForm */
  /* Given a mutable matrix over ZZ, compute the Hermite normal form for M. (replaces
     M with this normal form.
     Currently the algorithm used makes computing the change of basis matrices
     difficult (we use mod D arithmetic, where D is the determinant).
     If there is an error, then an error is flagged and false is returned.
  */

  /**************************************************/
  /**** Specialized operations **********************/
  /**************************************************/

  Matrix /* or null */ * rawSubduction(int numparts,
                                       const Matrix *M,
                               const RingMap *F,
                               Computation *C);
  /*
    Perform a subalgebra reduction of the entries of the one row matrix M.
    C should be a GB computed in high enough degree to handle the elements of M,
      of an ideal of the form y_i - m_i (m_i is the lead monomial of f_i).
    F should be a ring map R --> R, sending y_i to f_i.
    M should be a matrix over the ring R, usually only involving the variables in the f_i.
    R should be a ring containing the variables of the f_i, and the variables y_i,
      with a monomial order eliminating the first set of variables (which is 'numparts' parts of the
      monomial ordering).
    numparts: number of parts in the monomial order of the original ring.
   The resulting matrix will have no monomials which are in the subalgebra
   generated by the monomials m_i, and each entry of M and the corresponding entry of the
   result differ by an element of the subalgebra generated by the f_i.
   */

  M2_bool rawIdealOfPoints(const Ring *R,
                      const MutableMatrix *Pts,
                      Matrix /* or null */ ** result_GB,
                      Matrix /* or null */ ** result_std_monoms);
  /* Returns false if an error occured.
     Input: R: a polynomial ring of the form K[x1,...,xn]
            Pts: an n by d matrix over K.
     Action: Compute the ideal of the points in n-space
             given by the columns of 'Pts'
     Output: result_GB: the GB of this ideal
             result_std_monoms: the standard monomials (1 by d matrix)
     Question: should this return the separators as well?
  */

  /**************************************************/
  /**** Factory and libfac routines *****************/
  /**************************************************/

  const RingElement /* or null */ *rawGCDRingElement(
                                             const RingElement *f, const RingElement *g,
                                             const RingElement *mipo, M2_bool inExtension
                                             ); /* connect to rawGCD */
  const RingElement /* or null */ *rawExtendedGCDRingElement(
                                                     const RingElement *f, const RingElement *g,
                                                     const RingElement **A, const RingElement **B
                                                     ); /* connected to rawExtendedGCD */
  const RingElement /* or null */ *rawPseudoRemainder(const RingElement *f, const RingElement *g); /* connected to rawPseudoRemainder */
  void rawFactor(const RingElement *f,
                 engine_RawRingElementArrayOrNull *result_factors,
                 M2_arrayintOrNull *result_powers); /* connected to rawFactor  */
  void rawFactor2(const RingElement *f, const RingElement *minpoly,
                 engine_RawRingElementArrayOrNull *result_factors,
                 M2_arrayintOrNull *result_powers); /* connected to rawFactor  */
  M2_arrayintOrNull rawIdealReorder(const Matrix *M);/* connected to rawIdealReorder */
  engine_RawMatrixArrayOrNull rawCharSeries(const Matrix *M);/* connected to rawCharSeries */
  engine_RawRingElementArrayOrNull rawRoots(const RingElement *g, long prec, int unique); /* connected to rawRoots */

  void rawDummy(void);          /* connected to rawDummy */

  /**************************************************/
  /**** Special routines for objects over RRR,CCC ***/
  /**************************************************/

  /* The code for these is in x-mutablemat.cpp */

  /* These routines set any real or complex numbers whose absolute value is less than
     epsilon.  If the ring is not over RRR or CCC, then an error message is given, and NULL
     is returned. */

  const Matrix /* or null */ *rawMatrixClean(gmp_RR epsilon, const Matrix *M);
  const RingElement /* or null */ *rawRingElementClean(gmp_RR epsilon, const RingElement *f);
  MutableMatrix /* or null */ *rawMutableMatrixClean(gmp_RR epsilon, MutableMatrix *M); /* modifies M in place */

  /* p is currently limited to infinity (with a given precision), and this routine
     returns the maximum norm of any RRR or CCC coefficient in the object.
     If the ring is not over RRR or CCC, then an error message is given, and NULL
     is returned */

  gmp_RRorNull rawMatrixNorm(gmp_RR p, const Matrix *M);
  gmp_RRorNull rawRingElementNorm(gmp_RR p, const RingElement *f);
  gmp_RRorNull rawMutableMatrixNorm(gmp_RR p, const MutableMatrix *M);

  // NAG begin
  M2Homotopy /* or null */ *rawHomotopy(M2SLEvaluator *Hx, M2SLEvaluator *Hxt, M2SLEvaluator *HxH);
  M2SLEvaluator /* or null */ *rawSLEvaluator(M2SLProgram *SLP, M2_arrayint constsPos, M2_arrayint varsPos, const MutableMatrix *consts);
  M2SLEvaluator /* or null */ *rawSLEvaluatorSpecialize(M2SLEvaluator* H, const MutableMatrix *parameters);
  M2SLProgram /* or null */ *rawSLProgram(unsigned long nConstantsAndInputs);
  M2_string rawSLEvaluatorToString(M2SLEvaluator *); /* connected */
  M2_bool rawSLEvaluatorEvaluate(M2SLEvaluator *sle, const MutableMatrix *inputs, MutableMatrix *outputs);
  M2_string rawHomotopyToString(M2Homotopy *); /* connected */
  M2_string rawSLProgramToString(M2SLProgram *); /* connected */
  unsigned int rawSLEvaluatorHash(M2SLEvaluator *); /* connected */
  unsigned int rawHomotopyHash(M2Homotopy *); /* connected */
  unsigned int rawSLProgramHash(M2SLProgram *); /* connected */

  M2_bool rawHomotopyTrack(M2Homotopy *H, const MutableMatrix *inputs, MutableMatrix *outputs,
                           MutableMatrix* output_extras,  
                           gmp_RR init_dt, gmp_RR min_dt,
                           gmp_RR epsilon, // o.CorrectorTolerance,
                           int max_corr_steps, 
                           gmp_RR infinity_threshold,
                           M2_bool checkPrecision);

  gmp_ZZ rawSLPInputGate(M2SLProgram *S);
  gmp_ZZ rawSLPSumGate(M2SLProgram *S, M2_arrayint a);
  gmp_ZZ rawSLPProductGate(M2SLProgram *S, M2_arrayint a);
  gmp_ZZ rawSLPDetGate(M2SLProgram *S, M2_arrayint a);
  gmp_ZZ rawSLPsetOutputPositions(M2SLProgram *S, M2_arrayint a);
  gmp_ZZ rawSLPDivideGate(M2SLProgram *S, M2_arrayint a);

  StraightLineProgram /* or null */ *rawSLP(const Matrix *consts, M2_arrayint program);
  const Matrix /* or null */ *rawEvaluateSLP(StraightLineProgram *SLP, const Matrix *vals);
  M2_string rawStraightLineProgramToString(StraightLineProgram *); /* connected */
  unsigned int rawStraightLineProgramHash(StraightLineProgram *); /* connected */
  const Matrix /* or null */ *rawTrackPaths(StraightLineProgram* slp_pred, StraightLineProgram* slp_corr, const Matrix* start_sols ,
                                    M2_bool is_projective,
                                    gmp_RR init_dt, gmp_RR min_dt, gmp_RR max_dt,
                                    gmp_RR dt_increase_factor, gmp_RR dt_decrease_factor, int num_successes_before_increase,
                                    gmp_RR epsilon, int max_corr_steps,
                                    int pred_type);

  PathTracker /* or null */ *rawPathTrackerPrecookedSLPs(StraightLineProgram* slp_pred, StraightLineProgram* slp_corr);
  PathTracker /* or null */ *rawPathTracker(const Matrix *);
  PathTracker /* or null */ *rawPathTrackerProjective(const Matrix *, const Matrix *, gmp_RR);
  M2_string rawPathTrackerToString(PathTracker *); /* connected */
  unsigned int rawPathTrackerHash(PathTracker *); /* connected */
  void rawSetParametersPT(PathTracker* PT, M2_bool is_projective,
                          gmp_RR init_dt, gmp_RR min_dt,
                          gmp_RR dt_increase_factor, gmp_RR dt_decrease_factor, int num_successes_before_increase,
                          gmp_RR epsilon, int max_corr_steps, gmp_RR end_zone_factor, gmp_RR infinity_threshold,
                          int pred_type);
  void rawLaunchPT(PathTracker* PT, const Matrix* start_sols);
  const Matrix /* or null */ *rawGetSolutionPT(PathTracker* PT, int solN);
  const Matrix /* or null */ *rawGetAllSolutionsPT(PathTracker* PT);
  int rawGetSolutionStatusPT(PathTracker* PT, int solN);
  int rawGetSolutionStepsPT(PathTracker* PT, int solN);
  gmp_RRorNull rawGetSolutionLastTvaluePT(PathTracker* PT, int solN);
  gmp_RRorNull rawGetSolutionRcondPT(PathTracker* PT, int solN);
  const Matrix /* or null */ *rawRefinePT(PathTracker* PT, const Matrix* sols, gmp_RR tolerance, int max_corr_steps_refine);
  // M2PointArray
  unsigned int rawPointArrayHash(M2PointArray *); 
  M2_string rawPointArrayToString(M2PointArray *);
  M2PointArray /* or null */ *rawPointArray(double epsilon, int n);
  int rawPointArrayLookup(M2PointArray *pa, const MutableMatrix *M, int col);
  int rawPointArrayLookupOrAppend(M2PointArray *pa, const MutableMatrix *M, int col);
  // NAG end  
  const Matrix /* or null */ *rawGbBoolean(const Matrix *m);
  const Matrix /* or null */ *rawBIBasis(const Matrix* m, int toGroebner);

#if defined(__cplusplus)
}
#endif

#endif

#if defined(NO_CONST)
#undef const
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
