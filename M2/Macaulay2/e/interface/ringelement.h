#ifndef _ringelement_h_
#  define _ringelement_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Monomial;
class Ring;
class RingElement;
#  else
typedef struct Monomial Monomial;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   RingElement interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

unsigned int rawRingElementHash(const RingElement *a);

const Ring *IM2_RingElement_ring(const RingElement *a);

M2_string IM2_RingElement_to_string(const RingElement *f);

const RingElement *IM2_RingElement_from_Integer(const Ring *R, gmp_ZZ d);

const RingElement *IM2_RingElement_from_rational(const Ring *R, mpq_srcptr r);

const RingElement *IM2_RingElement_from_BigComplex(const Ring *R, gmp_CC z);

const RingElement *IM2_RingElement_from_BigReal(const Ring *R, gmp_RR z);

gmp_ZZorNull IM2_RingElement_to_Integer(const RingElement *a);
/* If the ring of a is ZZ, or ZZ/p, this returns the underlying representation.
   Otherwise, NULL is returned, and an error is given */

gmp_QQorNull IM2_RingElement_to_rational(const RingElement *a);

gmp_RRorNull IM2_RingElement_to_BigReal(const RingElement *a);

gmp_CCorNull IM2_RingElement_to_BigComplex(const RingElement *a);

long rawDiscreteLog(const RingElement *h);

const RingElement *rawMultiplicativeGenerator(const Ring *R);

const RingElement /* or null */ *IM2_RingElement_make_var(const Ring *R, int v);

M2_bool IM2_RingElement_is_zero(const RingElement *a);

M2_bool IM2_RingElement_is_equal(const RingElement *a, const RingElement *b);

engine_RawRingElementPair IM2_RingElement_divmod(const RingElement *a,
                                                 const RingElement *b);

int rawRingElementCompare(const RingElement *a, const RingElement *b);

const RingElement *IM2_RingElement_promote(const Ring *S, const RingElement *f);
/* drg: connected rawPromote*/

const RingElement /* or null */ *IM2_RingElement_lift(int *success_return,
                                                      const Ring *S,
                                                      const RingElement *f);
/* drg: connected rawLift*/
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

M2_bool IM2_RingElement_is_graded(const RingElement *a);

M2_arrayint IM2_RingElement_multidegree(const RingElement *a);

const RingElement * /* or null */ rawRingElementAntipode(const RingElement *f);

gmp_ZZpairOrNull rawWeightRange(M2_arrayint wts, const RingElement *a);
/* The first component of the degree is used, unless the degree monoid is
   trivial, in which case the degree of each variable is taken to be 1. Returns
   lo,hi degree.  If the ring is not a graded ring or a polynomial ring then
   (0,0) is returned.
*/

const RingElement /* or null */ *IM2_RingElement_homogenize_to_degree(
    const RingElement *a,
    int v,
    int deg,
    M2_arrayint wts);

const RingElement /* or null */ *
IM2_RingElement_homogenize(const RingElement *a, int v, M2_arrayint wts);

const RingElement /* or null */ *IM2_RingElement_term(const Ring *R,
                                                      const RingElement *a,
                                                      const Monomial *m);
/* R must be a polynomial ring, and 'a' an element of the
   coefficient ring of R.  Returns a*m, if this is a valid
   element of R.  Returns NULL if not (with an error message). */

const RingElement /* or null */ *IM2_RingElement_get_terms(
    int nvars, /* n variables in an outermost monoid */
    const RingElement *a,
    int lo,
    int hi);
/* Returns the sum of some monomials of 'a', starting at 'lo',
   going up to 'hi'.  If either of these are negative, they are indices
   from the back of the polynomial.
   'a' should be an element of a polynomial ring.
*/

const RingElement /* or null */ *IM2_RingElement_get_coeff(
    const Ring *coeffRing, /* ring of the result */
    const RingElement *a,
    const Monomial *m);
/* Return (as an element of the coefficient ring) the coeff
     of the monomial 'm'.
  */

const RingElement /* or null */ *IM2_RingElement_lead_coeff(
    const Ring *coeffRing, /* ring of the result */
    const RingElement *a);

const Monomial /* or null */ *IM2_RingElement_lead_monomial(
    int nvars, /* number of variables in an outermost monoid */
    const RingElement *a);

int IM2_RingElement_n_terms(
    int nvars, /* number of variables in an outermost monoid */
    const RingElement *a);

engine_RawArrayPairOrNull IM2_RingElement_list_form(
    const Ring *coeffRing, /* ring of the result coefficients */
    const RingElement *f);

engine_RawRingElementArray rawGetParts(const M2_arrayint wts,
                                       const RingElement *f);
/* Return an array of RingElement's, each having pure weight, and sorted by
   strictly increasing weight value.  The wt vector values must fit into
   a word length integer.  */

/* FIXME: where to get ring_elem?
void convolve(const PolyRing *R,
              const VECTOR(ring_elem) & input_relems,
              VECTOR(ring_elem) & output_relems,
              int convolve_type);
*/

engine_RawRingElementArrayOrNull rawConvolve(engine_RawRingElementArray H,
                                             int convolve_type);

const RingElement /* or null */ *rawGetPart(const M2_arrayint wts,
                                            const RingElement *f,
                                            M2_bool lobound_given,
                                            M2_bool hibound_given,
                                            long lobound,
                                            long hibound);
/* Return the sum of all of the terms t of f, which satisfy: lobound <= wt.t <=
   hibound,
   where, if lobound_given is false, then lobound is -infinity, and if
   hibound_given
   is false, then hibound is infinity. */

int IM2_RingElement_index_if_var(const RingElement *f);
/* if f is a variable of its ring, then the index of that variable is returned.
   If f isnot a variable, then -1 is returned. */

M2_arrayint rawRingElementIndices(const RingElement *f);
/* The list of indices of variables which occur in f is returned. */

const RingElement /* or null */ *rawAssociateDivisor(const RingElement *f);

const RingElement /* or null */ *rawRingElementContent(const RingElement *f);
// returns the content of f (as a matrix over the base coefficient ring)

const RingElement /* or null */ *rawRingElementRemoveContent(
    const RingElement *f);
// returns the polynomial which results after division by the content

const RingElement /* or null */ *rawRingElementSplitContent(
    const RingElement *f,
    const RingElement /* or null */ **result);
// returns the content of f (as a matrix over the base coefficient ring)
// result is set to the polynomial which results after division by the content

const RingElement /* or null */ *IM2_RingElement_numerator(
    const RingElement *a);

const RingElement /* or null */ *IM2_RingElement_denominator(
    const RingElement *a);

const RingElement /* or null */ *IM2_RingElement_fraction(const Ring *R,
                                                          const RingElement *a,
                                                          const RingElement *b);

gmp_ZZorNull rawSchurDimension(const RingElement *f);

const RingElement /* or null */ *rawSchurSnTensorMult(const RingElement *a,
                                                      const RingElement *b);
/* the tensor multiplication function in SchurSnRing */

const RingElement /* or null */ *rawSchurFromPartition(const Ring *R,
                                                       M2_arrayint part);
// R should be a SchurRing2
// part should be a partition: a weakly descending list of integers (for now,
// non-negative)
// if R has a limit on the size of partitions, then

int rawDegree(int v, const RingElement *f);
/* Returns -1 if 0 or not implemented for a given ring.  For now, valid only for
 * tower rings */

int rawExtensionDegree(int firstvar, const Ring *R1);
/* Currently only valid for tower rings.  Others return 0.  */

const RingElement /* or null */ *rawDiff(int v, const RingElement *f);

const RingElement /* or null */ *rawLowerP(const RingElement *f);

const RingElement /* or null */ *rawPowerMod(const RingElement *f,
                                             mpz_srcptr n,
                                             const RingElement *g);

#  if defined(__cplusplus)
}
#  endif

#endif /* _ringelement_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
