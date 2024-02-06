#ifndef _monomial_ordering_h_
#  define _monomial_ordering_h_

#  include "engine-includes.hpp"

typedef struct MonomialOrdering MonomialOrdering;

/**
   MonomialOrdering interface routines
 */

enum MonomialOrdering_type {
  MO_LEX = 1,
  MO_LEX2 = 2,
  MO_LEX4 = 3,
  MO_GREVLEX = 4,
  MO_GREVLEX2 = 5,
  MO_GREVLEX4 = 6,
  MO_GREVLEX_WTS = 7,
  MO_GREVLEX2_WTS = 8,
  MO_GREVLEX4_WTS = 9,
  MO_REVLEX = 10,
  MO_WEIGHTS = 11,
  MO_LAURENT = 12,        /* Lex order here */
  MO_LAURENT_REVLEX = 13, /* Rev lex order here */
  MO_NC_LEX = 14,         /* Lex order, non-commutative */
  MO_POSITION_UP = 15,
  MO_POSITION_DOWN = 16
};

typedef struct mon_part_rec_
{
  enum MonomialOrdering_type type;
  int nvars;
  int *wts;
} * mon_part;

struct MonomialOrdering
{
  unsigned int _hash;
  unsigned int len;
  mon_part array[1];
};

#  if defined(__cplusplus)
extern "C" {
#  endif

MonomialOrdering *rawLexMonomialOrdering(int nvars, int packing);
/* drg: connected rawMonomialOrdering*/
/* Lex, LexSmall, LexTiny */

MonomialOrdering /* or null */ *rawGRevLexMonomialOrdering(M2_arrayint degs,
                                                           int packing);
/* drg: connected rawMonomialOrdering*/
/* GRevLex, GrevLexSmall, GRevLexTiny */

MonomialOrdering *rawRevLexMonomialOrdering(int nvars);
/* drg: connected rawMonomialOrdering*/
/* RevLex => n */

MonomialOrdering *rawWeightsMonomialOrdering(M2_arrayint wts);
/* drg: connected rawMonomialOrdering*/
/* Weights => {...} */

MonomialOrdering *rawGroupLexMonomialOrdering(int nvars);
/* drg: connected rawMonomialOrdering*/
/* GroupLex => n */

MonomialOrdering *rawGroupRevLexMonomialOrdering(int nvars);
/* GroupRevLex => n */

MonomialOrdering *rawNClexMonomialOrdering(int nvars);
/* drg: connected rawMonomialOrdering*/
/* NCLex => n */

MonomialOrdering *rawPositionMonomialOrdering(M2_bool up_or_down);
/* drg: connected rawMonomialOrdering */
/* argument of true:  Position => Up, (should be the default)
 * argument of false: Position => Down
 */

MonomialOrdering *rawProductMonomialOrdering(
    engine_RawMonomialOrderingArray mo);
/* drg: connected rawMonomialOrdering*/
/* for tensor products */

MonomialOrdering *rawJoinMonomialOrdering(engine_RawMonomialOrderingArray mo);
/* drg: connected rawMonomialOrdering*/
/* default, when making monoids and polynomial rings */

int rawNumberOfVariables(const MonomialOrdering *mo);
/* drg: connected rawNumberOfVariables*/

int rawNumberOfInvertibleVariables(const MonomialOrdering *mo);
/* drg: connected rawNumberOfInvertibleVariables*/

M2_arrayint rawNonTermOrderVariables(const MonomialOrdering *mo);
/* Dan: PLEASE CONNECT */
/* Returns an array of the indices of those variables which are less than 1 in
   the monomial ordering.  If this number is > 0, then the monomial ordering is
   not a term order, and local (tangent cone) algorithms must be used for GB's
 */

M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering *mo);
/* drg: connected */

unsigned int rawMonomialOrderingHash(const MonomialOrdering *mo);
/* drg: connected hash */
/* Assigned sequentially */

int moIsGRevLex(const struct MonomialOrdering *mo);

int moIsLex(const struct MonomialOrdering *mo);

M2_arrayint moGetWeightValues(const struct MonomialOrdering *mo);

M2_arrayintOrNull rawMonomialOrderingToMatrix(
    const struct MonomialOrdering *mo);
/* return a (flattened) matrix corresponding to the monomial ordering 'mo'.
   Appended to this sequence of integers is 3 further numbers:
   (1) If the tie-breaker is revlex, one further value of "1" is added, else if
   it is lex, one further value of "0" is added. (2) If the module order is
   Position=>Up, then 0 else 1 in the next spot. (3) If the modules part of the
   order is considered right before the ith row of this matrix then "i" is in
   the next spot. (i=#rows, f the module component is considered last). The
   returned value represents a matrix with #vars columns, and #gradings weights,
   in row-major order (each row is contiguous in memory), plus the three extra
   entries. NULL is returned if 'mo' corresponds to a non-commutative monoid, or
   has "Inverses=>true" set.
*/

#  if defined(__cplusplus)
}
#  endif

#endif /* _monomial-ordering_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
