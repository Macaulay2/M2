#ifndef _monomial_ordering_h_
#  define _monomial_ordering_h_

/**
 * @file interface/monomial-ordering.h
 * @brief Engine-boundary C API for assembling block-level `MonomialOrdering`s from declarative pieces.
 *
 * Declares the `MonomialOrdering_type` enum --- `MO_LEX`,
 * `MO_GREVLEX`, packed `MO_LEX2` / `MO_LEX4` /
 * `MO_GREVLEX2` / `MO_GREVLEX4`, weighted GRevLex variants
 * (`MO_GREVLEX_WTS` / `_GREVLEX2_WTS` / `_GREVLEX4_WTS`),
 * `MO_REVLEX`, `MO_WEIGHTS`, the Laurent variants
 * (`MO_LAURENT` and `MO_LAURENT_REVLEX`), `MO_NC_LEX`, and
 * `MO_POSITION_UP` / `_DOWN` --- together with the per-block
 * `mon_part` record (carrying type, `nvars`, and an optional
 * `*wts` weight array) and the variable-length
 * `MonomialOrdering` struct that holds the assembled sequence
 * of parts (`mon_part array[1]` flexible-style) plus a cached
 * `_hash`. Each `raw*MonomialOrdering` factory builds one block
 * (lex, GRevLex-with-packing, revlex, weights,
 * group-lex / group-revlex, NC-lex, position-up/down);
 * composition is via `rawProductMonomialOrdering` (for tensor
 * products) and `rawJoinMonomialOrdering` (the default block-
 * joining used when making monoids and polynomial rings),
 * both over an `engine_RawMonomialOrderingArray`. Accessors
 * include `rawNumberOfVariables`,
 * `rawNumberOfInvertibleVariables`,
 * `rawNonTermOrderVariables` (which detect local / tangent-cone
 * orders where GB algorithms must switch behaviour),
 * `IM2_MonomialOrdering_to_string`,
 * `rawMonomialOrderingHash`, and the predicate helpers
 * `moIsGRevLex` / `moIsLex` / `moGetWeightValues`.
 *
 * The output of these constructors flows into
 * `rawMonoid(mo, ...)`, where `imonorder.cpp` compiles the
 * declarative `MonomialOrdering` into an executable
 * `MonomialOrder` layout. The packed `MO_*2` / `MO_*4`
 * variants squeeze two or four exponents per `int` for small-
 * exponent rings, halving or quartering monomial-comparison
 * memory footprint --- they appear as `Lex`/`LexSmall`/`LexTiny`
 * and `GRevLex`/`GRevLexSmall`/`GRevLexTiny` on the M2 side.
 *
 * @see monomial-ordering.cpp
 * @see monoid.h
 * @see imonorder.hpp
 * @see engine-includes.hpp
 */

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

/**
 * @brief Front-end-side description of a monomial ordering as a list of
 * `mon_part` blocks.
 *
 * @details `_hash` caches the front-end hash so order objects can be
 * compared and deduplicated cheaply; `len` is the number of
 * blocks; `array[1]` is a C99 flexible-array tail holding the
 * `len` ordered blocks (`Lex`, `GRevLex`, `Weights`, `Position`,
 * ...). Built by the `MonomialOrderings::*` factories
 * (`monordering.hpp`) and compiled into the engine-side
 * `MonomialOrder_rec` (`imonorder.hpp`) via `monomialOrderMake`.
 */
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
