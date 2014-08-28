#ifndef __monordering_h_
#define __monordering_h_
enum MonomialOrdering_type
{
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
  MO_LAURENT = 12, /* Lex order here */
  MO_LAURENT_REVLEX = 13, /* Rev lex order here */
  MO_NC_LEX = 14, /* Lex order, non-commutative */
  MO_POSITION_UP = 15,
  MO_POSITION_DOWN = 16
};

typedef struct mon_part_rec_
{
  enum MonomialOrdering_type type;
  int nvars;
  int *wts;
} *mon_part;

struct MonomialOrdering {
  unsigned int _hash;
  unsigned int len;
  mon_part array[1];
};

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
