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
  MO_NC_LEX = 13, /* Lex order, non-commutative */
  MO_COMPONENT = 14
};

typedef struct mon_part_rec_
{
  enum MonomialOrdering_type type;
  int nvars;
  int *wts;
} *mon_part;

struct MonomialOrdering {
  unsigned long _hash;
  unsigned int len;
  mon_part array[1];
};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
