#include "exptable.h"
#include "table.h"
/* Implementation of a hashtable [exponent vectors, of fixed length] --> unsigned long int. */
/* The implementation uses table.{h,c}, which was written by David R. Hanson */

struct exponent_table
{
  int nvars;
  Table_T * table;
};

static int table_nvars; /* Set this before calling 'hash' */

unsigned int exp_hash(const void * x)
     /* exp has type 'exponent' */
{
  int i;
  const int *xx = x;
  unsigned int result = 0;
  for (i=0; i<table_nvars; i++)
    result += (xx[i] << 3);
  return result;
}

int exp_cmp(const void * x, const void * y)
     /* x, y are both of type 'exponent' */
{
  int i;
  const int * xx = x;
  const int * yy = y;
  for (i=0; i<table_nvars; i++)
    {
      int cmp = xx[i] - yy[i];
      if (cmp != 0) return cmp;
    }
  return 0;
}

exponent_table *exponent_table_new(int hint,
                                   int nvars)
{
  exponent_table *result;
  NEW(result);
  result->nvars = nvars;
  result->table = Table_new(hint, exp_cmp, exp_hash);
  return result;
}

int exponent_table_length(exponent_table *E)
{
  return Table_length(E->table);
}

void exponent_table_free(exponent_table **E)
{
  Table_free(&((*E)->table));
  FREE(*E);
  *E = 0;
}

long exponent_table_put(exponent_table *E, const exponent expon, long value)
     /* Puts the element 'expon => value' into the table, and if an element is
        already there, its value is returned, otherwise 0 is returned.
        Thus, it is a good idea to not insert zero values into the table */
{
  table_nvars = E->nvars;
  return (long) Table_put(E->table, expon, (void *) value);
}

long exponent_table_get(exponent_table *E, const exponent expon)
     /* Returns the value associated to 'expon', returning zero, if 'expon'
        is not in the table */
{
  table_nvars = E->nvars;
  return (long) Table_get(E->table,expon);
}

const void ** exponent_table_to_array(exponent_table *E)
{
  return Table_toArray(E->table, NULL);
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
