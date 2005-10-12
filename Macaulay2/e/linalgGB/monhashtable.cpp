// Copyright 2005  Michael E. Stillman

#include "monhashtable.hpp"

#define HASHVALUE(m) (M->hash_value(m))
#define MONOMIAL_EQUAL(m,n) (M->is_equal(m,n))

void MonomialHashTable::initialize(int logsize0)
{
  count = 0;
  nclashes = 0;
  max_run_length = 0;
  logsize = logsize0;
  size = (1<<logsize);
  threshold = 2*size/3;
  hashtab = newarray(monomial, size);
  for (unsigned long i=0; i<size; i++)
    hashtab[i] = 0;
  hashmask = size-1;
}

void MonomialHashTable::insert(monomial m)
{
  long hashval = HASHVALUE(m) & hashmask;
  while (hashtab[hashval]) {
    hashval++;
    nclashes++;
    if (hashval == size)
      hashval = 0;
  }
  hashtab[hashval] = m;
  count++;
  if (count > threshold) grow();
}

void MonomialHashTable::grow()
{
  // Increase logsize, reset fields, and repopulate new hash table.
  dump();
  monomial *oldtab = hashtab;
  long oldsize = size;
  initialize(logsize+1);
  for (long i=0; i<oldsize; i++)
    if (oldtab[i])
      insert(oldtab[i]);
}

MonomialHashTable::MonomialHashTable(MonomialInfo *M0, int logsize0)
{
  M = M0;
  initialize(logsize0);
}

MonomialHashTable::~MonomialHashTable()
{
  deletearray(hashtab);
}

bool MonomialHashTable::find_or_insert(monomial m, monomial &result)
  // return true if the monomial already exists in the table.
  // otherwise, result is set to the new monomial.
{
  long hashval = HASHVALUE(m) & hashmask;
  if (!hashtab[hashval])
    {
      // No entry is there.  So, we insert it directly.
      hashtab[hashval] = m;
      result = m;
      count++;
      if (count > threshold) grow();
      return false;
    }
  else
    {
      // Something is there, so we need to find either this monomial,
      // or a free spot, whichever comes first.
      long mhash = HASHVALUE(m);
      monomial *hashtop = hashtab + size;
      for (monomial *i = hashtab + hashval; ; i++)
	{
	  if (i == hashtop) i = hashtab;
	  if (!(*i)) 
	    {
	      // Spot is empty, so m is a new monomial
	      *i = m;
	      result = m;
	      count++;
	      if (count > threshold) grow();
	      return false;
	    }
	  if (HASHVALUE(*i) == mhash && MONOMIAL_EQUAL(m, *i))
	    {
	      result = *i;
	      return true;
	    }
	  nclashes++;
	}
    }
}

void MonomialHashTable::dump() const
{
  fprintf(stderr, "size of hashtable   = %ld\n",size);
  fprintf(stderr, "  number of monoms  = %ld\n",count);
  fprintf(stderr, "  number of clashes = %ld\n",nclashes);
  fprintf(stderr, "  max run length    = %ld\n",max_run_length);
}

void MonomialHashTable::show() const
{
  long nzeros = 0;
  for (long i=0; i<size; i++)
    {
      if (hashtab[i] == 0)
	nzeros++;
      else 
	{
	  long *m = hashtab[i];
	  if (nzeros > 0)
	    {
	      fprintf(stderr, "-- %ld zero elements --\n", nzeros);
	      nzeros = 0;
	    }
	  fprintf(stderr, "[%ld", m[0]);
	  for (int v=1; v<M->monomial_size(m); v++)
	    fprintf(stderr, " %ld", m[v]);
	  fprintf(stderr, "]\n");
	}
    }
  if (nzeros > 0)
    fprintf(stderr, "-- %ld zero elements --\n", nzeros);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
