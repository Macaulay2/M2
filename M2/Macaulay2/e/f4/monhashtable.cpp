// Copyright 2005  Michael E. Stillman

#include "../newdelete.hpp"
#include "monhashtable.hpp"

#define HASHVALUE(m) (M->hash_value(m))
#define MONOMIAL_EQUAL(m,n) (M->is_equal(m,n))

template <typename ValueType>
void MonomialHashTable<ValueType>::reset()
{
  count = 0;
  nclashes = 0;
  max_run_length = 0;
  monequal_count = 0;
  monequal_fails = 0;
  //  bzero(hashtab, sizeof(value) * size);
  for (unsigned long i=0; i<size; i++)
    hashtab[i] = 0;
}

template <typename ValueType>
void MonomialHashTable<ValueType>::initialize(int logsize0)
{
  logsize = logsize0;
  size = (1<<logsize);
  threshold = 2*size/3;
  hashtab = newarray(value, size);
  hashmask = size-1;
  reset();
}

template <typename ValueType>
void MonomialHashTable<ValueType>::insert(value m)
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

template <typename ValueType>
void MonomialHashTable<ValueType>::grow()
{
  // Increase logsize, reset fields, and repopulate new hash table.
  dump();
  value *oldtab = hashtab;
  long oldsize = size;
  initialize(logsize+1);
  for (long i=0; i<oldsize; i++)
    if (oldtab[i])
      insert(oldtab[i]);
}

template <typename ValueType>
MonomialHashTable<ValueType>::MonomialHashTable(const ValueType *M0, int logsize0)
{
  M = M0;
  initialize(logsize0);
}

template <typename ValueType>
MonomialHashTable<ValueType>::~MonomialHashTable()
{
  deletearray(hashtab);
}

template <typename ValueType>
bool MonomialHashTable<ValueType>::find_or_insert(value m, value &result)
  // return true if the value already exists in the table.
  // otherwise, result is set to the new value.
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
      // Something is there, so we need to find either this value,
      // or a free spot, whichever comes first.
      long mhash = HASHVALUE(m);
      value *hashtop = hashtab + size;
      long run_len = 1;
      for (value *i = hashtab + hashval; ; i++, run_len++)
	{
	  if (run_len > max_run_length)
	    max_run_length = run_len;
	  if (i == hashtop) i = hashtab;
	  if (!(*i)) 
	    {
	      // Spot is empty, so m is a new value
	      *i = m;
	      result = m;
	      count++;
	      if (count > threshold) grow();
	      return false;
	    }
	  if (HASHVALUE(*i) == mhash)
	    {
	      monequal_count++;
	      if (MONOMIAL_EQUAL(m, *i))
		{
		  monequal_count++;
		  result = *i;
		  return true;
		}
	      monequal_fails++;
	    }
	  nclashes++;
	}
    }
}

template <typename ValueType>
void MonomialHashTable<ValueType>::dump() const
{
  fprintf(stderr, "size of hashtable   = %ld\n",size);
  fprintf(stderr, "  number of monoms  = %ld\n",count);
  fprintf(stderr, "  number of clashes = %ld\n",nclashes);
  fprintf(stderr, "  max run length    = %ld\n",max_run_length);
  fprintf(stderr, "  monequal calls    = %ld\n",monequal_count);
  fprintf(stderr, "  monequal false    = %ld\n",monequal_fails);
}

template <typename ValueType>
void MonomialHashTable<ValueType>::show() const
{
  long nzeros = 0;
  for (long i=0; i<size; i++)
    {
      if (hashtab[i] == 0)
	nzeros++;
      else 
	{
	  value m = hashtab[i];
	  if (nzeros > 0)
	    {
	      fprintf(stderr, "-- %ld zero elements --\n", nzeros);
	      nzeros = 0;
	    }
	  fprintf(stderr, "hash %ld monomial ", M->hash_value(m));
	  M->show(m);
	  fprintf(stderr, "\n");
	}
    }
  if (nzeros > 0)
    fprintf(stderr, "-- %ld zero elements --\n", nzeros);
}

template class MonomialHashTable<MonomialInfo>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
