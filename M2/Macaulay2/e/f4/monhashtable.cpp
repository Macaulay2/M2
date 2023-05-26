// Copyright 2005-2021  Michael E. Stillman

#include "f4/monhashtable.hpp"
#include <string.h>  // for memset
#include <iostream>  // for operator<<, endl, ostream, cout, basic_ostream

#define HASHVALUE(m) (M->hash_value(m))
#define MONOMIAL_EQUAL(m, n) (M->is_equal(m, n))

template <typename ValueType>
void MonomialHashTable<ValueType>::reset()
{
  memset(hashtab.get(), 0, sizeof(value) * size);

  count = 0;
  nfind_or_insert = 0;
  nclashes = 0;
  max_run_length = 0;
  monequal_count = 0;
  monequal_fails = 0;
}

template <typename ValueType>
void MonomialHashTable<ValueType>::initialize(int logsize0)
{
  logsize = logsize0;
  size = (1 << logsize);
  // threshold = size/3; // was 2*size/3
  // threshold = 2*size/3; // was 2*size/3
  threshold = size / 16;  // was 2*size/3
  hashtab = std::unique_ptr<value[]>(new value[size]);
  hashmask = size - 1;
  reset();
}

template <typename ValueType>
void MonomialHashTable<ValueType>::insert(value m)
{
  long hashval = HASHVALUE(m) & hashmask;
  while (hashtab[hashval])
    {
      hashval++;
      nclashes++;
      if (hashval == size) hashval = 0;
    }
  hashtab[hashval] = m;
  count++;
  if (count > threshold) grow();
}

template <typename ValueType>
void MonomialHashTable<ValueType>::grow()
{
  // Increase logsize, reset fields, and repopulate new hash table.
  //  if (M2_gbTrace >= 2) dump();
  std::unique_ptr<value[]> oldtab = std::move(hashtab);
  long oldsize = size;
  initialize(logsize + 1);
  for (long i = 0; i < oldsize; i++)
    if (oldtab[i]) insert(oldtab[i]);
}

template <typename ValueType>
MonomialHashTable<ValueType>::MonomialHashTable(const ValueType *M0,
                                                int logsize0)
  : M(M0),
    hashtab(nullptr), // set in body
    size(0),
    logsize(logsize0),
    hashmask(0),
    threshold(0),
    count(0),
    nfind_or_insert(0),
    nclashes(0),
    max_run_length(0),
    monequal_count(0),
    monequal_fails(0)
{
  initialize(logsize0);
}

template <typename ValueType>
bool MonomialHashTable<ValueType>::find_or_insert(value m, value &result)
{
  auto mhash = HASHVALUE(m);
  auto hashval = mhash & hashmask;
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
      value *hashtop = hashtab.get() + size;
      for (value *i = hashtab.get() + hashval;; i++)
        {
          if (i == hashtop) i = hashtab.get();
          if (!(*i))
            {
              // Spot is empty, so m is a new value
              *i = m;
              result = m;
              count++;
              if (count > threshold) grow();
              return false;
            }
          //          if (HASHVALUE(*i) == mhash)
          //            {
          if (MONOMIAL_EQUAL(m, *i))
            {
              result = *i;
              return true;
            }
          //            }
        }
    }
}

template <typename ValueType>
void MonomialHashTable<ValueType>::dump() const
{
  std::cout << "--hash table info--" << std::endl;
  std::cout << "  size of hashtable = " << size << std::endl;
  std::cout << "  number of monoms  = " << count << std::endl;
  std::cout << "  number of calls   = " << nfind_or_insert << std::endl;
  std::cout << "  number of clashes = " << nclashes << std::endl;
  std::cout << "  max run length    = " << max_run_length << std::endl;
  std::cout << "  monequal calls    = " << monequal_count << std::endl;
  std::cout << "  monequal false    = " << monequal_fails << std::endl;
}

template <typename ValueType>
void MonomialHashTable<ValueType>::show() const
{
  long nzeros = 0;
  for (long i = 0; i < size; i++)
    {
      if (hashtab[i] == 0)
        nzeros++;
      else
        {
          value m = hashtab[i];
          if (nzeros > 0)
            {
              std::cerr << "-- " << nzeros << " zero elements --" << std::endl;
              nzeros = 0;
            }
          std::cerr << "hash " << M->hash_value(m) << " monomial ";
          M->show(m);
          std::cerr << std::endl;
        }
    }
  if (nzeros > 0) std::cerr << "-- " << nzeros << " zero elements --" << std::endl;
}

template class MonomialHashTable<MonomialInfo>;
template class MonomialHashTable<MonomialsWithComponent>;
template class MonomialHashTable<MonomialsIgnoringComponent>;
template class MonomialHashTable<ResMonomialsWithComponent>;
template class MonomialHashTable<ResMonomialsIgnoringComponent>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
