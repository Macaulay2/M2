// Copyright 1998 by Michael Stillman
#ifndef _Ehashtab_hpp_
#define _Ehashtab_hpp_

#include <stdio.h>
#include "Emonoid.hpp"
#include "text_io.hpp"

const int MONOMIAL_HASH_SIZE = 1024*2;
const int ALLOC_SIZE = 1024*10-4;

class MonomialAllocation
{
  struct SLAB {
    SLAB *next;
    int *first;
  };
  SLAB *oldies;
  int *first;			// Pointer to beginning of large array
  int *next;			// Return the next 
  int left;
  int nrequests;

  void make_new_slab(int size) {
    SLAB *t = new SLAB;
    t->next = oldies;
    t->first = first;
    oldies = t;
    if (size < ALLOC_SIZE)
      size = ALLOC_SIZE;
    first = new int[size];
    next = first;
    left = size;
  }
public:
  MonomialAllocation() : oldies(0), first(0), next(0), left(0), nrequests(0) {}
  ~MonomialAllocation() {
    // Delete all of the slabs
    while (oldies != 0) {
      SLAB *tmp = oldies;
      oldies = oldies->next;
      delete [] tmp->first;
      delete tmp;
    }
    delete [] first;
  }

  int * new_array(int size) {
    // a way of doing: a = new int[size], but without (too much) space overhead.
    if (left < size)
      make_new_slab(size);
    int *result = next;
    next += size;
    left -= size;
    nrequests++;
    return result;
  }

  void reset_top(int *m) {
    // WARNING!! A dangerous operation...
    next = m;
  }

  void stats() {
    int nslabs = 1;
    for (SLAB *p = oldies; p != 0; p=p->next)
      nslabs++;
    fprintf(stdout, "number of requests       = %d\n", nrequests);
    fprintf(stdout, "number of monomial slabs = %d\n", nslabs);
  }
};

class EMonomialTable
{
protected:
  int len;			// Length of bins array.
  monomial **bins;		// Hash table bin[0..len-1].
  MonomialAllocation MA;

public:
  EMonomialTable(int nbins) : len(nbins) {
    bins = new monomial * [len];
    for (int i=0; i<len; i++) bins[i] = 0;
  }
  ~EMonomialTable() {
    // Remove all of the bins: MES
  }
  void enlarge(int newsize) {
    // A somewhat time consuming process...: MES
  }
  void insert(monomial *elem) {
    // Assumption: 'elem' is not in the hash table,
    // and the hashval and other fields of the monomial are all set.
    int h = elem->hashval % len;
    elem->next = bins[h];
    bins[h] = elem;
  }

  int *allocate_tentative_monomial(int len)
    {
      return MA.new_array(len);
    }

  void give_back(int *val)
    {
      // WARNING: a dangerous unchecked operation
      MA.reset_top(val);
    }
  void showshape() {
    int maxpernode;
    int minpernode;
    int total = 0;
    int nzeros = 0;
    int i;
    int *nelems = new int[len];
    for (i=0; i<len; i++)
      {
	int n = 0;
	for (monomial *v = bins[i]; v!=0; v=v->next) n++;
	nelems[i] = n;
	total += n;
	if (n == 0) nzeros++;
      }
    maxpernode = nelems[0];
    minpernode = nelems[0];
    for (i=1; i<len; i++)
      {
	if (nelems[i] < minpernode)
	  minpernode = nelems[i];
	else if (nelems[i] > maxpernode)
	  maxpernode = nelems[i];
      }
    fprintf(stdout, "total #mons = %d\n", total);
    fprintf(stdout, "max per bin = %d\n", maxpernode);
    fprintf(stdout, "min per bin = %d\n", minpernode);
    fprintf(stdout, "# zero bins = %d\n", nzeros);
    buffer o;
    for (i=0; i<len; i++)
      {
        o << nelems[i] << " ";
	//fprintf(stderr, "%d ", nelems[i]);
	//if ((i % 50) == 0) fprintf(stderr, "\n");
	if ((i % 50) == 0) o << newline;
      }
    //fprintf(stderr, "\n");
    o << newline;
    emit(o.str());
  }
  void stats() {
    showshape();
    MA.stats();
  }
};

class ECommMonomialTable : public EMonomialTable
{
  int nvars;
  int nslots;
  const ECommMonoid *M;
  const EMonomialOrder *MO;

  bool is_equal_commutative(const int *a, const int *b) const {
    for (int i=0; i<nvars; i++)
      if (*a++ != *b++) return false;
    return true;
  }
  bool search_commutative(const int *exponents, uint32 hashval, monomial * & found) const {
    int h = hashval % len;
    for (monomial *v = bins[h]; v != 0; v=v->next) {
      if (v->hashval != hashval) continue;
      if (is_equal_commutative(v->monom, exponents)) {
	found = v;
	return true;
      }
    }
    return false;
  }
public:
  ECommMonomialTable(const ECommMonoid *M, int nbins) : EMonomialTable(nbins), M(M) {
    MO = M->getMonomialOrder();
    nvars = M->n_vars();
    nslots = M->n_slots();
  }
  ~ECommMonomialTable() {
  }
  monomial *lookup_and_insert_commutative(int *exponents) {
    // ONLY VALID IN COMMUTATIVE CASE...!!
    uint32 hashval = M->hash(exponents);
    monomial *result;
    if (!search_commutative(exponents,hashval,result)) {
      // Need to create a new monomial
      result = new monomial;
      result->next = 0;
      result->hashval = hashval;
      result->monom = exponents;
      MO->encode_commutative(exponents,exponents+nslots);
      insert(result);
    } else
      MA.reset_top(exponents);
    return result;
  }
};

class ENCMonomialTable : public EMonomialTable
{
  const ENCMonoid *M;

  bool is_equal_noncommutative(const int *a, const int *b) const {
    if (*a != *b) return false;
    int len = *a++; 
    b++;
    for (int i=1; i<len; i++)
      if (*a++ != *b++) return false;
    return true;
  }
  bool search_noncommutative(const int *encoded, uint32 hashval, monomial * & found) const {
    // arrlen = length of the 'encoded' array.
    int h = hashval % len;
    for (monomial *v = bins[h]; v != 0; v=v->next) {
      if (v->hashval != hashval) continue;
      if (is_equal_noncommutative(v->monom, encoded)) {
	found = v;
	return true;
      }
    }
    return false;
  }

public:
  ENCMonomialTable(ENCMonoid *M, int nbins) : EMonomialTable(nbins), M(M) {
  }
  ~ENCMonomialTable() {
  }
  const monomial *lookup_and_insert_noncommutative(int *encoded) {
    // This is the Non-commutative case!!
    uint32 hashval = M->hash(encoded);
    monomial *result;
    if (!search_noncommutative(encoded,hashval,result)) {
      // This monomial is to be interned
      result = new monomial;
      result->next = 0;
      result->hashval = hashval;
      result->monom = encoded;
      MA.reset_top(encoded + *encoded);
      insert(result);
    } else
      MA.reset_top(encoded);
    return result;
  }
};

#endif
