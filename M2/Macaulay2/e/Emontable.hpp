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

  void stats() {
    int nslabs = 1;
    for (SLAB *p = oldies; p != 0; p=p->next)
      nslabs++;
    fprintf(stderr, "number of requests       = %d\n", nrequests);
    fprintf(stderr, "number of monomial slabs = %d\n", nslabs);
  }
};

class EMonomialTable
{
  int len;			// Length of bins array.
  monomial **bins;		// Hash table bin[0..len-1].
  const EMonoid *M;
  const EMonomialOrder *MO;
  MonomialAllocation MA;

  bool word_is_equal(const int *a, const int *b, int len) const;
  bool exp_is_equal(const int *a, const int *b) const {
    int n = M->n_vars();
    for (int i=0; i<n; i++)
      if (*a++ != *b++) return false;
    return true;
  }
  bool encoded_is_equal(const int *a, const int *b, int len) const {
    for (int i=0; i<len; i++)
      if (*a++ != *b++) return false;
    return true;
  }

  bool search_exponents(const int *exponents, uint32 hashval, monomial * & found) const {
    int h = hashval % len;
    for (monomial *v = bins[h]; v != 0; v=v->next) {
      if (v->hashval != hashval) continue;
      if (exp_is_equal(v->exponents, exponents)) {
	found = v;
	return true;
      }
    }
    return false;
  }
  bool search_encoded(int arrlen,const int *encoded, uint32 hashval, monomial * & found) const {
    // arrlen = length of the 'encoded' array.
    int h = hashval % len;
    for (monomial *v = bins[h]; v != 0; v=v->next) {
      if (v->hashval != hashval) continue;
      if (encoded_is_equal(v->partial_sums, encoded, arrlen)) {
	found = v;
	return true;
      }
    }
    return false;
  }

public:
  EMonomialTable(EMonoid *M, int nbins) : len(nbins), M(M) {
    MO = M->getMonomialOrder();
    bins = new monomial * [len];
    for (int i=0; i<len; i++) bins[i] = 0;
  }
  ~EMonomialTable() {
  }
  void enlarge(int newsize) {
    // A somewhat time consuming process...
  }
  void insert(monomial *elem) {
    // Assumption: 'elem' is not in the hash table,
    // and the hashval and other fields of the monomial are all set.
    int h = elem->hashval % len;
    elem->next = bins[h];
    bins[h] = elem;
  }

  monomial *lookup_and_insert_commutative(const int *exponents) {
    // ONLY VALID IN COMMUTATIVE CASE...!!
    uint32 hashval = M->hash_exponents(exponents);
    monomial *result;
    if (!search_exponents(exponents,hashval,result)) {
      // Need to create a new monomial
      result = new monomial;
      result->next = 0;
      result->hashval = hashval;
      result->exponents = MA.new_array(M->n_vars());
      result->partial_sums = MA.new_array(M->n_slots());
      for (int i=0; i<M->n_vars(); i++)
	result->exponents[i] = exponents[i];
      MO->encode(exponents,result->partial_sums);
      insert(result);
    }
    return result;
  }

  monomial * lookup_and_insert_noncomm_encoded(const int *encoded) {
    // This is the Non-commutative case!!
    uint32 hashval = M->hash_encoded(encoded);
    int len = M->encoded_length(encoded);
    monomial *result;
    if (!search_encoded(len,encoded,hashval,result)) {
      // Need to create a new monomial
      result = new monomial;
      result->next = 0;
      result->hashval = hashval;
      result->exponents = 0;
      result->partial_sums = MA.new_array(len);
      for (int i=0; i<len; i++)
	result->partial_sums[i] = encoded[i];
      insert(result);
    }
    return result;
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
    fprintf(stderr, "total #mons = %d\n", total);
    fprintf(stderr, "max per bin = %d\n", maxpernode);
    fprintf(stderr, "min per bin = %d\n", minpernode);
    fprintf(stderr, "# zero bins = %d\n", nzeros);
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

#endif
