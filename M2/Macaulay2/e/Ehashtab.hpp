// Copyright 1998 by Michael Stillman
#ifndef _Ehashtable_hpp_
#define _Ehashtable_hpp_

#include "text_io.hpp"

// This class collects various types of 'object_element's
// Basically, each type that is desired to be kept 'unique'
// is placed in here, using the 'intern' function.
//
// types allowed:
// EFreeModule, object_EVector
// EMonoid, EPolynomialRing are NOT placed here: we need to be able to clone
// these entities (they contain monomial tables that we might want to keep separate).
const int OBJECT_HASH_SIZE = 1024*2;

class EHashTable
{
  struct NODE {
    NODE *next;
    uint32 hashval;
    object_element * element;
  };
  int len;
  NODE **bins;
  NODE *head;
  
  bool isequal(object_element *a, object_element *b) {
    type_identifier atype = a->type_id();
    type_identifier btype = b->type_id();
    if (atype == TY_EFreeModule)
      if (btype == TY_EFreeModule)
        return a->cast_to_EFreeModule()->contentIsEqual(b->cast_to_EFreeModule());
      else
        return false;
    else
      if (btype == TY_EFreeModule)
        return false;
      else
        return a->cast_to_EVector()->isEqual(b->cast_to_EVector());
  }
public:
  EHashTable() {
    len = OBJECT_HASH_SIZE;
    bins = new NODE *[len];
    for (int i=0; i<len; i++) bins[i] = 0;
    head = new NODE;
    // These fields are not used (except for 'next'), but set them so they aren't junk.
    head->next = 0;
    head->element = 0;
    head->hashval = 0;
  }
  
  ~EHashTable() {
    for (int i=0; i<len; i++) {
      while (bins[i] != 0) {
        NODE *tmp = bins[i];
        bins[i] = tmp->next;
        tmp->next = 0;
        tmp->element = 0;
        delete tmp;
      }
    }
    delete head;
    delete [] bins;
  }
  
  void remove(object_element *a) {
    // It is assumed that 'a' is in here!
    uint32 hashval = a->hash();
    int h = hashval % len;
    head->next = bins[h];
    for (NODE *p = head; p->next != 0; p=p->next)
      if (p->next->element == a)
        {
          NODE *tmp = p->next;
          p->next = tmp->next;
          tmp->element = 0;
          delete tmp;
          bins[h] = head->next;
          return;
        }
    // If we arrive here, that means that the element is not in the table.
    gError << "internal error: object hashtable remove called for non-existent object";
    return;
  }
  
  // Returns true if the element did not previously exist.
  bool insert(object_element *&a) {  // If the element 'a' is in the table, this calls delete on a.
    uint32 hashval = a->hash();
    int h = hashval % len;
    for (NODE *p = bins[h]; p != 0; p=p->next)
      if (p->hashval == hashval && isequal(p->element,a))
        {
          delete a;
          a = p->element;
          return false;
        }
    NODE *newnode = new NODE;
    newnode->next = bins[h];
    newnode->hashval = hashval;
    newnode->element = a;
    bins[h] = newnode;
    return true;
  }
  
  bool insertEFreeModule(EFreeModule *&a) {
    object_element *b = a;
    if (!insert(b)) {
      a = (EFreeModule *) b;
      return false;
    } else
      return true;
  }
  bool insertEVector(object_EVector *&a) {
    object_element *b = a;
    if (!insert(b)) {
      a = (object_EVector *) b;
      return false;
    } else
      return true;
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
	for (NODE *v = bins[i]; v!=0; v=v->next) n++;
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
    fprintf(stderr, "total #elem = %d\n", total);
    fprintf(stderr, "max per bin = %d\n", maxpernode);
    fprintf(stderr, "min per bin = %d\n", minpernode);
    fprintf(stderr, "# zero bins = %d\n", nzeros);
    buffer o;
    for (i=0; i<len; i++)
      {
        o << nelems[i] << " ";
	if ((i % 50) == 0) o << newline;
      }
    o << newline;
    emit(o.str());
  }
};

extern EHashTable EUniqueObjects;
#endif