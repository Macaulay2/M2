// Copyright 1999  Michael E. Stillman
#ifndef _Espairs_hpp_
#define _Espairs_hpp_

#include "gb_comp.hpp"
#include "freemod.hpp"

#define EVector vec
#define EMatrix Matrix
#define EFreeModule FreeModule
#define EGroebnerComputation gb_comp
#define EVectorHeap vecHeap
#define EPolynomialRing PolynomialRing
#define EMonoid Monoid
// The following two are actually pointers to arrays of integers
#define monomial int
#define exponent_vector int

struct egb_elem;
struct ering_elem;

struct es_pair
{
  es_pair *next;
  int degree;
  monomial *lcm;  // exponent vector??
  int type; // SP_SYZ, SP_RING, SP_SKEW, SP_GEN
  union {
    struct {  // SP_SYZ
      egb_elem *i;
      egb_elem *j;
    } syz;
    struct {  // SP_RING
      egb_elem *i;
      ering_elem *j;
    } ringsyz;
    struct {  // SP_SKEW
      egb_elem *i;
      int v;
    } skewsyz;
    struct { // SP_GEN
      EVector f;
      EVector fsyz;
    } gen;
  } s;

  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#if 0
struct es_pair
{
  es_pair *next;
  int degree;
  monomial *lcm;  // exponent vector??

  EVector gsyz; // the pair
  EVector rsyz; // the ring part of the pair.
  EVector fsyz;	// the part from the original generators
  EVector f;	// if f != 0, then the spolynomial has already been constructed.

  egb_elem *i;	  // the first (main) part of the s-pair.
  int type; // SP_SYZ, SP_RING, SP_SKEW, SP_GEN
  union {
    egb_elem *syz; // SP_SYZ
    ering_elem *ringsyz; // SP_RING
    int v;		 // SP_SKEW, SP_GEN(?)
  } s;
#if 0    
  union {
    struct {  // SP_SYZ
      egb_elem *j;
    } syz;
    struct {  // SP_RING
      ering_elem *j;
    } ringsyz;
    struct {  // SP_SKEW
      int v;
    } skewsyz;
    struct { // SP_GEN
    } gen;
  } s;
#endif
  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};
#endif

class ESPairSet
{
  friend class EGB1;
  es_pair *heap;
  int nelems;
public:
  ESPairSet();

  ~ESPairSet() {}  
    // It is the responsibility of the creator to remove all pairs.

  void insert(es_pair *p);  
    // Insert a list of pairs.

  int next_degree(int &nextdeg) const;	
    // Returns number to be done in the next degree, sets nextdeg.

  int get_next_degree(int &deg, es_pair *&elems);
    // Return in 'elems' an unsorted list of elements in the lowest degree.
    // This lowest degree is set in 'lodeg', and the number of pairs in the
    // list is returned.

  int n_elems_left() const { return nelems; } // The number currently contained in this set

  void stats();			// Displays some statistics about this set.
};

class ESPairOperations
{
  es_pair *merge(es_pair *list1, es_pair *list2) const;
public:
  virtual int compare(es_pair *s1, es_pair *s2) const;
  virtual void remove_pair(es_pair *p) const;
  void sort(es_pair * & pairs) const;
  void remove_pair_list(es_pair * & pairs) const;
};
#endif
