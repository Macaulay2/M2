#include "monoms.h"
#include "MonomialSet.hpp"
#include <cstdio>

#define ORD(m) ((m)[-1])
#define UNINTERNED(m) (m)

/////////////////
// MemoryBlock //
/////////////////

MemoryBlock::MemoryBlock()
  : first_slab(0),
    last_slab(0),
    next_free(0)
{
  first_slab = new_slab();
  last_slab = first_slab;
  next_free = last_slab->block;
}

MemoryBlock::~MemoryBlock()
{
  // Destroy the slabs one by one
  while (first_slab != 0)
    {
      slab *tmp = first_slab;
      first_slab = first_slab->next;
      deleteitem(tmp);
    }
}

MemoryBlock::slab *MemoryBlock::new_slab()
{
  slab *result = new slab;
  result->next = 0;
  return result;
}

int * MemoryBlock::reserve(int len)
{
  if (next_free + len > last_slab->block + NSLAB)
    {
      last_slab->next = new slab;
      last_slab = last_slab->next;
      next_free = last_slab->block;
    }
  return next_free;
}

void MemoryBlock::intern(int len)
{
  next_free += len;
}

int MemoryBlock::n_slabs() const
{
  int result = 0;
  for (slab *p = first_slab; p != 0; p = p->next) result++;
  return result;
}

/////////////////
// MonomialSet //
/////////////////

MonomialSet::MonomialSet()
{
  hashtab_len = 12043;
  hashtab = newarray(hash_node *,hashtab_len);
  for (int i=0; i<hashtab_len; i++)
    hashtab[i] = 0;
}

MonomialSet *MonomialSet::make()
{
  return new MonomialSet;
}

uninterned_monomial MonomialSet::reserve(int len)
{
  last_alloc = B.reserve(len);
  return last_alloc;
}

monomial MonomialSet::intern_monomial(uninterned_monomial m)
{
  // Copy the monomial to a the latest slab.
  monomial result;
  int len = 2*m[0]+2;
  if (m == last_alloc)
    {
      last_alloc = 0;
      result = m;
    }
  else
    {
      int *next_free = B.reserve(len);
      *next_free++ = 0;
      result = next_free;
      for (int i=len-1; i>0; i--)
	*next_free++ = *m++;
    }
  B.intern(len);
  return result;
}

MonomialSet::hash_node *MonomialSet::get_next_hash_node()
{
  return new hash_node;
}

unsigned long MonomialSet::hash(uninterned_monomial m)
{
  int len = 2*(*m++);
  unsigned long result = len;
  int a = 1013746461;
  for (int i=1; i<=len;i++)
    {
      result += a*result + (*m++);
    }
  
  return result;
#if 0
  int len = 2*(*m++);
  unsigned long result = len;
  int a = 1013746461;
  for (int i=1; i<=len;i++)
    {
      result += a*(*m++);
      a += 13;
    }
  
  return result;
#endif
}

bool MonomialSet::find_or_insert(uninterned_monomial m,
				 monomial &result)
  // return true if the monomial already exists in the table.
  // otherwise, result is set to the new monomial.
  // The monomial is copied into the 'slab' space locally.
{
  unsigned long hashval = hash(m);
  int x = hashval % hashtab_len;
  hash_node *head = reinterpret_cast<hash_node *>(hashtab + x);
  for ( ; head->next != 0; head=head->next)
    {
      if (head->next->key > hashval)
	break;
      else if (head->next->key == hashval 
	       && monomial_equal(m,
				 UNINTERNED(head->next->monom)))
	{
	  /* In this case, the monomial is already present */
	  result = head->next->monom;
	  return true;
	}
    }
  hash_node *p = get_next_hash_node();
  p->next = head->next;
  p->key = hashval;
  p->monom = intern_monomial(m);
  head->next = p;
  result = p->monom;
  return false;
}

void MonomialSet::dump()
  // displays on stderr some info about the hash table
  // and number of monomials
{
  // Then the tally of how many times each number occurs
  int tally[100];
  int nmonoms = 0;
  for (int i=0; i<100; i++) tally[i] = 0;
  for (int i=0; i<hashtab_len; i++)
    {
      int n = 0;
      for (hash_node *p = hashtab[i]; p != 0; p = p->next) n++;
      nmonoms += n;
      if (n<100) tally[n]++;
      else tally[99]++;
    }

  int nslabs = B.n_slabs();

  // Now display the data
  fprintf(stderr, "number of monomials in table = %d\n", nmonoms);
  fprintf(stderr, "number of monomial slabs     = %d\n", nslabs);
  fprintf(stderr, "number of hash bins having x monomials is y\n");
  for (int i=0; i<100; i++)
    if (tally[i] > 0)
      fprintf(stderr, "    having %d monomials = %d\n", i, tally[i]);
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
*/
