#include "hashtab.hpp"
#include <math.h>

int hash_allocated = 0;
int hash_highwater = 0;

static const int init_log_n_hbins = 1;
static inline compint mostsig(compint i, int n)
     // return the most significant 'n' bits of 'i'.
{
  return i >> (sizeof(i)*8 - n);
}

static const compint A2w = compint(ldexp((sqrt(5.) - 1)/2,sizeof(compint)*8));


inline void hash_alloc(int n)
{
  hash_allocated += n;
  if (hash_allocated > hash_highwater)
    hash_highwater = hash_allocated;
}

inline void hash_dealloc(int n)
{
  hash_allocated -= n;
}

static inline compint hash(compint k)
     // Compute hash value.
{
  return k*A2w;
}

template<class T>
unsigned int hashbin<T>::search(compint index)
     // Linear search: find match for 'index' in 'this'.
     // If not found, returns n_held.
{
  unsigned int i;
  for (i=0; i<n_held; i++)
    if (entries[i].index == index) break;

  return i;
}

template<class T>
hashbin<T>::remove(compint index)
     // Delete from bin: return whether index was there.
{
  unsigned int match;

  if ((match = search(index)) >= n_held)
    return 0;

  if (--n_held > 0)
    entries[match] = entries[n_held];

  entries[n_held].elem = 0;
  entries[n_held].index = 0;
  return 1;
}

template<class T>
hashtable<T>::hashtable()
{
  hashbin<T> *onespot = new hashbin<T>;
  hash_alloc(sizeof(hashbin<T>));

  int nhbins = (1 << (log_n_hashbins = init_log_n_hbins));
  bin = new hashbin<T> *[nhbins];
  hash_alloc(nhbins * sizeof(hashbin<T> *));
  for (int i=0; i<nhbins; i++)
    bin[i] = onespot;
  onespot->n_using = nhbins;
  n_held = 0;
}

template<class T>
hashtable<T>::~hashtable()
{
  int hold;
  
  int nhbins = 1 << log_n_hashbins;
  for (int b=0; b<nhbins; b += hold) 
    {
      hold = bin[b]->n_using;
      delete bin[b];
      hash_dealloc(sizeof(hashbin<T>));
    }

  delete [] bin;
  hash_dealloc(nhbins * sizeof(hashbin<T> *));
}

template<class T>
void hashtable<T>::redistribute(compint i)
     // Takes bin 'i' and reenters all its contents.
{
  bin[i]->n_held = 0;
  for (int tmp = 0; tmp < hashbin_size; tmp++)
    {
      compint hh = hash((compint)(bin[i]->entries[tmp].index));
      compint ii = mostsig(hh, log_n_hashbins);
      bin[ii]->entries[bin[ii]->n_held++] =
	bin[i]->entries[tmp];
    }
}

/*----------------------------------------------------------------
 * insert - put *ptr into t for index i, expanding as necessary
 *---------------------------------------------------------------*/
template<class T>
hashtable<T>::insert(T &elem, compint index)
{
  hashbin<T> *b;

  compint h = hash(index);
  compint i = mostsig(h, log_n_hashbins);
  
  if (bin[i]->search(index) < bin[i]->n_held)
    return 0;

  // first, hope we can find or make space in the table.
  for (compint flipper = 1 << log_n_hashbins-1; 
      (bin[i]->n_held >= hashbin_size) && (flipper != 0);
      flipper >>= 1)
    {
      compint bot,top;

      compint j = i ^ flipper;			 

      if (bin[i] != bin[j])
	continue;

      bot =  (i & ~((flipper << 1) - 1)) | ( flipper & ~i);
      top = bot + flipper;
      b = new hashbin<T>;
      hash_alloc(sizeof(hashbin<T>));
      for (compint tmp = bot; tmp < top; tmp++)
	bin[tmp] = b;

      b->n_using = (bin[i]->n_using >>= 1);
      
      redistribute(i);
    }

  // Here, if there still isn't room, we double the table size.
  while (bin[i]->n_held >= hashbin_size)
    {
      compint nhbins = (1 << log_n_hashbins);

      hashbin<T> **tmptr = new hashbin<T> *[2*nhbins];
      hash_alloc(2*nhbins*sizeof(hashbin<T> *));
      hashbin<T> *prev = NULL;
      for(compint tmp = 0; tmp < nhbins; tmp++)
	{
	  tmptr[2*tmp] = tmptr[2*tmp+1] = bin[tmp];
	  if (bin[tmp] != prev)
	    {
	      bin[tmp]->n_using <<= 1;
	      prev = bin[tmp];
	    }
	}
      delete [] bin;
      hash_dealloc(nhbins * sizeof(hashbin<T> *));
      bin = tmptr;
      log_n_hashbins++;

      i = mostsig(h, log_n_hashbins);

      b = new hashbin<T>;
      hash_alloc(sizeof(hashbin<T>));
      bin[i^1] = b;
      bin[i^1]->n_using = (bin[i]->n_using >>= 1);

      redistribute(i);
    }

  bin[i]->entries[bin[i]->n_held].elem = elem;
  bin[i]->entries[bin[i]->n_held].index = index;  
  bin[i]->n_held++;
  n_held++;
  return 1;
}

template<class T>
hashtable<T>::remove(compint index)
     // Delete any instance of 'index', return whether it was there.
{
  compint kill,keep;

  compint h = hash(index);
  compint i = mostsig(h, log_n_hashbins);
  
  if (!bin[i]->remove(index)) return 0;

  for (compint flipper = bin[i]->n_using;
      flipper < ((unsigned) 1 << log_n_hashbins);
      flipper <<= 1)
    {
      compint j = i ^ flipper;
      if (bin[i]->n_using != bin[j]->n_using) break;
      if (bin[i]->n_held == 0)
	{
	  kill = i;
	  keep = j;
	}
      else if (bin[j]->n_held == 0)
	{
	  kill = j;
	  keep = i;
	}
      else break;
      
      delete bin[kill];
      hash_dealloc(sizeof(hashbin<T>));

      compint bot =  (keep & ~((flipper << 1) - 1)) | (flipper & ~keep);
      compint top = bot + flipper;
      for(compint tmp = bot; tmp<top; tmp++)
	bin[tmp] = bin[keep];

      bin[keep]->n_using <<= 1;
    }

  n_held--;
  return 1;
}

template<class T>
hashtable<T>::search(T &elem, compint index)
     // Search: return 1, and set 'elem' if found, return 0 otherwise.
{
  unsigned int     n;

  compint h = hash(index);
  compint i = mostsig(h, log_n_hashbins);

  if ((n = bin[i]->search(index)) >= bin[i]->n_held)
    return 0;

  elem = bin[i]->entries[n].elem;
  return 1;
}

template<class T>
void hashtable<T>::debug_display(ostream &o)
     // Debug display routine.
{
  hashbin<T> *pr = NULL;
  compint nhbins = (1 << log_n_hashbins);
  for (compint i=0; i<nhbins; i++)
    {
      o << i << ":" << bin[i]->n_using << " " << bin[i]->n_held;
      if (bin[i] != pr)
	{
	  pr = bin[i];
	  for (unsigned int j=0; j<(bin[i]->n_held); j++)
	    o << '\t' << bin[i]->entries[j].index
	      << " " << bin[i]->entries[j].elem;
	}
      else
	o << "\t--^";
      o << endl;
    }
  o << endl;
}

#if 0
template<class T>
void hashtable<T>::display(ostream &o)
     // Debug display routine.
{
  for (cursor_hashtable<T> p(*this); p.valid(); p++)
    o << p.index() << " " << p.elem() << endl;
}
#endif

template class HASHTABLE(int);
#include "handles.hpp"
template class HASHTABLE(handle *);
