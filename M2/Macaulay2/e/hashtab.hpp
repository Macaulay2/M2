// (c) 1994  Michael E. Stillman
#ifndef _hashtable_hh_
#define _hashtable_hh_

#include "style.hpp"

#define HASHTABLE(T) hashtable<T>

#undef index

template<class T> class hashtable;
template<class T> class cursor_hashtable;

typedef unsigned long int compint;

const int hashbin_size = 4;

template<class T>
class hashentry
{
  friend hashbin<T>;
  friend hashtable<T>;
  friend cursor_hashtable<T>;
  T         elem;
  compint   index;
};

template<class T>
class hashbin
{
  friend hashtable<T>;
  friend cursor_hashtable<T>;
  hashentry<T> entries[hashbin_size];  
  unsigned int n_held;
  unsigned int n_using;

  hashbin() : n_held(0), n_using(0) {}
  ~hashbin() {}

  unsigned int search(compint index);
  remove(compint index);
};

template<class T>
class hashtable
{
  friend cursor_hashtable<T>;

  int          log_n_hashbins;
  int          n_held;
  hashbin<T> **bin;

  void redistribute(compint i);
public:
  hashtable();
  ~hashtable();

  insert(T &elem, compint index);
  remove(compint index);
  search(T &elem, compint index);
  
  void debug_display(ostream &o);
};

template<class T>
class cursor_hashtable
{
  compint hashtable_i;
  compint hashbin_i;
  int is_valid;
  hashtable<T> *h;
public:
  cursor_hashtable(hashtable<T> &a) 
    : hashtable_i(0), hashbin_i(0), h(&a)
      {
	//is_valid = (h->bin[0]->n_held > 0);
	// MES replaced by:
	is_valid = (h->n_held > 0);
	if (is_valid) {		// DRG's fix
	     while (h->bin[hashtable_i]->n_held == 0) hashtable_i++;
	}
      }
  cursor_hashtable(const cursor_hashtable &c);
  ~cursor_hashtable() {}

  int valid() {return is_valid;}
  cursor_hashtable &operator++() // Next non-zero (var,exponent) pair.
    {
      if (++hashbin_i >= h->bin[hashtable_i]->n_held)
	{
	  unsigned int limit = (1 << h->log_n_hashbins); // DRG
	  hashbin_i = 0;
	  hashtable_i += h->bin[hashtable_i]->n_using;	    
	  while (		// DRG
		 hashtable_i < limit &&
		 h->bin[hashtable_i]->n_held == 0
		 ) {
	       hashtable_i++;
	  }			// DRG
	  is_valid = (hashtable_i < limit);
	}
    return *this;
    }

  compint  index() {return h->bin[hashtable_i]->entries[hashbin_i].index;}
  T       &elem () {return h->bin[hashtable_i]->entries[hashbin_i].elem;}
};

#endif
