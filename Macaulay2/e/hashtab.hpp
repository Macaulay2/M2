// (c) 1994  Michael E. Stillman

#include "style.hpp"
#include <stdio.h>

typedef unsigned long compint;

template<class T> class hashtable;
template<class T> class cursor_hashtable;

template<class T>
class hashtable
{
  unsigned int used;
  unsigned int maxused;
  T trivial_elem;
  struct ENTRY {
       T elem;
       compint key;
       bool occupied;
       } *bin;
  inline compint hashup(compint i, unsigned int size) {
       return (i ^ (i >> 8)) & (size - 1);
       }
  unsigned int size;		// always a power of 2
  void enlarge() {
       unsigned int newsize = 2 * size;
       ENTRY *newbin = new ENTRY [newsize];
       for (unsigned int i = 0; i<newsize; i++) newbin[i].occupied = false;
       for (unsigned int j=0; j<size; j++) {
	    if (!bin[j].occupied) continue;
	    compint key = bin[j].key;
	    unsigned int i;
	    for (i = hashup(key,newsize); newbin[i].occupied; i>0 ? i-- : i=newsize-1) ;
	    newbin[i].elem = bin[j].elem;
	    newbin[i].key = key;
     	    newbin[i].occupied = true;
	    }
       delete [] bin;
       bin = newbin;
       size = newsize;
       }
  friend class cursor_hashtable<T>;
public:
  int highwater() { return maxused; }
  int current() { return used; }
  hashtable(T trivial_elem0) {
       trivial_elem = trivial_elem0;
       maxused = used = 0;
       size = 1;
       bin = new ENTRY [size];
       for (unsigned int i = 0; i<size; i++) bin[i].occupied = false;
       }
  ~hashtable() { delete [] bin; }
  void insert(T &elem, compint key) {
       used ++;
       if (used > maxused) maxused = used;
       if (used * 4 > size * 3) enlarge();
       unsigned int i;
       for (i = hashup(key,size); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 fprintf(stderr,"duplicate key encountered - internal error\n");
		 exit(1);
		 }
	    }
       bin[i].elem = elem;
       bin[i].key = key;
       bin[i].occupied = true;
       }
  bool search(T &elem, compint key) {
       unsigned int i;
       for (i = hashup(key,size); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 elem = bin[i].elem;
		 return true;
		 }
	    }
       return false;
       }
  void remove(T &elem, compint key) {
       for (unsigned int i = hashup(key,size); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 used --;
		 bin[i].occupied = false;
		 elem = bin[i].elem;
		 bin[i].elem = trivial_elem;
		 bin[i].key = 0;
       		 for (unsigned int j = i > 0 ? i-1 : size-1; bin[j].occupied; j>0 ? j-- : j=size-1) {
		      unsigned int k = hashup(bin[j].key,size);
		      if ( j<i && (k>=i || k<j) ||
			   j>i && (k>=i && k<j)) 
		      {
			   struct ENTRY tmp = bin[i]; // it's empty!
			   bin[i] = bin[j];
			   bin[j] = tmp;
			   i = j;
			   }
		      }
		 return;
		 }
	    }
       fprintf(stderr,"key not found in hash table -- internal error\n");
       exit(1);
       }
  void showshape() {
    fprintf(stdout, "used = %d\n", used);
    fprintf(stdout, "size = %d\n", size);
    unsigned int i;
    for (i=0; i<size; i++) {
	 unsigned int n = 0;
	 for(; i<size && n<80; i++, n++) putchar(bin[i].occupied ? '*' : '.');
	 putchar('\n');
      }
    for (i=0; i<size; i++) if (bin[i].occupied) {
      fprintf(stdout," i=%08x", i);
      fprintf(stdout," key=%08lx", bin[i].key);
      fprintf(stdout," hashup=%08lx", hashup(bin[i].key,size));
      fprintf(stdout," distance=%d", (int)((hashup(bin[i].key,size)-i)&(size-1)));
      putchar('\n');
      }
    }
  };

template<class T>
class cursor_hashtable
{
  hashtable<T> *h;
  unsigned int i;
public:
  bool valid() { return i < h->size; }
  cursor_hashtable(hashtable<T> &a) {
       h = &a;
       i = 0;
       while (i < h->size && !h->bin[i].occupied) i++;
       }
  // cursor_hashtable(const cursor_hashtable &c) ;
  ~cursor_hashtable() {}
  cursor_hashtable &operator++() {
       if (!valid()) {
	    ERROR("invalid hashtable cursor used - internal error");
	    }
       i++;
       while (i < h->size && !h->bin[i].occupied) i++;
       return *this;
       }
  compint  key() {
       return h->bin[i].key;
       }
  T &elem () {
       return h->bin[i].elem;
       }
  };
