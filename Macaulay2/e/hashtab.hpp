// (c) 1994  Michael E. Stillman

#include "style.hpp"
#define TRUE 1
#define FALSE 0
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
       char occupied;
       } *bin;
  unsigned int size;		// always a power of 2
  void enlarge() {
       unsigned int newsize = 2 * size;
       ENTRY *newbin = new ENTRY [newsize];
       for (unsigned int i = 0; i<newsize; i++) newbin[i].occupied = FALSE;
       for (unsigned int j=0; j<size; j++) {
	    if (!bin[j].occupied) continue;
	    compint key = bin[j].key;
	    unsigned int i;
	    for (i = key & (newsize - 1); newbin[i].occupied; i>0 ? i-- : i=newsize-1) ;
	    newbin[i].elem = bin[j].elem;
	    newbin[i].key = key;
     	    newbin[i].occupied = TRUE;
	    }
       delete [] bin;
       bin = newbin;
       size = newsize;
       }
  friend cursor_hashtable<T>;
public:
  int highwater() { return maxused; }
  int current() { return used; }
  hashtable(T trivial_elem0) {
       trivial_elem = trivial_elem0;
       maxused = used = 0;
       size = 1;
       bin = new ENTRY [size];
       for (unsigned int i = 0; i<size; i++) bin[i].occupied = FALSE;
       }
  ~hashtable() { delete [] bin; }
  void insert(T &elem, compint key) {
       used ++;
       if (used > maxused) maxused = used;
       if (used * 5 > size * 4) enlarge();
       unsigned int i;
       for (i = key & (size - 1); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 cerr << "duplicate key encountered - internal error" << endl;
		 exit(1);
		 }
	    }
       bin[i].elem = elem;
       bin[i].key = key;
       bin[i].occupied = TRUE;
       }
  int search(T &elem, compint key) {
       unsigned int i;
       for (i = key & (size - 1); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 elem = bin[i].elem;
		 return TRUE;
		 }
	    }
       return FALSE;
       }
  void remove(T &elem, compint key) {
       for (unsigned int i = key & (size - 1); bin[i].occupied; i>0 ? i-- : i=size-1) {
	    if (bin[i].key == key) {
		 used --;
		 bin[i].occupied = FALSE;
		 elem = bin[i].elem;
		 bin[i].elem = trivial_elem;
		 bin[i].key = 0;
       		 for (unsigned int j = i > 0 ? i-1 : size-1; bin[j].occupied; j>0 ? j-- : j=size-1) {
		      for (unsigned int k = bin[j].key & (size - 1); k!=j ; k>0 ? k-- : k=size-1) {
			   if (!bin[k].occupied) {
				bin[k] = bin[j];
				bin[j].occupied = FALSE;
				bin[j].elem = trivial_elem;
				bin[j].key = 0;
				}
			   }
		      }
		 return;
		 }
	    }
       cerr << "key not found in hash table -- internal error" << endl;
       exit(1);
       }
  };

template<class T>
class cursor_hashtable
{
  hashtable<T> *h;
  unsigned int i;
public:
  int valid() { return i < h->size; }
  cursor_hashtable(hashtable<T> &a) {
       h = &a;
       i = 0;
       while (i < h->size && !h->bin[i].occupied) i++;
       }
  // cursor_hashtable(const cursor_hashtable &c) ;
  ~cursor_hashtable() {}
  cursor_hashtable &operator++() {
       if (!valid()) {
	    cerr << "invalid hashtable cursor used - internal error" << endl;
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
