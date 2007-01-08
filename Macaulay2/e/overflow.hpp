// methods for detecting arithmetic overflows

#include "exceptions.hpp"

namespace safe {
     static inline int over_1(int x) { return x < 0 ; }
     static inline int over_2(int x) { return (0x80008000 & x) != 0 ; }
     static inline int over_4(int x) { return (0x80808080 & x) != 0 ; }
     static inline int add(int x, int y, const char *msg) {
	  int z = x+y;
	  if ((x < 0) != (z < 0) && (x < 0) == (y < 0)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int add(int x, int y) { 
	  return add(x,y,"overflow: int + int"); 
     }
     static inline int pos_add(int x, int y, const char *msg) {
	  assert(! over_1(x) && ! over_1(y));
	  int z = x+y;
	  if (over_1(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int pos_add(int x, int y) { 
	  return pos_add(x,y,"overflow: pos int + pos int");
     }
     static inline int pos_add_2(int x, int y, const char *msg) {
	  assert(! over_2(x) && ! over_2(y));
	  int z = x+y;
	  if (over_2(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int pos_add_2(int x, int y) { 
	  return pos_add_2(x,y,"overflow: pos int + pos int, packed 2");
     }
     static inline int pos_add_4(int x, int y, const char *msg) {
	  assert(! over_4(x) && ! over_4(y));
	  int z = x+y;
	  if (over_4(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int pos_add_4(int x, int y) { 
	  return pos_add_4(x,y,"overflow: pos int + pos int, packed 4");
     }
};
