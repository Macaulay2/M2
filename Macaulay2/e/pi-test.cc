#include "pi.h"
#include <stdlib.h>
#include <assert.h>

int (*g)(int64_t *, int64_t *, int) = pui<int64_t,int,7,UNSIGNED>::cmp_lex;

template class pui<uint32_t,int,7,UNSIGNED>;
template class pui<uint32_t,int,32,UNSIGNED>;
template class pui<uint32_t,unsigned,32,UNSIGNED>;
template class puiv<uint32_t,int,7,10,UNSIGNED>;

template <typename T, typename U, int bits_per_fld, field_type type, int numflds> struct tester {
     typedef pui<T,U,bits_per_fld,type> A;
     static void run() {
	  T dest[numflds+1];
	  U src[numflds+1], src2[numflds+1];
	  try {
	       for (int i = 0; i <= numflds; i++) src[i] = A::random_U();
	       src2[numflds] = src[numflds];
	       A::pack(dest,src,numflds);
	       A::unpack(src2,dest,numflds);
	       for (int i = 0; i < numflds; i++) assert(src[i] == src2[i]);
	       assert(src2[numflds] == src[numflds]);
	  }
	  catch (exc::overflow_error e) {
	       printf("pi-test: error: %s\n",e.what());
	       exit(1);
	  }
     }
};

int main () {
     tester<uint32_t,int32_t,7,SIGNED,8>::run();
     tester<uint32_t,int32_t,32,SIGNED,8>::run();
     tester<uint32_t,int32_t,7,UNSIGNED,8>::run();
     tester<uint32_t,int32_t,32,UNSIGNED,8>::run();
     tester<uint64_t,int32_t,7,SIGNED,8>::run();
     tester<uint64_t,int32_t,32,SIGNED,8>::run();
     tester<uint64_t,int32_t,7,UNSIGNED,8>::run();
     tester<uint64_t,int32_t,32,UNSIGNED,8>::run();
     tester<uint64_t,int64_t,64,SIGNED,8>::run();
     printf("pi-test: tests completed\n");
     return 0;
}

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make pi-test-demangled.s pi-test-run"
// End:
