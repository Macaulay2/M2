#include "pi.h"
#include <stdlib.h>
#include <assert.h>

#ifdef DEBUG
void trap () {}
#endif

template <typename T, typename U, int bits_per_fld, field_type type, int numflds> struct tester1 {
     typedef pui<T,U,bits_per_fld,type> A;
     static U random2() {
	  if (bits_per_fld == 1) return 0;
	  U u = A::random_U();
	  if (u < 0) {
	       U v = (-u) & A::mask_one_field();
	       if (u == v) u += 2;
	  }
	  u >>= 1;
	  return u;
     }
     static void run() __attribute__ ((noinline)) {
	  T dest[numflds+1], dest2[numflds+1], dest3[numflds+1];
	  U src[numflds+1], src2[numflds+1], src3[numflds+1];
	  try {
	       for (int i = 0; i <= numflds; i++) src[i] = A::random_U();
	       src2[numflds] = src[numflds];
	       A::pack(dest,src,numflds);
	       A::unpack(src2,dest,numflds);
	       for (int i = 0; i < numflds; i++) assert(src[i] == src2[i]);
	       assert(src2[numflds] == src[numflds]); // make sure there is no scribbling

	       for (int i = 0; i <= numflds; i++) src2[i] = random2(), src3[i] = random2();
	       A::pack(dest2,src2,numflds);
	       A::pack(dest3,src3,numflds);
	       A::add(dest,dest2,dest3,A::numbins(numflds));
	       A::unpack(src,dest,numflds);
	       for (int i = 0; i < numflds; i++) assert(src[i] == src2[i] + src3[i]);

	       for (int i = 0; i <= numflds; i++) src3[i] = random2(), src2[i] = random2() + src3[i];
	       A::pack(dest2,src2,numflds);
	       A::pack(dest3,src3,numflds);
	       A::sub(dest,dest2,dest3,A::numbins(numflds));
	       A::unpack(src,dest,numflds);
	       for (int i = 0; i < numflds; i++) assert(src[i] == src2[i] - src3[i]);

	  }
	  catch (exc::overflow_error e) {
	       printf("pi-test: error: %s\n",e.what());
	       exit(1);
	  }
     }
};

template <typename T, typename U, int bits_per_fld, field_type type> struct tester2 {
     static void run() __attribute__ ((noinline)) {
	  tester1<T,U,bits_per_fld,type,1>::run();
	  tester1<T,U,bits_per_fld,type,2>::run();
	  tester1<T,U,bits_per_fld,type,7>::run();
	  tester1<T,U,bits_per_fld,type,8>::run();
	  tester1<T,U,bits_per_fld,type,11>::run();
	  tester1<T,U,bits_per_fld,type,80>::run();
     }
};

template <typename T, typename U, int bits_per_fld> struct tester3 {
     static void run() __attribute__ ((noinline)) {
	  if (~(U)0 < 0) {
	       tester2<T,U,bits_per_fld,SIGNED>::run();
	       tester2<T,U,bits_per_fld,SIGNED_REVERSED>::run();
	  }
	  else {
	       tester2<T,U,bits_per_fld,UNSIGNED>::run();
	       tester2<T,U,bits_per_fld,UNSIGNED_REVERSED>::run();
	  }
     }
};

struct tester4 {
     static void run() __attribute__ ((noinline)) {
	  tester3<uint32_t,int32_t,1>::run();
	  tester3<uint32_t,int32_t,2>::run();
	  tester3<uint32_t,int32_t,3>::run();
	  tester3<uint32_t,int32_t,7>::run();
	  tester3<uint32_t,int32_t,8>::run();
	  tester3<uint32_t,int32_t,16>::run();
	  tester3<uint32_t,int32_t,31>::run();
	  tester3<uint32_t,int32_t,32>::run();

	  tester3<uint32_t,uint32_t,1>::run();
	  tester3<uint32_t,uint32_t,2>::run();
	  tester3<uint32_t,uint32_t,3>::run();
	  tester3<uint32_t,uint32_t,7>::run();
	  tester3<uint32_t,uint32_t,8>::run();
	  tester3<uint32_t,uint32_t,16>::run();
	  tester3<uint32_t,uint32_t,31>::run();
	  tester3<uint32_t,uint32_t,32>::run();

	  tester3<uint64_t,int32_t,1>::run();
	  tester3<uint64_t,int32_t,2>::run();
	  tester3<uint64_t,int32_t,3>::run();
	  tester3<uint64_t,int32_t,7>::run();
	  tester3<uint64_t,int32_t,8>::run();
	  tester3<uint64_t,int32_t,16>::run();
	  tester3<uint64_t,int32_t,31>::run();
	  tester3<uint64_t,int32_t,32>::run();

	  tester3<uint64_t,uint32_t,1>::run();
	  tester3<uint64_t,uint32_t,2>::run();
	  tester3<uint64_t,uint32_t,3>::run();
	  tester3<uint64_t,uint32_t,7>::run();
	  tester3<uint64_t,uint32_t,8>::run();
	  tester3<uint64_t,uint32_t,16>::run();
	  tester3<uint64_t,uint32_t,31>::run();
	  tester3<uint64_t,uint32_t,32>::run();

	  tester3<uint64_t,uint64_t,63>::run();
	  tester3<uint64_t,uint64_t,64>::run();
     }
};

int main () {
     tester4::run();
     printf("pi-test: tests completed\n");
     return 0;
}

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make pi-test-run"
// End:
