#include "integertypes.h"
#include <stdio.h>
template <typename T> T ovmask(int bits) { if (bits==0) return 0; T n=1, r=0; n<<=bits  ; while(n&~r) {r|=n; n<<=bits;} return r; }
template <typename T> T himask(int bits) { if (bits==0) return 0; T n=1, r=0; n<<=bits-1; while(n&~r) {r|=n; n<<=bits;} return r; }

int main() {

     printf("const static uint32_t ovmask32[] = {\n");
     for (int bits=0; bits<=32; bits++) printf("  0x%08x,\n",ovmask<uint32_t>(bits));
     printf("};\n");
     printf("const static uint32_t himask32[] = {\n");
     for (int bits=0; bits<=32; bits++) printf("  0x%08x,\n",himask<uint32_t>(bits));
     printf("};\n");

     printf("const static uint64_t ovmask64[] = {\n");
     for (int bits=0; bits<=64; bits++) printf("  0x%016LxLL,\n",ovmask<uint64_t>(bits));
     printf("};\n");
     printf("const static uint64_t himask64[] = {\n");
     for (int bits=0; bits<=64; bits++) printf("  0x%016LxLL,\n",himask<uint64_t>(bits));
     printf("};\n");

     return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-masks.h"
// End:

