#include "f4/f4-mem.hpp"
#include <cstdio>

const unsigned int doublesize[DOUBLESIZE] = {
    12,        24,        48,        96,         192,        384,
    768,       1536,      3072,      6144,       12288,      24576,
    49152,     98304,     196608,    393216,     786432,     1572864,
    3145728,   6291456,   12582912,  25165824,   50331648,   100663296,
    201326592, 402653184, 805306368, 1610612736, 3221225472U};

void F4Vec::show()
{
  fprintf(stderr, "memory usage for %s.  nreallocs = %d\n", name, nreallocs);
  fprintf(stderr,
          "index\t\tsize\t\tnalloc\t\tndealloc\t\thighwater\tcurrent\n");
  for (int i = 0; i < DOUBLESIZE; i++)
    if (nallocs[i] > 0)
      {
        fprintf(stderr,
                "%4u\t%12u\t%12u\t%12u\t\t%12u\t%12u\n",
                i,
                doublesize[i],
                nallocs[i],
                ndeallocs[i],
                highwater[i],
                current[i]);
      }
}

F4Mem::F4Mem()
    : monom_alloc(0),
      monom_total(0),
      monom_freed(0),
      monom_dealloc(0),
      monom_highwater(0),
      monom_current(0),
      components("comps"),
      coefficients("coeffs")
{
}

void F4Mem::show()
{
  fprintf(stderr,
          "class\tnalloc\tndealloc\thighwater\ttotal\tfreed\tcurrent\n");
  fprintf(stderr,
          "monom\t%ld\t%ld\t\t%ld\t\t%ld\t%ld\t%ld\n",
          monom_alloc,
          monom_dealloc,
          monom_highwater,
          monom_total,
          monom_freed,
          monom_current);
  components.show();
  coefficients.show();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
