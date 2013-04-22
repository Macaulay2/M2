#include <gc/gc.h>
main()
{
  unsigned int bit=0x40000000, sum=0;
  char *x[1000];
  int i=0;

  GC_INIT();
  
  while (bit > 4096) 
  {
    x[i] = GC_MALLOC(bit);
    if (x[i])
    sum += bit;
    bit >>= 1;
    i++;
  }
  printf("%08x bytes (%.1fMb)\n", sum, sum/1024.0/1024.0);
  return 0;
}

