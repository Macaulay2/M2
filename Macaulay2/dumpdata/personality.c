#include <stdio.h>
#if 0
# include <linux/personality.h>
# undef personality
#endif
#define ADDR_NO_RANDOMIZE 0x0040000
extern long personality(unsigned long persona);

int main (int argc, char **argv) {
     if (argc == 1) {
	  printf("0x%x\n",personality(-1));
	  return 0;
     }
     personality(ADDR_NO_RANDOMIZE | personality(-1));
     return execvp(argv[1],argv+1);
}
