#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv) {
     dup2(2,1);
     while (*argv) {
	  char *p;
	  putchar('\'');
	  for (p=*argv++; *p; p++) {
	       if (*p == '\'') putchar('\\');
	       putchar(*p);
	       }
	  putchar('\'');
	  putchar(' ');
	  }
     printf("\n");
     return 0;
     }
