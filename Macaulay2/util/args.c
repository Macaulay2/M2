#include <stdlib.h>
#include <stdio.h>

main(int argc, char **argv) {
	int i;
	printf("args");
	for (i=0; i<argc; i++) {
		printf(" '%s'",argv[i]);
	}
	printf("\n");
}
