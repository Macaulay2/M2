#include <stdio.h>

/* for Windows NT and Windows 95 */

#include <direct.h>
#include <string.h>
#include <stdlib.h>

main(int argc, char **argv) {
	char buf[1000];
	char dir[1000];
	int i;
	char *M2fn = "bin/M2.bat";
	FILE *M2bat = fopen(M2fn,"w");
	if (M2bat == NULL) {
		perror("can't open M2.bat");
		exit(1);
	}
	if (NULL == _getcwd(buf, sizeof buf)) {
		perror("can't get current working directory");
		exit(1);
	}
	strcpy(dir,buf);
	for (i=0; i<sizeof dir; i++) {
		if (dir[i] == '\\') dir[i] = '/';
	}
	fprintf(M2bat,"%s\\bin\\Macaulay2 -ephase=1 %s/m2/setup.m2 -ephase=0 -erunStartFunctions() %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9\n",buf,dir);
	return 0;
}
