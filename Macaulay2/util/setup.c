#include <stdio.h>

/* for Windows NT and Windows 95 */

#include <direct.h>
#include <string.h>
#include <stdlib.h>

main(int argc, char **argv) {
	char dirWithBackslashes[1000];
	char dir[1000];
	int i;
	FILE *M2bat = fopen("bin/M2.bat","w");
	FILE *M2 = fopen("bin/M2","w");
	FILE *startup = fopen("bin/startup.m2","w");
	if (M2bat == NULL) { perror("can't open bin/M2.bat"); exit(1); }
	if (M2 == NULL) { perror("can't open bin/M2"); exit(1); }
	if (startup == NULL) { perror("can't open bin/startup.m2"); exit(1); }
	if (NULL == _getcwd(dirWithBackslashes, sizeof dirWithBackslashes))
	  { perror("can't get current working directory"); exit(1); }
	strcpy(dir,dirWithBackslashes);
	for (i=0; i<sizeof dir; i++) if (dir[i] == '\\') dir[i] = '/';
	fprintf(M2bat,
		"\"%s\\bin\\Macaulay2\""
		/* " -tty" */
		" -ephase=1"
		" \"%s/m2/setup.m2\""
		" -ephase=0"
		" -erunStartFunctions()"
		" %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9"
		"\n",
		dirWithBackslashes,dir);
	fprintf(M2,
		"#! /bin/sh\n"
		"'%s/bin/Macaulay2'"
		/* " -tty" */
		" '%s/bin/startup.m2'"
		" \"$@\""
		"\n",
		dir,dir);
	fprintf(startup,
		"phase=1\n"
		"path={\"%s/m2\"}\n"
		"load \"%s/m2/setup.m2\"\n"
		"path={\".\"}\n"
		"phase=0\n"
		"runStartFunctions()\n",
		dir,dir);
	return 0;
}
