#include "map.h"
extern int isCheckable(map m);
extern void checkmaps(int nmaps, struct MAP m[nmaps]);
extern char mapfmt[];
extern int isStatic(map m);
extern int isStack(map m);
extern int isDumpable(map m);
extern void sprintmap(char *s, map m);
extern void fdprintmap(int fd, map m);
