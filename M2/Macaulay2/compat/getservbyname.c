#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
void *getservbyname() { errno = ENOSYS; return (void *)0; }
