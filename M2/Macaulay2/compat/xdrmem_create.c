#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
void *xdrmem_create() { errno = ENOSYS; return (void *)0; }
