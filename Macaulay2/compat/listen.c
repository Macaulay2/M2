#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int listen() { errno = ENOSYS; return -1; }
