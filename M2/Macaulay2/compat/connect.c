#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int connect() { errno = ENOSYS; return -1; }
