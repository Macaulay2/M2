#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int inet_addr() { errno = ENOSYS; return -1; }
